import mdlog, os
mdlog.initLogging("server", "/tmp", stdOut=True)
log = mdlog.getLogger(__name__)
log.setLevel(10)
log.info("\n--------------------------------------------------------------------------------\n")

import os, sys
from dfly_server import DragonflyThread
from WindowEventWatcher import WindowEventWatcher
import EventLoop
from EventLoop import SubscriptionHandle
import re
import time
import traceback
from EventList import (MicrophoneEvent, ConnectedEvent, WindowListEvent,
                       ExitEvent, RestartEvent, EventsDrainedEvent)
from rules.ContextualRule import makeContextualRule
import EventList
import select
from protocol import RuleType

badWindows = {
    "Desktop",
    ".*Edge Panel.*",
    "gnome-screensaver",
    "Panel",
    "$^", # empty string
}

def filterWindows(w):
    # filter out known bad names
    for bad in badWindows:
        if re.search(bad, w.name) is not None:
            return False

    # filter out windows that don't have
    # icons set. typically these only exist
    # as an artifact of dealing with X
    if not w.hasIcon:
        return False

    return True

class TimerEntry(object):
    def __init__(self, nextExpiration, callback, seconds, priority):
        self.nextExpiration = nextExpiration
        self.callback = callback
        self.seconds = seconds
        self.priority = priority

class MainThread(object):
    def __init__(self):
        # this needs to run before any user modes are imported
        self.timers = []
        EventLoop.event_loop = self        

        self.run = True
        self.events = []
        self.eventSubscribers = {}

        self.dfly = DragonflyThread(('', 23133), self)
        self.win = WindowEventWatcher(self, filterWindows)
        
        self.subscribeEvent(RestartEvent, self.restart)
        self.subscribeEvent(ExitEvent, self.stop)
        
        mapping = { "restart mandimus" : (lambda x: self.put(RestartEvent())),
                    "completely exit mandimus" : (lambda x: self.put(ExitEvent())) }
        self.MainControlRule = makeContextualRule("MainControlRule", mapping, ruleType=RuleType.INDEPENDENT)
        self.MainControlRule.activate()
        
    def subscribeEvent(self, eventType, handler, priority=100):
        if eventType not in self.eventSubscribers:
            self.eventSubscribers[eventType] = []
        self.eventSubscribers[eventType].append((priority, handler))
        self.eventSubscribers[eventType].sort(key=lambda x: x[0])
        return SubscriptionHandle((eventType, priority, handler))

    def subscribeTimer(self, seconds, cb, priority=100,):
        entry = TimerEntry(time.time() + seconds, cb, seconds, priority)
        self.timers.append(entry)
        self.timers.sort(key=lambda x: x.priority)
        return SubscriptionHandle(entry)

    def unsubscribe(self, handleData):
        if isinstance(handleData, TimerEntry):
            self.timers.remove(handleData)
        else:
            self.eventSubscribers[handleData[0]].remove((handleData[1], handleData[2]))

    def timeout(self):
        if self.timers:
            nextTimer = min(self.timers, key=lambda x: x.nextExpiration)
            nextExpiration = nextTimer.nextExpiration
        else:
            # without a timeout, cgetrl-c doesn't work because.. python
            ONEYEAR = 365 * 24 * 60 * 60
            nextExpiration = time.time() + ONEYEAR 
        return max(nextExpiration - time.time(), 0)

    def dispatchTimers(self):
        now = time.time()
        for t in self.timers:
            if now >= t.nextExpiration:
                t.nextExpiration = now + t.seconds
                try:
                    t.callback()
                except KeyboardInterrupt:
                    raise
                except Exception:
                    exc_type, exc_value, exc_traceback = sys.exc_info()
                    log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
                    continue
                    
    def put(self, p):
        self.events += [p]

    def processEvent(self, ev):
        if type(ev) in self.eventSubscribers:
            for h in self.eventSubscribers[type(ev)]:
                try:
                    h[1](ev)
                except KeyboardInterrupt:
                    raise
                except Exception as e:
                    exc_type, exc_value, exc_traceback = sys.exc_info()
                    log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
                    continue

    def drainEvents(self):
        ranOnce = False
        try:
            while self.run:
                try:
                    ev = self.events.pop()
                except IndexError:
                    break

                self.processEvent(ev)
                ranOnce = True

            if ranOnce:
                self.processEvent(EventsDrainedEvent())
        except KeyboardInterrupt:
            self.stop()
            sys.exit()
        
    def __call__(self):
        try:
            while self.run:
                time.sleep(self.timeout())
                self.dispatchTimers()
                self.drainEvents()
        except KeyboardInterrupt:
            self.stop()
            sys.exit()

    def stop(self, ev=None):
        self.run = False
        self.dfly.cleanup()

    def restart(self, ev=None):
        log.info("Restarting mandimus")
        self.processEvent(MicrophoneEvent("server-disconnected"))
        mdlog.flush()
        self.stop()
        sys.stdout.flush()
        sys.stderr.flush()
        python = sys.executable
        os.execl(python, python, *sys.argv)        

if __name__ == "__main__":
    main = MainThread()
    
    imports = [
        ('rules.Always', ['']),
        ('rules.CUA', ['']),
        ('rules.Chrome', ['']),
        ('rules.emacs.Belt', ['']),
        ('rules.emacs.BufferNames', ['']),
        ('rules.emacs.Dired', ['']),
        ('rules.emacs.Emacs', ['']),
        ('rules.emacs.Eww', ['']),
        ('rules.emacs.Python', ['']),
        ('rules.emacs.Lisp', ['']),
        ('rules.emacs.Edit', ['']),
        ('rules.emacs.ERC', ['']),
        ('rules.emacs.VarNames', ['']),
        ('rules.emacs.Pairs', ['']),
        ('rules.emacs.Profiling', ['']),
        ('rules.emacs.Mic', ['']),
        ('rules.emacs.Magit', ['']),
        ('rules.emacs.Nav', ['']),
        ('rules.emacs.NickNames', ['']),
        ('rules.emacs.Org', ['']),
        ('rules.emacs.ProjectFileNames', ['']),
        ('rules.emacs.ProjectNames', ['']),
        ('rules.emacs.Term', ['']),
        ('rules.emacs.ModeLine', ['']),
        ('rules.emacs.Snippet', ['']),
        ('rules.emacs.SymbolPicker', ['']),
        ('rules.emacs.Words', ['']),
        ('rules.WindowNames', ['']),
        ('rules.XMonad', ['']),
        ('RefreshClient', ['']),
    ]

    # TODO: catch syntax errors, make copies of module files, then
    # try to import again with offending line removed
    for module, fromlist in imports:
        try:
            __import__(module, globals(), locals(), fromlist)
        except Exception as e:
            log.info("Couldn't import %s" % module)
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            # raise


    main()
