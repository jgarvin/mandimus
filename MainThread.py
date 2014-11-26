import mdlog, os
mdlog.initLogging("server", "/tmp", stdOut=True)
log = mdlog.getLogger(__name__)

import os, sys
from dfly_server import DragonflyThread
from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, SelectWindow
from WindowEventWatcher import WindowEventWatcher, FocusChangeEvent, WindowListEvent
from Window import Window, getFocusedWindow
from wordUtils import extractWords, buildSelectMapping, punc2Words
import EventLoop
import re
import time
import traceback
from Events import GrammarEvent
from EventList import MicrophoneEvent, ConnectedEvent
from rules.Rule import registerRule, registeredRules
from rules.SeriesMappingRule import SeriesMappingRule
from rules.MappingRule import MappingRule
from rules.Elements import Integer, Dictation

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

spokenWindowRules = set()
def spokenWindowRule(f):
    global spokenWindowRules
    spokenWindowRules.add(f)
    return f

@spokenWindowRule
def weechat(w):
    if "terminal" in w.wmclass.lower() and "weechat" in w.name:
        return ["we", "chat"]
    return []


@spokenWindowRule
def chromium(w):
    "without this open tabs affect the name"
    if "chromium" in w.wmclass.lower() and "chromium" in w.name.lower():
        return ["chrome", "chromium"]
    return []

# so write rules for specific window types
# and then fall back to generic word search
# grammar only when the windows aren't special
# cased?

class RestartEvent(object): pass
class ExitEvent(object): pass

class TimerEntry(object):
    def __init__(self, nextExpiration, callback, seconds):
        self.nextExpiration = nextExpiration
        self.callback = callback
        self.seconds = seconds

class MainThread(object):
    def __init__(self):
        # this needs to run before any user modes are imported
        self.timers = []
        EventLoop.event_loop = self        

        self.dfly = DragonflyThread(('', 23133), self)
        self.win = WindowEventWatcher(self, filterWindows)
        self.run = True
        
        self.events = []

        self.eventSubscribers = {}

        self.determineCallbacks = set()

        self.subscribeEvent(ConnectedEvent, self.onConnect)
        self.subscribeEvent(RestartEvent, self.restart)
        self.subscribeEvent(ExitEvent, self.stop)
        self.subscribeEvent(FocusChangeEvent, self.onFocusChange)
        self.subscribeEvent(WindowListEvent, self.handleWindowList)
        self.subscribeEvent(GrammarEvent, self.handleGrammarEvent)
        
    def subscribeEvent(self, eventType, handler, priority=100):
        if eventType not in self.eventSubscribers:
            self.eventSubscribers[eventType] = []
        self.eventSubscribers[eventType].append((priority, handler))
        self.eventSubscribers[eventType].sort(key=lambda x: x[0])

    def subscribeTimer(self, seconds, cb):
        self.timers.append(TimerEntry(time.time() + seconds, cb, seconds))

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
                t.callback()
    
    def onFocusChange(self, ev):
        self.determineRules(ev.window)

    def onDetermineRules(self, handler):
        self.determineCallbacks.add(handler)

    def determineRules(self, window):
        for h in self.determineCallbacks:
            h()

        active = set()
        for r in registeredRules().values():
            if r.activeForWindow(window):
                active.add(r)
        self.dfly.updateRuleEnabledness(active)
    
    def put(self, p):
        self.events += [p]

    def processEvent(self, ev):
        if type(ev) in self.eventSubscribers:
            for h in self.eventSubscribers[type(ev)]:
                h[1](ev)        

    def onConnect(self, ev=None):
        class MainRule(SeriesMappingRule):
            mapping = { "restart mandimus" : (lambda x: self.put(RestartEvent())),
                        "completely exit mandimus" : (lambda x: self.put(ExitEvent())) }
            def activeForWindow(self, w):
                return True

        
        registerRule(MainRule)
        # so that rules apply for whatever is focused on startup
        self.determineRules(getFocusedWindow())
        print "everything is set up"
        self.dfly.requestStartupComplete()

    def __call__(self):
        try:
            while self.run:
                time.sleep(self.timeout())
                
                self.dispatchTimers()

                try:
                    ev = self.events.pop()
                except IndexError:
                    continue

                self.processEvent(ev)
        except KeyboardInterrupt:
            self.stop()
            sys.exit()

    def handleGrammarEvent(self, ev):
        if ev.load:
            self.dfly.loadGrammar(ev.grammar)
        else:
            self.dfly.unloadGrammar(ev.grammar)

    def handleWindowList(self, ev):
        # sometimes at startup list is empty
        if not ev.windows:
            return

        spokenWindows = {}
        for w in ev.windows:
            global spokenWindowRules
            spokenForms = []
            for rule in spokenWindowRules:
                spokenForms = rule(w)
                if spokenForms != []:
                    spokenForms = [set(spokenForms)]
                    break

            if spokenForms == []:
                # thought about using name instead of wmclass,
                # but the title tends to contain debris like
                # the name of the currently opened document/page
                nameset = extractWords(w.name, translate=punc2Words)
                classset = extractWords(w.wmclass, translate=punc2Words)
                spokenForms = [nameset, classset]

            spokenWindows[w] = spokenForms

        # remove empty sets
        for w, spokenForms in spokenWindows.items():
            try:
                spokenForms.remove(set())
            except ValueError:
                pass # python is stupid
            try:
                spokenForms.remove(set(u''))
            except ValueError:
                pass # python is stupid
        # remove windows that map to no forms
        spokenWindows = dict((k, v) for k, v in spokenWindows.iteritems() if v)

        omapping = buildSelectMapping('win', spokenWindows, SelectWindow)

        class WindowRule(MappingRule):
            mapping = omapping
            def activeForWindow(self, w):
                return True

        registerRule(WindowRule)

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
        ('rules.emacs.Dired', ['']),
        ('rules.emacs.Emacs', ['']),
        ('rules.emacs.Python', ['']),
        ('rules.emacs.ERC', ['']),
        ('rules.emacs.VarNames', ['']),
        ('rules.emacs.Pairs', ['']),
        ('rules.emacs.Mic', ['']),
        ('rules.emacs.Magit', ['']),
        ('rules.XMonad', ['']),
        ('rules.CUA', ['']),
        ('rules.Chrome', ['']),
    ]

    # TODO: catch syntax errors, make copies of module files, then
    # try to import again with offending line removed
    for module, fromlist in imports:
        try:
            __import__(module, globals(), locals(), fromlist)
        except Exception as e:
            log.info("Couldn't import %s" % module)
            traceback.print_exc()

    main()
