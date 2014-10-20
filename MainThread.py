import os, sys
import Queue
from dfly_server import DragonflyThread
from Actions import Key, Text, Camel, Underscore, Hyphen, Speak
from DragonflyNode import ConnectedEvent
from WindowEventWatcher import WindowEventWatcher, FocusChangeEvent
from Window import Window
from rules.Rule import registerRule, registeredRules
from rules.SeriesMappingRule import SeriesMappingRule
from rules.Elements import Integer, Dictation
from rules.Always import AlwaysRule
from rules.Emacs import EmacsRule
from rules.XMonad import XMonadRule
from rules.CUA import CUARule
from rules.Chrome import ChromeRule

class RestartEvent(object):
    pass

class ExitEvent(object):
    pass

class MainThread(object):
    def __init__(self):
        self.eventQ = Queue.Queue()
        self.dfly = DragonflyThread(('', 23133), self.eventQ)
        self.win = WindowEventWatcher(self.eventQ)
        self.run = True

    def determineRules(self, window):
        for r in registeredRules():
            if r.activeForWindow(window):
                self.dfly.loadGrammar(r)
            else:
                self.dfly.unloadGrammar(r)

    def __call__(self):
        class MainRule(SeriesMappingRule):
            mapping = { "restart mandimus" : (lambda x: self.eventQ.put(RestartEvent())),
                        "completely exit mandimus" : (lambda x: self.eventQ.put(ExitEvent())) }


        try:
            while self.run:
                # without a timeout, ctrl-c doesn't work because.. python
                ONEYEAR = 365 * 24 * 60 * 60
                ev = self.eventQ.get(True, ONEYEAR)
                if isinstance(ev, ConnectedEvent):
                    self.dfly.loadGrammar(MainRule)

                    # so that rules apply for whatever is focused on startup
                    self.determineRules(Window(winId=Window.FOCUSED))
                elif isinstance(ev, RestartEvent):
                    self.restart()
                elif isinstance(ev, ExitEvent):
                    self.stop()
                    return
                elif isinstance(ev, FocusChangeEvent):
                    self.determineRules(ev.window)
                elif len(ev): # don't print heartbeats
                    print "message: " + str(ev)
        except KeyboardInterrupt:
            self.stop()
            sys.exit()

    def stop(self):
        self.run = False
        self.dfly.stop()
        self.win.stop()

    def restart(self):
        self.stop()
        sys.stdout.flush()
        sys.stderr.flush()
        python = sys.executable
        os.execl(python, python, *sys.argv)        

if __name__ == "__main__":
    main = MainThread()
    main()
