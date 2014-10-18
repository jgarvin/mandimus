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

@registerRule
class CUARule(SeriesMappingRule):
    mapping = {
        "copy" : Key("c-c"),
        "cut" : Key("c-x"),
        "paste" : Key("c-v"),
        "term paste" : Key("s-insert"),
        "select all" : Key("c-a"),
        "undo [that]" : Key("c-z"),
        "redo [that]" : Key("c-y"),
        "next form" : Key("tab"),
        "previous form" : Key("s-tab"),
        "escape" : Key("escape"),
        "find [<search_terms>]" : Key("c-f") + Text("%(search_terms)s"),
        "find next" : Key("F3"),
        "enter" : Key("enter"),
        "page up" : Key("pgup"),
        "page down" : Key("pgdown"),
        }

    extras = [
        Dictation("text"),
        Dictation("search_terms"),
        ]

    defaults = {
        "search_terms" : "",
        }

    @classmethod
    def activeForWindow(cls, window):
        return not EmacsRule.activeForWindow(window)

@registerRule
class ChromeRule(SeriesMappingRule):
    mapping  = {
        "new tab" : Key("c-t"),
        "close tab" : Key("c-w"),
        "address" : Key("c-l"),
        "next tab" : Key("c-tab"),
        "previous tab" : Key("cs-tab"),
        "(reopen tab | undo close tab)" : Key("cs-t"),
        "back" : Key("a-left"),
        "forward" : Key("a-right"),
        "refresh" : Key("F5"),
        "reopen tab" : Key("cs-t"),
        "enter" : Key("enter"),
        "tab" : Key("tab"),
        "reload" : Key("c-r"),
        "refresh" : Key("c-r"),
        "search <text>" : Key("c-l, c-a, backspace") + Text("%(text)s") + Key("enter"),
        "zoom in [<n>]" : Key("c-plus:%(n)d"),
        "zoom out [<n>]" : Key("c-minus:%(n)d"),
        # these are provided by the 'tabloid' extension
        "move tab right" : Key("as-l"),
        "move tab left" : Key("as-h"),
        "move tab to start" : Key("as-k"),
        "move tab to end" : Key("as-j"),
        # these are provided by the 'tabasco' extension
        "close other tabs" : Key("as-o"),
        "close tabs to the right" : Key("as-r"),
        "close right tabs" : Key("as-r"),
        "pin tab" : Key("as-p"),
        }

    extras = [
        Dictation("text"),
        Integer("n", 1, 20),
        ]

    defaults = {
        'n' : 1,
        }
    
    @classmethod
    def activeForWindow(cls, window):
        return "chrome" in window.wmclass or "chromium" in window.wmclass

@registerRule
class XMonadRule(SeriesMappingRule):
    mapping  = {
        "mon left [<n>]" : Key("ca-backspace:%(n)d"),
        "mon right [<n>]" : Key("ca-space:%(n)d"),
        "move left" : Key("ca-a"),
        "move right" : Key("ca-t"),
        "next [<n>]" : Key("ca-e:%(n)d"),
        "previous [<n>]" : Key("ca-o:%(n)d"),
        "move next" : Key("cas-e"),
        "move previous" : Key("cas-o"),
        "expand [<n>]" : Key("ca-i:%(n)d"),
        "shrink [<n>]" : Key("ca-n:%(n)d"),
        "cycle" : Key("ca-y"),
        "kill window" : Key("ca-x"),
        "make master" : Key("ca-enter"),
        "editor" : Key("ca-w"),
        "browser" : Key("ca-b"),
        "new terminal" : Key("csa-t"),
        "restart window manager" : Key("ca-q"),
        }
    
    extras = [
        Integer("n", 1, 20),
        Dictation("text"),
        ]
    
    defaults = {
        "n": 1,
        }

    @classmethod
    def activeForWindow(cls, window):
        return True

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
