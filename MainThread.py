import os, sys
import Queue
from dfly_server import DragonflyThread
from Actions import Key, Text
from DragonflyNode import ConnectedEvent
from WindowEventWatcher import WindowEventWatcher, FocusChangeEvent
from Window import Window

from dfly_parser import ARG_DELIMETER

class Integer(object):
    def __init__(self, var, lower_bound, upper_bound):
        self.var = var
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound

    def __str__(self):
        return "INTEGER %s %d %d" % (self.var, self.lower_bound, self.upper_bound)

class Dictation(object):
    def __init__(self, var):
        self.var = var

    def __str__(self):
        return "DICTATION %s" % (self.var,)

class ServerMappingRule(object):
    mapping = {}
    extras = []
    defaults = {}
    serializedType = "ServerMappingRule"

    @classmethod
    def textSerialize(cls):
        serializeType = cls.serializedType
        serializeType = serializeType.split("Server", 1)[1]
        
        msg = []
        msg += [serializeType + ARG_DELIMETER + cls.__name__ + ARG_DELIMETER,
                ARG_DELIMETER.join(cls.mapping.keys())]
        msg += [ARG_DELIMETER]
        msg += ["EXTRAS"]
        for extra in cls.extras:
            msg += [ARG_DELIMETER, str(extra)]
        msg += [ARG_DELIMETER]
        msg += ["DEFAULTS"]
        for key, val in cls.defaults.items():
            msg += [ARG_DELIMETER, str(key), ':', str(val)]
        msg += [ARG_DELIMETER]
        return ''.join(msg)

class ServerSeriesMappingRule(ServerMappingRule):
    serializedType = "ServerSeriesMappingRule"

rules = set()    
def registerRule(f):
    global rules
    rules.add(f)
    return f

@registerRule
class EmacsRule(ServerSeriesMappingRule):
    mapping  = {
        "switch (buff | buffer)" : Key("c-x, b"),
        "other window" : Key("c-x, o"),
        "one window" : Key("c-x, 1"),
        "new frame" : Key("c-x, 5, 2"),
        "cancel" : Key("c-g"),
        }

    @classmethod
    def activeForWindow(cls, window):
        return "emacs" in window.wmclass or "Emacs" in window.wmclass
    
@registerRule
class ChromeRule(ServerSeriesMappingRule):
    mapping  = {
        "new tab" : Key("c-t"),
        "close tab" : Key("c-w"),
        "address" : Key("c-l"),
        "next tab" : Key("c-tab"),
        "previous tab" : Key("cs-tab"),
        "back" : Key("a-left"),
        "forward" : Key("a-right"),
        "refresh" : Key("F5"),
        "reopen tab" : Key("cs-t"),
        "enter" : Key("enter"),
        "tab" : Key("tab"),
        "reload" : Key("c-r"),
        "refresh" : Key("c-r"),
        "search <text>" : Key("c-l, c-a, backspace") + Text("%(text)s") + Key("enter"),
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
        Dictation("text")
        ]
    
    @classmethod
    def activeForWindow(cls, window):
        return "chrome" in window.wmclass or "chromium" in window.wmclass

@registerRule
class XMonadRule(ServerSeriesMappingRule):
    mapping  = {
        "left" : Key("ca-backspace"),
        "right" : Key("ca-space"),
        "move left" : Key("ca-a"),
        "move right" : Key("ca-t"),
        "next" : Key("ca-e"),
        "previous" : Key("ca-o"),
        "move next" : Key("cas-e"),
        "move previous" : Key("cas-o"),
        "expand" : Key("ca-i"),
        "shrink" : Key("ca-n"),
        "cycle" : Key("ca-space"),
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
        global rules
        for r in rules:
            if r.activeForWindow(window):
                self.dfly.loadGrammar(r)
            else:
                self.dfly.unloadGrammar(r)

    def __call__(self):
        class MainRule(ServerMappingRule):
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
