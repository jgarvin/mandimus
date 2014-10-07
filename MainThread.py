import os, sys
import Queue
from dfly_server import DragonflyThread
from Actions import keys
from DragonflyNode import ConnectedEvent
from WindowEventWatcher import WindowEventWatcher, FocusChangeEvent

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

class ChromeRule(ServerSeriesMappingRule):
    mapping  = {
        "new tab" : keys("c-t"),
        "close tab" : keys("c-w"),
        "address" : keys("c-l"),
        "next tab" : keys("c-tab"),
        "previous tab" : keys("cs-tab"),
        "back" : keys("a-left"),
        "forward" : keys("a-right"),
        "refresh" : keys("F5"),
        "reopen tab" : keys("cs-t"),
        "enter" : keys("enter"),
        "tab" : keys("tab")
        }
    
class XMonadRule(ServerSeriesMappingRule):
    mapping  = {
        "left" : keys("ca-s"),
        "right" : keys("ca-h"),
        "move left" : keys("ca-a"),
        "move right" : keys("ca-t"),
        "next" : keys("ca-e"),
        "previous" : keys("ca-o"),
        "move next" : keys("cas-e"),
        "move previous" : keys("cas-o"),
        "expand" : keys("ca-i"),
        "shrink" : keys("ca-n"),
        "cycle" : keys("ca-space"),
        "kill window" : keys("ca-x"),
        "make master" : keys("ca-enter"),
        "editor" : keys("ca-w"),
        "browser" : keys("ca-b"),
        "new terminal" : keys("csa-t"),
        "restart window manager" : keys("ca-q"),
        #"restart mandimus" : restartMandimus
        }
    
    extras = [
        Integer("n", 1, 20),
        Dictation("text"),
        ]
    
    defaults = {
        "n": 1,
        }

class RestartEvent(object):
    pass

class MainThread(object):
    def __init__(self):
        self.eventQ = Queue.Queue()
        self.dfly = DragonflyThread(('', 23133), self.eventQ)
        self.win = WindowEventWatcher(self.eventQ)
        self.run = True

    def __call__(self):
        try:
            while self.run:
                # without a timeout, ctrl-c doesn't work because.. python
                ONEYEAR = 365 * 24 * 60 * 60
                ev = self.eventQ.get(True, ONEYEAR)
                if isinstance(ev, ConnectedEvent):
                    class RestartRule(ServerMappingRule):
                        mapping = { "restart mandimus" : (lambda: self.eventQ.put(RestartEvent())) }
                    self.dfly.loadGrammar(RestartRule)
                    self.dfly.loadGrammar(XMonadRule)
                elif isinstance(ev, RestartEvent):
                    self.restart()
                elif isinstance(ev, FocusChangeEvent):
                    if "chrome" in ev.window.wmclass or "chromium" in ev.window.wmclass:
                        self.dfly.loadGrammar(ChromeRule)
                    else:
                        self.dfly.unloadGrammar(ChromeRule)
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
