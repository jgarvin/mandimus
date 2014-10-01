# This file will be rsynced automagically to the right place for dragon to see it.

from dragonfly import Grammar, CompoundRule, MappingRule, ActionBase, Key, Text, Integer, Dictation
from dragonfly import get_engine
import natlink
import socket
import sys, os
from functools import partial

# Without this stderr won't go to the Natlink
# message window.
import logging
# logging.basicConfig(filename="E:\\log.txt", filemode='w+')
logging.basicConfig()

# Natlink reloads modules when the mic is woken up
# if the file has changed, so we need to support
# code reloading.
def importOrReload(module_name, *names):
    import sys
    
    if module_name in sys.modules:
        # if hasattr(sys.modules[module_name], "unload") and callable(sys.modules[module_name].unload):
        #     sys.modules[module_name].unload()
        reload(sys.modules[module_name])
    else:
        __import__(module_name, fromlist=names)
        
    for name in names:
        globals()[name] = getattr(sys.modules[module_name], name)

print "-----------load--------------"

# load our project specific modules this way so
# that changes apply when NatLink reloads our code,
# otherwise Natlink will reload dfly_client but not
# the things that dfly_client imports
importOrReload("dfly_parser", "parseMessages", "MESSAGE_TERMINATOR", "ARG_DELIMETER",
               "MATCH_MSG_START")

class ReportingAction(ActionBase):
    def __init__(self, grammarString, dclient):
        self.dclient = dclient
        self.grammarString = grammarString
        ActionBase.__init__(self)
        
    def _execute(self, data=None):
        self.dclient.onMatch(self.grammarString, data)

class DragonflyClient(object):
    def __init__(self):
        # Natlink doesn't provide a way to poll of files or sockets,
        # and it runs in the same thread as Dragon itself so we can't
        # block, so we run on a periodic timer.
        self.timer = get_engine().create_timer(self._eventLoop, 1)
        self.sock = None
        self.testSent = False
        self.buf = ""
        self.grammar = None

    def _eventLoop(self):
        try:
            self.eventLoop()
        except Exception as e:
            print e.__doc__
            print e.message
            self.unload()

    def eventLoop(self):
        if self.sock is None:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
            self.sock.settimeout(5)
            try:
                self.sock.connect(("10.0.0.2", 23133))
                print 'connected'
            except socket.timeout as e:
                print 'connection timed out, resetting to none'
                self.sock = None
                return

        if not self.testSent:
            self.testSent = True
            self.sendMsg("test")
            self.sendMsg("foo")
            
        messages = []
        try:
            self.sock.setblocking(False)
            self.buf += self.sock.recv(4096)
            (self.buf, messages) = parseMessages(self.buf)
        except socket.error as e:
            pass

        for msg in messages:
            self.onMessage(msg)

    def sendMsg(self, data):
        print 'called with ' + data
        self.sock.setblocking(True)
        self.sock.sendall(data + MESSAGE_TERMINATOR)

    def onMessage(self, msg):
        print 'got msg: ' + msg
        if msg.startswith("GRAMMAR"):
            self.parseGrammarMsg(msg)

    def parseGrammarMsg(self, msg):
        msg = msg.split("GRAMMAR")[1]
        grammars, extras = msg.split("EXTRAS")
        extras, defaults = extras.split("DEFAULTS")
        
        omapping = filter(lambda a: a != '', grammars.split(ARG_DELIMETER))
        omapping = self.transformMapping(omapping)
        
        oextras = filter(lambda a: a != '', extras.split(ARG_DELIMETER))
        oextras = self.parseExtras(oextras)
        
        odefaults = filter(lambda a: a != '', defaults.split(ARG_DELIMETER)) 
        odefaults = self.parseDefaults(odefaults)
        
        class NewRule(MappingRule):
            mapping = omapping
            extras = oextras
            defaults = odefaults

        if self.grammar is not None:
            self.grammar.unload()
        self.grammar = Grammar("The grammar")
        self.grammar.add_rule(NewRule())
        self.grammar.load()
        
    def parseExtras(self, extras):
        parsed = []
        # print extras
        for e in extras:
            # print e
            e = e.split()
            if e[0] == "INTEGER":
                parsed.append(Integer(e[1], int(e[2]), int(e[3])))
            elif e[0] == "DICTATION":
                parsed.append(Dictation(e[1]))
        return parsed
        
    def parseDefaults(self, defaults):
        parsed = {}
        for e in defaults:
            e = e.split(':')
            try:
                parsed[e[0]] = int(e[1])
            except ValueError:
                parsed[e[0]] = e[1]
        return parsed
    
    def onMatch(self, grammarString, data):
        # print 'data ' + str(data)
        # print 'node ' + str(' '.join(data['_node'].words()))
        # print 'rule ' + str(data['_rule'].name)
        # print 'grammar ' + str(data['_grammar'].name)
        msg = [MATCH_MSG_START, grammarString, ARG_DELIMETER]
        msg += [' '.join(data['_node'].words()), ARG_DELIMETER]
        if data:
            # TODO: we really should be sending the whole node structure
            # so we can have more elaborate phrases that change meaning
            # based on what was actually said...
            for key, value in data.items():
                if isinstance(value, int) or isinstance(value, str):
                    msg += [str(key), ":", str(value), ARG_DELIMETER]
        self.sendMsg(''.join(msg))

    def unload(self):
        self.timer.stop()
        if self.sock is not None:
            print 'closing socket'
            self.sock.close()
        if self.grammar:
            self.grammar.unload()

    def transformMapping(self, grammarList):
        """We never perform actions directly, we just send
        back to the client that we have a match."""
        mapping = {}
        for g in grammarList:
            mapping[g] = ReportingAction(g, self)
        return mapping
        
client = DragonflyClient()

def unload():
    client.unload()
    print "----------unload-------------"
    
# Local Variables:
# eval: (add-hook 'after-save-hook (lambda () (shell-command (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem/_%s" (buffer-file-name) (getenv "HOME") (buffer-name)))) nil t)
# End:



