# This file will be rsynced automagically to the right place for dragon to see it.

from dragonfly import Grammar, CompoundRule, MappingRule, ActionBase, Key, Text, Integer, Dictation
from dragonfly import get_engine
import natlink
import socket
from functools import partial

# Without this stderr won't go to the Natlink
# message window.
import logging
logging.basicConfig(filename="E:\\log.txt", filemode='w+')

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
        self.grammarString = grammarString
        self.dclient = dclient
        ActionBase.__init__(self)
        
    def _execute(self, data=None):
        self.dclient.onMatch(self.grammarString, data)

class DragonflyClient(object):
    def __init__(self):
        # Natlink doesn't provide a way to poll of files or sockets,
        # and it runs in the same thread as Dragon itself so we can't
        # block, so we run on a periodic timer.
        self.timer = get_engine().create_timer(self.eventLoop, 1)
        self.sock = None
        self.testSent = False
        self.buf = ""

    def eventLoop(self):
        if not self.sock:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
            self.sock.settimeout(5)
            try:
                self.sock.connect(("10.0.0.2", 23133))
            except socket.timeout as e:
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
        print "Message: " + msg
        
    def onMatch(self, grammarString, data):
        msg = [MATCH_MSG_START, grammarString, ARG_DELIMETER]
        if data:
            print data
            for key, value in data.items():
                if isinstance(value, int) or isinstance(value, str):
                    msg += [str(key), ":", str(value), ARG_DELIMETER]
        self.sendMsg(''.join(msg))

    def unload(self):
        self.timer.stop()
        self.sock.close()

    def transformMapping(self, grammarList):
        """We never perform actions directly, we just send
        back to the client that we have a match."""
        mapping = {}
        for g in grammarList:
            mapping[g] = ReportingAction(g, self)
        return mapping
        
client = DragonflyClient()

class ExampleRule(MappingRule):
    mapping  = {
                "[feed] address [bar]":                Key("a-d"),
                "subscribe [[to] [this] feed]":        Key("a-u"),
                "paste [feed] address":                Key("a-d, c-v, enter"),
                "feeds | feed (list | window | win)":  Key("a-d, tab:2, s-tab"),
                "down [<n>] (feed | feeds)":           Key("a-d, tab:2, s-tab, down:%(n)d"),
                "up [<n>] (feed | feeds)":             Key("a-d, tab:2, s-tab, up:%(n)d"),
                "open [item]":                         Key("a-d, tab:2, c-s"),
                "newer [<n>]":                         Key("a-d, tab:2, up:%(n)d"),
                "older [<n>]":                         Key("a-d, tab:2, down:%(n)d"),
                "mark all [as] read":                  Key("cs-r"),
                "mark all [as] unread":                Key("cs-u"),
                "search [bar]":                        Key("a-s"),
                "search [for] <text>":                 Key("a-s") + Text("%(text)s\n"),
               }
    extras   = [
                Integer("n", 1, 20),
                Dictation("text"),
               ]
    defaults = {
                "n": 1,
               }

ExampleRule.mapping = client.transformMapping(ExampleRule.mapping.keys())

# Create a grammar which contains and loads the command rule.
grammar = Grammar("example grammar")                # Create a grammar to contain the command rule.
grammar.add_rule(ExampleRule())                     # Add the command rule to the grammar.
grammar.load()    

def unload():
    grammar.unload()
    client.unload()
    print "----------unload-------------"

# Local Variables:
# eval: (add-hook 'after-save-hook (lambda () (shell-command (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem/_%s" (buffer-file-name) (getenv "HOME") (buffer-name)))) nil t)
# End:

