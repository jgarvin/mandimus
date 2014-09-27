# This file will be rsynced automagically to the right place for dragon to see it.

from dragonfly import Grammar, CompoundRule
from dragonfly import get_engine
import natlink
import socket
import logging

logging.basicConfig()

def importOrReload(module_name, *names):
    import sys
    
    if module_name in sys.modules:
        reload(sys.modules[module_name])
        for name in names:
            globals()[name] = getattr(sys.modules[module_name], name)
    else:
        __import__(module_name, names)

print "-----------load--------------"

# load our project specific modules this way so
# that changes apply when NatLink reloads our code
importOrReload("dfly_parser", "parseMessages")

class DragonflyClient(object):
    def __init__(self):
        self.timer = get_engine().create_timer(self.eventLoop, 1)
        self.sock = None
        self.testSent = False
        self.buf = ""

    def eventLoop(self):
        if not self.sock:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect(("10.0.0.2", 23133))

        if not self.testSent:
            self.testSent = True
            self.send("test###>>>data###>>>print hi###>>>")
            
        messages = []
        try:
            self.sock.setblocking(False)
            buf = self.sock.recv(4096)
            (buf, messages) = parseMessages(buf)
        except socket.error as e:
            pass

        for msg in messages:
            self.onMessage(msg)

    def send(self, data):
        self.sock.setblocking(True)
        self.sock.sendall(data)

    def onMessage(self, msg):
        print "Message: " + msg
        
    def unload(self):
        self.timer.stop()
        self.sock.close()

# Voice command rule combining spoken form and recognition processing.
class ExampleRule(CompoundRule):
    spec = "do something computer"                  # Spoken form of command.
    def __init__(self):
        CompoundRule.__init__(self)

    def _process_recognition(self, node, extras):   # Callback when command is spoken.
        print "Voice command spoken."

# Create a grammar which contains and loads the command rule.
grammar = Grammar("example grammar")                # Create a grammar to contain the command rule.
grammar.add_rule(ExampleRule())                     # Add the command rule to the grammar.
grammar.load()

client = DragonflyClient()

def unload():
    grammar.unload()
    client.unload()
    print "----------unload-------------"

# Local Variables:
# eval: (add-hook 'after-save-hook (lambda () (shell-command (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem/_%s" (buffer-file-name) (getenv "HOME") (buffer-name)))) nil t)
# End:
