import socket
import sys
import inspect
import errno
import os
from EventThread import EventThread
from Actions import keys

from dfly_parser import parseMessages, MESSAGE_TERMINATOR, ARG_DELIMETER

def restartMandimus():
    sys.stdout.flush()
    sys.stderr.flush()
    python = sys.executable
    os.execl(python, python, *sys.argv)

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

class XMonadRule(object):
    mapping  = {
        "left" : keys("ctrl+alt+s"),
        "right" : keys("ctrl+alt+h"),
        "move left" : keys("ctrl+alt+a"),
        "move right" : keys("ctrl+alt+t"),
        "next" : keys("ctrl+alt+e"),
        "previous" : keys("ctrl+alt+o"),
        "move next" : keys("ctrl+alt+shift+e"),
        "move previous" : keys("ctrl+alt+shift+o"),
        "expand" : keys("ctrl+alt+i"),
        "shrink" : keys("ctrl+alt+n"),
        "cycle" : keys("ctrl+alt+backslash"),
        "kill window" : keys("ctrl+alt+x"),
        "make master" : keys("ctrl+alt+Return"),
        "editor" : keys("ctrl+alt+w"),
        "browser" : keys("ctrl+alt+b"),
        "new terminal" : keys("ctrl+shift+alt+t"),
        "restart window manager" : keys("ctrl+alt+q"),
        "restart mandimus" : restartMandimus
        }
    
    extras = [
        Integer("n", 1, 20),
        Dictation("text"),
        ]
    
    defaults = {
        "n": 1,
        }

class DragonflyThread(EventThread):
    def __init__(self, address, pushQ):
        self.address = address
        EventThread.__init__(self, pushQ)
        
    def __call__(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(self.address)
        self.socket.listen(1)
        self.client = None
        self.buf = ''

        while self.run:
            if not self.client:
                # we use a timeout so ctrl-c will work
                self.socket.settimeout(0.25)
                try:
                    print 'waiting for connection'
                    self.client, addr = self.socket.accept()
                    print 'connected'
                    self.onConnect()
                except socket.timeout:
                    continue
                
            messages = []
            try:
                self.buf += self.recv()
                (self.buf, messages) = parseMessages(self.buf)
            except socket.timeout as e:
                continue
            except socket.error as e:
                if e.errno == errno.EAGAIN or e.errno == errno.EINTR:
                    print os.strerror(e.errno)
                    continue
                else:
                    print 'dumping client'
                    self.client = None
                    continue

            for msg in messages:
                self.onMessage(msg)

    def recv(self):
        self.client.settimeout(0.2)
        return self.client.recv(4096)

    def send(self, msg):
        print 'sending ' + msg
        self.client.settimeout(None)
        self.client.sendall(msg + MESSAGE_TERMINATOR)

    def onConnect(self):
        self.loadGrammar(XMonadRule)

    def loadGrammar(self, grammar):
        msg = []
        msg += ["GRAMMAR", ARG_DELIMETER.join(grammar.mapping.keys())]
        msg += [ARG_DELIMETER]
        msg += ["EXTRAS"]
        if hasattr(grammar, "extras"):
            for extra in grammar.extras:
                msg += [ARG_DELIMETER, str(extra)]
        msg += [ARG_DELIMETER]
        msg += ["DEFAULTS"]
        if hasattr(grammar, "defaults"):
            for key, val in grammar.defaults.items():
                msg += [ARG_DELIMETER, str(key), ':', str(val)]
        msg += [ARG_DELIMETER]
        self.send(''.join(msg)) 

    def onMessage(self, msg):
        self.send("ack " + msg)
        self.pushQ.put(msg)

if __name__ == "__main__":
    import Queue
    q = Queue.Queue()
    sub = DragonflyThread(('', 23133), q)
    try:
        while True:
            # without a timeout, ctrl-c doesn't work because.. python
            ONEYEAR = 365 * 24 * 60 * 60
            ev = q.get(True, ONEYEAR)
            print "message: " + str(ev)
    except KeyboardInterrupt:
        sub.stop()
        sys.exit()
    
