import socket
import sys
import inspect
import errno
import os
from EventThread import EventThread

from dfly_parser import parseMessages, MESSAGE_TERMINATOR

# class XMonadRule(object):
#     mapping  = {
#         "left" : keys("ctrl+alt+s"),
#         "right" : keys("ctrl+alt+h"),
#         "move left" : keys("ctrl+alt+a"),
#         "move right" : keys("ctrl+alt+t"),
#         "next" : keys("ctrl+alt+e"),
#         "previous" : keys("ctrl+alt+o"),
#         "move next" : keys("ctrl+alt+shift+e"),
#         "move previous" : keys("ctrl+alt+shift+o"),
#         "expand" : keys("ctrl+alt+i"),
#         "shrink" : keys("ctrl+alt+n"),
#         "cycle" : keys("ctrl+alt+backslash"),
#         "kill window" : keys("ctrl+alt+x"),
#         "make master" : keys("ctrl+alt+Return"),
#         "editor" : keys("ctrl+alt+w"),
#         "browser" : keys("ctrl+alt+b"),
#         "new terminal" : keys("ctrl+shift+alt+t"),
#         "restart window manager" : keys("ctrl+alt+q"),
#         "restart mandimus" : restartMandimus
#         }
    
#     extras = [
#         Integer("n", 1, 20),
#         Dictation("text"),
#         ]
    
#     defaults = {
#         "n": 1,
#         }

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
                    self.client, addr = self.socket.accept()
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
        self.client.settimeout(None)
        self.client.sendall(msg + MESSAGE_TERMINATOR)

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
    
