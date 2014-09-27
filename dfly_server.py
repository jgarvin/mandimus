import SocketServer
import sys
from EventThread import EventThread

from dfly_parser import parseMessages

class DragonflyServer(SocketServer.TCPServer):
    allow_reuse_address = True
    def __init__(self, addr, cls, q):
        self.pushQ = q
        self.buf = ""
        SocketServer.TCPServer.__init__(self, addr, cls)
        self.timeout = 0.2

class DragonflyHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        "this is redonkuslously inefficient, but it doesn't matter"
        self.server.buf += self.request.recv(1024)
        self.server.buf, messages = parseMessages(self.server.buf)
        for m in messages:
            self.server.pushQ.put(m)
            
class DragonflyThread(EventThread):
    def __init__(self, address, pushQ):
        self.address = address
        EventThread.__init__(self, pushQ)
        
    def __call__(self):
        self.server = DragonflyServer(self.address, DragonflyHandler, self.pushQ)
        while self.run:
            self.server.handle_request()

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
    
# Local Variables:
# eval: (add-hook 'after-save-hook (lambda () (shell-command (format "touch %s/dragonshare/NatLink/NatLink/MacroSystem/_dfly_client.py" (getenv "HOME")))) nil t)
# End:
