import time, socket, errno

from dfly_parser import parseMessages, MESSAGE_TERMINATOR

class ConnectedEvent(object):
    pass

class DragonflyNode(object):
    def __init__(self):
        self.other = None
        self.lastMsgSendTime = time.time()

    def makeSocket(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
        return sock

    def retrieveMessages(self):
        if self.other is None:
            return

        messages = []
        try:
            self.buf += self.recv()
            (self.buf, messages) = parseMessages(self.buf)
        except socket.timeout as e:
            pass
        except socket.error as e:
            if e.errno == errno.EAGAIN or e.errno == errno.EINTR:
                print os.strerror(e.errno)
            else:
                self.dumpOther()
                return

        for msg in messages:
            self.onMessage(msg)

    def heartbeat(self):
        if self.other is None:
            return        

        # heartbeating
        newtime = time.time()
        if newtime - self.lastMsgSendTime > 1 and self.other is not None:
            self.sendMsg('')

    def recv(self):
        #print 'receiving...'
        self.other.settimeout(0.2)
        return self.other.recv(4096)            

    def cleanup(self):
        if self.other is not None:
            self.other.close()

    def onMessage(self):
        pass

    def onConnect(self):
        pass

    def dumpOther(self):
        print 'other lost'
        self.other.close()
        self.other = None

    def sendMsg(self, msg):
        if len(msg) and not msg.startswith('ack'): # don't print heartbeats
            print 'sending ' + msg
        self.other.settimeout(None)
        try:
            self.other.sendall(msg + MESSAGE_TERMINATOR)
            self.lastMsgSendTime = time.time()
        except socket.error as e:
            if e.errno == errno.EPIPE or e.errno == errno.EBADF:
                self.dumpOther()
            else:
                raise

### DRAGONSHARE RSYNC

