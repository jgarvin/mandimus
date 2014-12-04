import mdlog
log = mdlog.getLogger(__name__)

from hotCode import importOrReload

import time, socket, errno, select

importOrReload("dfly_parser", "parseMessages", "MESSAGE_TERMINATOR")
importOrReload("EventList", "MicrophoneEvent", "ConnectedEvent", "DisconnectedEvent")

class DragonflyNode(object):
    def __init__(self, eventQ=None):
        self.other = None
        self.lastMsgSendTime = time.time()
        self.eventQ = eventQ

        self.outgoing = []

    def makeSocket(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
        sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
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
                log.info(os.strerror(e.errno))
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
        #log.info('receiving...')
        self.other.settimeout(0.05)
        return unicode(self.other.recv(4096), 'utf-8')            

    def cleanup(self):
        if self.other is not None:
            self.other.close()
        self.other = None

    def onMessage(self):
        pass

    def onConnect(self):
        pass

    def dumpOther(self):
        if self.other is not None:
            log.info('other lost')
        self.other.close()
        self.other = None
        if self.eventQ:
            self.eventQ.put(DisconnectedEvent)

    def sendMsg(self, msg):
        if self.other is None:
            log.info("can't send msg, not connected")
            return
        
        if len(msg) and not msg.startswith('ack'): # don't log.info(heartbeats)
            pass

        try:
            self.other.settimeout(None)
            try:
                self.other.sendall((msg + MESSAGE_TERMINATOR).encode('utf-8'))
            except UnicodeDecodeError as e:
                log.error(str(e))
                log.error("attempted msg: [%s]" %msg)
                #log.error("problem area: %s [%s]" % (msg[35683], msg[35680:35686]))
            self.lastMsgSendTime = time.time()
        except socket.error as e:
            if e.errno == errno.EPIPE or e.errno == errno.EBADF:
                self.dumpOther()
            else:
                raise

### DRAGONSHARE RSYNC

