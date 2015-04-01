import mdlog
log = mdlog.getLogger(__name__)

from hotCode import importOrReload

import time, socket, errno, select, struct, fcntl

from protocol import parseStream, HeartbeatMsg, makeJSON
from EventList import DisconnectedEvent

class DragonflyNode(object):
    def __init__(self, eventQ=None):
        self.other = None
        self.lastMsgSendTime = time.time()
        self.eventQ = eventQ

        self.outgoing = []
        self.nextMsgSize = 0
        self.buf = ""

    def makeSocket(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
        sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        fd = sock.fileno()
        old_flags = fcntl.fcntl(fd, fcntl.F_GETFD)
        fcntl.fcntl(fd, fcntl.F_SETFD, old_flags | fcntl.FD_CLOEXEC)
        return sock

    def retrieveMessages(self):
        if self.other is None:
            return

        messages = []
        
        try:
            self.buf += self.recv()
            (messages, self.buf, self.nextMsgSize) = parseStream(messages, self.buf, self.nextMsgSize)
        except socket.timeout as e:
            pass
        except socket.error as e:
            if e.errno == errno.EAGAIN or e.errno == errno.EINTR:
                log.info(os.strerror(e.errno))
            else:
                self.dumpOther()
                return

        self.messageBatch(messages)

    def messageBatch(self, messages):
        for msg in messages:
            self.onMessage(msg)

    def heartbeat(self):
        if self.other is None:
            return        

        # heartbeating
        newtime = time.time()
        if newtime - self.lastMsgSendTime > 1 and self.other is not None:
            self.sendMsg(makeJSON(HeartbeatMsg("")))

    def recv(self):
        # log.info('receiving...')
        self.other.setblocking(0)
        buf = []
        received = True
        try:
            while received:
                #received = unicode(self.other.recv(4096 * 1000), 'utf-8')
                received = self.other.recv(4096 * 1000)
                buf.append(received)
                #log.info("received: [%s]" % received)
        except socket.error as e:
            if e.errno == errno.EWOULDBLOCK:
                pass
            else:
                raise
            
        # log.info("buf: [%s]" % buf)
        return ''.join(buf)

    def cleanup(self):
        if self.other is not None:
            self.other.shutdown(socket.SHUT_RDWR)
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
            return False
        
        try:
            try:
                encodedMsg = msg.encode('utf-8')
                #log.info("Sending: [%s]" % encodedMsg)
                data = struct.pack("!I", len(encodedMsg)) + encodedMsg 
                self.other.settimeout(None)
                self.other.sendall(data)
                return True
            except UnicodeDecodeError as e:
                log.error(str(e))
                log.error("attempted msg: [%s]" % msg)
            self.lastMsgSendTime = time.time()
        except socket.error as e:
            log.info("Socket error: %s" % e)
            if e.errno == errno.EPIPE or e.errno == errno.EBADF:
                self.dumpOther()
            else:
                raise
        except Exception as e:
            log.info("Unknown error while sending: %s" % e)
            self.dumpOther()
            raise

        return False

### DRAGONSHARE RSYNC

