import socket
import sys
import inspect
import errno
import os
import time
import Queue
from EventThread import EventThread

from dfly_parser import parseMessages, MESSAGE_TERMINATOR, ARG_DELIMETER
from DragonflyNode import DragonflyNode, ConnectedEvent

def threadRequest(f):
    def wrapper(self, *args, **kwargs):
        self.pullQ.put((f, args, kwargs))
    return wrapper

class DragonflyThread(EventThread, DragonflyNode):
    def __init__(self, address, pushQ):
        self.address = address
        EventThread.__init__(self, pushQ)
        DragonflyNode.__init__(self)
        self.grammars = set()
        self.pullQ = Queue.Queue()
        
    def __call__(self):
        self.server_socket = self.makeSocket()
        self.server_socket.bind(self.address)
        self.server_socket.listen(1)
        self.other = None
        self.buf = ''

        while self.run:
            if not self.other:
                # we use a timeout so ctrl-c will work
                self.server_socket.settimeout(0.25)
                try:
                    print 'waiting for connection'
                    self.other, addr = self.server_socket.accept()
                    print 'connected'
                    self.onConnect()
                except socket.timeout:
                    continue
                
            self.runRequests()
            self.retrieveMessages()
            self.heartbeat()
            
        self.cleanup()

    def runRequests(self):
        while True:
            try:
                request = self.pullQ.get(False)
            except Queue.Empty:
                break
            request[0](self, *request[1], **request[2])

    def cleanup(self):
        DragonflyNode.cleanup(self)
        if self.server_socket is not None:
            self.server_socket.close()

    @threadRequest
    def loadGrammar(self, grammar):
        if grammar in self.grammars:
            return
        
        print 'Loading grammar: ' + grammar.__name__
        self.sendMsg(grammar.textSerialize())
        self.grammars.add(grammar)

    @threadRequest
    def unloadGrammar(self, grammar):
        if grammar not in self.grammars:
            return
        
        print 'Unloading grammar: ' + grammar.__name__        
        if grammar not in self.grammars:
            return # TODO: do I want this?
        self.sendMsg('unload' + ARG_DELIMETER + grammar.__name__)
        self.grammars.remove(grammar)
        
    def onConnect(self):
        self.pushQ.put(ConnectedEvent())
        oldGrammars = self.grammars
        self.grammars = set()
        for g in oldGrammars:
            self.unloadGrammar(g)
            self.loadGrammar(g)

    def onMessage(self, msg):
        if msg.startswith("MATCH"):
            self.parseMatchMsg(msg)
        self.sendMsg("ack " + msg)

    def parseMatchMsg(self, msg):
        msg = msg.split("MATCH")[1]
        tokens = msg.split(ARG_DELIMETER)
        grammar, words = tokens[:2]
        extras = tokens[2:]
        
        extras = [g for g in extras if g != '']
        extras = self.parseExtras(extras)

        # todo replace this with MatchEvent
        for g in self.grammars:
            if grammar in g.mapping:
                g.mapping[grammar](extras)

    def parseExtras(self, extras):
        parsed = {}
        for e in extras:
            e = e.split(':')
            try:
                parsed[e[0]] = int(e[1])
            except ValueError:
                parsed[e[0]] = e[1]
        return parsed

    
      

