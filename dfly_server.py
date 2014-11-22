import socket
import sys
import inspect
import errno
import os
import time
import traceback

from dfly_parser import parseMessages, MESSAGE_TERMINATOR, ARG_DELIMETER, KEY_VALUE_SEPARATOR
from DragonflyNode import DragonflyNode, ConnectedEvent
from namedtuple import namedtuple
from Actions import Repeat
from EventLoop import getLoop

GrammarMatchEvent = namedtuple("GrammarMatchEvent", "grammar, extras")

BLOCK_TIME = 0.05

class DragonflyThread(DragonflyNode):
    def __init__(self, address, pushQ):
        self.address = address
        self.pushQ = pushQ
        DragonflyNode.__init__(self)
        self.grammars = {}
        self.history = []
        
        self.server_socket = self.makeSocket()
        self.server_socket.bind(self.address)
        self.server_socket.listen(1)
        self.other = None
        self.buf = ''

        getLoop().subscribeTimer(BLOCK_TIME, self)

    def __call__(self):
        if not self.other:
            # we use a timeout so ctrl-c will work
            self.server_socket.settimeout(BLOCK_TIME)
            try:
                #print 'waiting for connection'
                self.other, addr = self.server_socket.accept()
                print 'connected'
                self.onConnect()
            except socket.timeout:
                return

        self.retrieveMessages()
        self.heartbeat()

    def cleanup(self):
        DragonflyNode.cleanup(self)
        if self.server_socket is not None:
            self.server_socket.close()

    def loadGrammar(self, grammar):
        if grammar.name in self.grammars:
            if grammar == self.grammars[grammar.name]:
                #print 'grammar UNchanged: ' + grammar.__name__
                return
            else:
                print 'grammar changed: ' + grammar.name
                self.unloadGrammar(self.grammars[grammar.name])
        
        print 'Loading grammar: ' + grammar.name
        self.sendMsg(grammar.textSerialize())
        self.grammars[grammar.name] = grammar

    def unloadGrammar(self, grammar):
        if grammar.name not in self.grammars:
            return
        
        print 'Unloading grammar: ' + grammar.name        
        self.sendMsg('unload' + ARG_DELIMETER + grammar.name)
        del self.grammars[grammar.name]

    def onConnect(self):
        self.pushQ.put(ConnectedEvent())
        oldGrammars = self.grammars
        self.grammars = {}
        for k, g in oldGrammars.items():
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
        extras['grammar'] = grammar
        extras['words'] = words

        self.onMatch(grammar, extras)

    def onMatch(self, grammar, extras):
        # todo replace this with MatchEvent
        for k, g in self.grammars.items():
            if grammar in g.mapping:
                try:
                    cb = g.mapping[grammar]

                    if isinstance(cb, Repeat):
                        if len(self.history) >= 1:
                            repetitions = extras['n']
                            grammar, extras = self.history[-1]
                            for i in range(repetitions):
                                self.onMatch(grammar, extras)
                            return
                    else:
                        self.history.append(GrammarMatchEvent(grammar, extras))

                    print 'match %s -- %s' % (grammar, extras['words'])
                    cb(extras)
                except Exception as e:
                    # don't want the whole thing to crash just because
                    # of one bad rule
                    traceback.print_exc()

    def parseExtras(self, extras):
        parsed = {}
        for e in extras:
            e = e.split(KEY_VALUE_SEPARATOR)
            try:
                parsed[e[0]] = int(e[1])
            except ValueError:
                parsed[e[0]] = e[1]
        return parsed

    
      

