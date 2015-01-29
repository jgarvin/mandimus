import mdlog
log = mdlog.getLogger(__name__)

import socket
import sys
import inspect
import errno
import os
import time
import traceback

from dfly_parser import parseMessages, MESSAGE_TERMINATOR, ARG_DELIMETER, KEY_VALUE_SEPARATOR
from DragonflyNode import DragonflyNode
from namedtuple import namedtuple
from Actions import Repeat
from EventLoop import getLoop
from rules.Rule import registeredRules
from rules.SeriesMappingRule import SeriesMappingRule
from EventList import MicrophoneEvent, RuleMatchEvent, ConnectedEvent, StartupCompleteEvent, WordEvent
from copy import copy

BLOCK_TIME = 0.05

class DragonflyThread(DragonflyNode):
    def __init__(self, address, pushQ):
        self.address = address
        self.pushQ = pushQ
        DragonflyNode.__init__(self, pushQ)
        self.history = []
        
        self.server_socket = self.makeSocket()
        self.server_socket.bind(self.address)
        self.server_socket.listen(1)
        self.other = None
        self.buf = ''

        self.rules = {}
        self.enabledRules = set()

        self.utterance = []

        getLoop().subscribeTimer(BLOCK_TIME, self)

    def __call__(self):
        if not self.other:
            # we use a timeout so ctrl-c will work
            self.server_socket.settimeout(BLOCK_TIME)
            try:
                #log.info('waiting for connection')
                self.other, addr = self.server_socket.accept()
                log.info('connected')
                self.onConnect()
            except socket.timeout:
                return

        self.retrieveMessages()
        self.heartbeat()

    def cleanup(self):
        DragonflyNode.cleanup(self)
        if self.server_socket is not None:
            self.server_socket.close()

    def loadRule(self, rule):
        if rule.name in self.rules:
            if rule == self.rules[rule.name]:
                #log.info('rule UNchanged: ' + rule.name)
                return
            else:
                log.info('rule changed: ' + rule.name)
        
        log.info('Loading rule: ' + rule.name)
        self.sendMsg(rule.textSerialize())
        self.rules[rule.name] = rule

    def unloadRule(self, rule):
        if rule.name not in self.rules:
            return
        
        log.info('Unloading rule: ' + rule.name)        
        self.sendMsg('unload' + ARG_DELIMETER + rule.name)
        try:
            self.enabledRules.remove(rule)
        except KeyError:
            pass

    def clearAllRules(self):
        log.info('Unloading all rules.')        
        self.sendMsg('unload_all')
        self.rules = {}
        self.enabledRules = set()
        for r in registeredRules().values():
            self.unloadRule(r)

    def sendAllRules(self):
        log.info('Sending all rules.')
        for r in registeredRules().values():
            self.loadRule(r)
        #self.sendMsg("all sent" )

    def commitRuleEnabledness(self):
        log.info("Committing rule enabledness: %s" % [rule.name for rule in self.enabledRules])
        self.sendMsg(ARG_DELIMETER.join(['enable'] + [rule.name for rule in self.enabledRules]))

    def updateRuleEnabledness(self, active):
        oldEnabledNames = [rule.name for rule in self.enabledRules]

        self.enabledRules = set()

        # load anything new that was registered or that changed
        registered = set(registeredRules().values())
        for r in registered:
            self.loadRule(r)

        allRules = set(self.rules.values())

        for l in active:
            self.enabledRules.add(l)

        newEnabledNames = [rule.name for rule in self.enabledRules]

        # We do this by name rather than value because equality depends on the
        # definitions being the same.
        if oldEnabledNames != newEnabledNames:
            self.commitRuleEnabledness()

    def onConnect(self):
        self.clearAllRules()
        self.sendAllRules()
        log.info("Pushing connected event.")
        self.pushQ.put(ConnectedEvent())

    def requestStartupComplete(self):
        self.sendMsg("REQUEST_STARTUP_COMPLETE")

    def onMessage(self, msg):
        log.info("Client msg: [%s]" % msg)
        if msg.startswith("MATCH"):
            self.parseMatchMsg(msg)
        elif msg.startswith("MICSTATE"):
            #log.info("Received mic event: %s" % msg)
            self.pushQ.put(MicrophoneEvent(msg.split(ARG_DELIMETER)[1]))
        elif msg.startswith("STARTUP_COMPLETE"):
            self.pushQ.put(StartupCompleteEvent())
        elif msg.startswith("START_RECOGNITION"):
            self.utterance = []
        elif msg.startswith("STOP_RECOGNITION"):
            if self.utterance:
                self.pushQ.put(WordEvent(' '.join(self.utterance)))
        elif msg == "":
            log.debug("heartbeat")
        elif msg.startswith("ack"):
            log.debug('received ack: ' + msg)
        else:
            log.info("Unknown message type: [%s]" % msg[:min(10, len(msg))])
        self.sendMsg("ack " + msg)

    def parseMatchMsg(self, msg):
        log.info(msg)

        msg = msg.split("MATCH")[1]
        tokens = msg.split(ARG_DELIMETER)
        rule, words = tokens[:2]
        extras = tokens[2:]
        
        extras = [g for g in extras if g != '']
        extras = self.parseExtras(extras)
        extras['rule'] = rule
        extras['words'] = words

        self.onMatch(rule, extras)

    def onMatch(self, rule, extras):
        # todo replace this with MatchEvent
        for g in self.enabledRules:
            if rule in g.mapping:
                try:
                    cb = g.mapping[rule]

                    if isinstance(cb, Repeat):
                        if len(self.history) >= 1:
                            repetitions = extras['n']
                            rule, extras = self.history[-1]
                            for i in range(repetitions):
                                self.onMatch(rule, extras)
                            return
                    else:
                        self.history.append(RuleMatchEvent(rule, extras))

                    log.info('match %s -- %s -- %s' % (g.name, rule, extras['words']))
                    self.utterance.append(extras['words'])
                    cb(extras)
                except Exception as e:
                    # don't want the whole thing to crash just because
                    # of one bad rule
                    exc_type, exc_value, exc_traceback = sys.exc_info()
                    log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))

    def parseExtras(self, extras):
        parsed = {}
        for e in extras:
            e = e.split(KEY_VALUE_SEPARATOR)
            try:
                parsed[e[0]] = int(e[1])
            except ValueError:
                parsed[e[0]] = e[1]
        return parsed
    
    
      

