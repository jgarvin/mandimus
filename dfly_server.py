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
from DragonflyNode import DragonflyNode, ConnectedEvent
from namedtuple import namedtuple
from Actions import Repeat
from EventLoop import getLoop
from rules.Rule import registeredRules
from rules.SeriesMappingRule import SeriesMappingRule, combineSeriesMappingRules
from EventList import MicrophoneEvent, RuleMatchEvent

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

        self.combinedSeries = None

        self.rules = {}
        self.enabledRules = set()

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
                self.unloadRule(self.rules[rule.name])
        
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
        for r in registeredRules().values():
            self.loadRule(r)
        #self.sendMsg("all sent" )

    def enableRule(self, rule):
        if rule in self.enabledRules:
            return
        log.info('Enabling rule: ' + rule.name)        
        self.sendMsg('enable' + ARG_DELIMETER + rule.name)
        self.enabledRules.add(rule)
        
    def disableRule(self, rule):
        if rule not in self.enabledRules:
            return
        log.info('Disabling rule: ' + rule.name)        
        self.sendMsg('disable' + ARG_DELIMETER + rule.name)
        self.enabledRules.remove(rule)

    def updateRuleEnabledness(self, active):
        # load anything new that was registered or that changed
        registered = set(registeredRules().values())
        for r in registered:
            self.loadRule(r)

        allRules = set(self.rules.values())

        combine_series = []
        
        for l in active:
            if isinstance(l, SeriesMappingRule) and l.allowCombining and not l.isMergedSeries:
                combine_series.append(l)
        for s in combine_series:
            active.remove(s)

        # sort so they'll compare reliably
        combine_series.sort(key=lambda x: type(x).__name__)
        combine_series_name = ','.join([type(x).__name__ for x in combine_series])

        if combine_series:
            if combine_series_name in self.rules:
                self.combinedSeries = self.rules[combine_series_name]
                active.add(self.combinedSeries)
            else:
                self.combinedSeries = combineSeriesMappingRules(combine_series)()
                log.info("Series combining: %s" % [type(x).__name__ for x in self.combinedSeries.parts]) 
                self.loadRule(self.combinedSeries)
                active.add(self.combinedSeries)

        inactive = allRules - active        
        for u in inactive:
            self.disableRule(u)

        for l in active:
            self.enableRule(l)

    def onConnect(self):
        self.clearAllRules()
        self.sendAllRules()
        self.pushQ.put(ConnectedEvent())

    def onMessage(self, msg):
        if msg.startswith("MATCH"):
            self.parseMatchMsg(msg)
        elif msg.startswith("MICSTATE"):
            #log.info("Received mic event: %s" % msg)
            self.pushQ.put(MicrophoneEvent(msg.split(ARG_DELIMETER)[1]))
        elif msg == "":
            log.debug("heartbeat")
        elif msg.startswith("ack"):
            log.debug('received ack: ' + msg)
        else:
            log.info("Unknown message type: [%s]" % msg[:min(10, len(msg))])
        self.sendMsg("ack " + msg)

    def parseMatchMsg(self, msg):
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
    
    
      

