import mdlog
log = mdlog.getLogger(__name__)

import socket
import sys
import inspect
import errno
import os
import time
import traceback

from DragonflyNode import DragonflyNode
from namedtuple import namedtuple
from Actions import RepeatPreviousAction
from EventLoop import getLoop
from rules.Rule import registeredRules
from rules.SeriesMappingRule import SeriesMappingRule
from EventList import (MicrophoneEvent, RuleMatchEvent, ConnectedEvent,
                       StartupCompleteEvent, WordEvent, RuleActivateEvent,
                       RuleDeactivateEvent)
from copy import copy
from protocol import (EnableRulesMsg, LoadRuleMsg, MicStateMsg,
                      LoadRuleFinishedMsg, RequestRulesMsg, RecognitionStateMsg,
                      MatchEventMsg, HeartbeatMsg, WordListMsg, makeJSON,
                      parseMessage)

BLOCK_TIME = 0.05

class DragonflyThread(DragonflyNode):
    def __init__(self, address, pushQ):
        self.address = address
        self.pushQ = pushQ
        DragonflyNode.__init__(self, pushQ)
        self.history = []

        # dictionary mapping rule hash -> rule
        self.hashedRules = {}
        # contains HashedRule's
        self.activatedRules = set()
        # contains HashedRule's from last time we committed, so
        # we can check if we actually need to send changes
        self.activatedLastCommit = set()
        self.waitingForLoadConfirmation = set()
        
        self.server_socket = self.makeSocket()
        self.server_socket.bind(self.address)
        self.server_socket.listen(1)
        self.other = None
        self.buf = ''

        self.utterance = []

        getLoop().subscribeTimer(BLOCK_TIME, self)
        getLoop().subscribeEvent(RuleActivateEvent, self.onRuleActivate)
        getLoop().subscribeEvent(RuleDeactivateEvent, self.onRuleDeactivate)

    def onRuleActivate(self, ev):
        if ev.hash not in self.hashedRules:
            log.info("Adding new hashed rule %s" % ev.rule)
            self.hashedRules[ev.rule.hash] = ev.rule.rule

        if ev.rule in self.activatedRules:
            log.info("Requested to activate already activated rule (%s), ignoring." % ev.rule)
            self.activatedRules.add(ev.rule)

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

    def loadRule(self, hash):
        if hash not in self.hashedRules:
            log.error("Client requested rule we don't have! Hash: %s" % hash)
            return
        
        log.info("Loading rule: %s" % self.hashedRules[hash])
        self.sendMsg(makeJSON(LoadRuleMsg(self.hashedRules[hash], hash)))
        self.waitingForLoadConfirmation.add(hash)
        self.pushQ.put(LoadingRulesEvent(True))

    def onLoadFinished(self, hash):
        self.waitingForLoadConfirmation.remove(hash)
        if not self.waitingForLoadConfirmation:
            self.pushQ.put(LoadingRulesEvent(False))

    def commitRuleEnabledness(self):
        if self.activatedRules - self.activatedLastCommit == set():
            return
        self.activatedLastCommit = copy(self.activatedRules)
        log.info("Committing rule activations: %s" % [rule.name for rule in self.activatedRules])
        self.sendMsg(makeJSON(EnableRulesMsg([r.hash for r in self.activatedRules])))

    def onConnect(self):
        self.waitingForLoadConfirmation = set()
        self.pushQ.put(LoadingRulesEvent(False))
        self.activatedLastCommit = set()
        self.commitRuleEnabledness()
        log.info("Pushing connected event.")
        self.pushQ.put(ConnectedEvent())

    def onMessage(self, json_msg):
        log.info("Client msg: [%s]" % json_msg)

        msg = parseMessage(json_msg)
        if isinstance(msg, HeartbeatMsg):
            log.debug("")
        elif isinstance(msg, LoadRuleFinishedMsg):
            self.onLoadFinished(msg)
        elif isinstance(msg, MatchEventMsg):
            self.onMatch(msg)
        elif isinstance(msg, MicStateMsg):
            self.pushQ.put(MicrophoneEvent(msg.state))
        elif isinstance(msg, RecognitionStateMsg):
            if msg.state == "start":
                self.utterance = []
            elif msg.state == "stop":
                if self.utterance:
                    self.pushQ.put(WordEvent(' '.join(self.utterance)))
            else:
                log.error("Unknown recognition state [%s] ignoring message: [%s]" % json_msg)
                return
        elif isinstance(msg, RequestRulesMsg):
            for hash in msg.hashes:
                self.loadRule(hash)
        else:
            log.error("Unknown message type, ignoring: [%s]" % json_msg)
            return
        
        elif msg.startswith("STARTUP_COMPLETE"):
            self.pushQ.put(StartupCompleteEvent())

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

                    if isinstance(cb, RepeatPreviousAction):
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
    
    
      

