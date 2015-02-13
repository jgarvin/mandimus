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
from EventList import (MicrophoneEvent, RuleMatchEvent, ConnectedEvent,
                       StartupCompleteEvent, WordEvent, RuleActivateEvent,
                       RuleRegisterEvent, RuleDeactivateEvent, LoadingRulesEvent,
                       EventsDrainedEvent)
from copy import copy
from protocol import (EnableRulesMsg, LoadRuleMsg, MicStateMsg,
                      LoadRuleFinishedMsg, RequestRulesMsg, RecognitionStateMsg,
                      MatchEventMsg, HeartbeatMsg, WordListMsg, makeJSON,
                      parseMessage, Rule, HashedRule)

BLOCK_TIME = 0.05

class DragonflyThread(DragonflyNode):
    def __init__(self, address, pushQ):
        self.address = address
        self.pushQ = pushQ
        DragonflyNode.__init__(self, pushQ)
        self.history = []

        # dictionary mapping rule hash -> HashedRule
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
        getLoop().subscribeEvent(RuleRegisterEvent, self.onRuleRegister)
        getLoop().subscribeEvent(RuleDeactivateEvent, self.onRuleDeactivate)
        getLoop().subscribeEvent(EventsDrainedEvent, self.commitRuleEnabledness)

    def onRuleRegister(self, ev):
        if ev.rule.hash not in self.hashedRules:
            log.info("Adding new hashed rule [%s]" % (ev.rule,))
            self.hashedRules[ev.rule.hash] = ev.rule

    def onRuleActivate(self, ev):
        self.onRuleRegister(ev)

        if ev.rule in self.activatedRules:
            log.info("Requested to activate already activated rule [%s], ignoring." % (ev.rule,))
            return
        
        log.info("Activating rule [%s]" % (ev.rule,))
        self.activatedRules.add(ev.rule)

    def onRuleDeactivate(self, ev):
        if ev.rule.hash not in self.hashedRules:
            log.error("Deactivating rule that was never added [%s]" % (ev.rule,))
            return

        if ev.rule not in self.activatedRules:
            log.error("Asking to deactivate already deactivated rule [%s], ignoring." % (ev.rule,))
            return

        log.info("Deactivating rule [%s]" % (ev.rule,))
        self.activatedRules.remove(ev.rule)

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

    def stripActions(self, hash):
        r = self.hashedRules[hash].rule
        # We never send actions to the client, and the hashes are generated
        # without including the actions.
        forSendingMapping = { k : None for k in r.mapping.keys() }
        r = HashedRule(Rule(r.ruleType, r.seriesMergeGroup, r.name, forSendingMapping,
                            r.extras, r.defaults), hash)
        return r

    def loadRule(self, hash):
        if hash not in self.hashedRules:
            log.error("Client requested rule we don't have! Hash: %s" % hash)
            return
        
        log.info("Loading rule: %s" % (self.hashedRules[hash],))
        self.sendMsg(makeJSON(LoadRuleMsg(self.stripActions(hash))))
        self.waitingForLoadConfirmation.add(hash)
        self.pushQ.put(LoadingRulesEvent(True))

    def onLoadFinished(self, hash):
        self.waitingForLoadConfirmation.remove(hash)
        if not self.waitingForLoadConfirmation:
            self.pushQ.put(LoadingRulesEvent(False))

    def commitRuleEnabledness(self, ev=None):
        if self.activatedRules == self.activatedLastCommit:
            return
        self.activatedLastCommit = copy(self.activatedRules)
        log.info("Committing rule activations: %s" % [rule.rule.name for rule in self.activatedRules])
        self.sendMsg(makeJSON(EnableRulesMsg([r.hash for r in self.activatedRules])))

    def onConnect(self):
        self.waitingForLoadConfirmation = set()
        self.pushQ.put(LoadingRulesEvent(False))
        self.activatedLastCommit = set()
        self.commitRuleEnabledness()
        log.info("Pushing connected event.")
        self.pushQ.put(ConnectedEvent())

    def onMessage(self, json_msg):
        log.debug("Client msg: [%s]" % json_msg)

        msg = parseMessage(json_msg)
        if isinstance(msg, HeartbeatMsg):
            log.debug("Heartbeat")
        elif isinstance(msg, LoadRuleFinishedMsg):
            self.onLoadFinished(msg)
        elif isinstance(msg, MatchEventMsg):
            self.onMatch(msg.hash, msg.phrase, msg.extras, msg.words)
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

    def onMatch(self, hash, phrase, extras, words):
        if hash not in self.hashedRules:
            log.error("Received match for unknown hash [%s]" % hash)
            return

        rule = self.hashedRules[hash]
        if rule not in self.activatedRules:
            log.error("Received match for deactivated rule! [%s -- %s]" % (rule.rule.name, hash))
            return

        rule = rule.rule
        if phrase not in rule.mapping:
            log.error("Received match on phrase not in rule! Phrase [%s] Rule [%s -- %s]" (phrase, rule.name, hash))
            return

        extras["words"] = words

        # todo replace this with MatchEvent
        try:
            cb = rule.mapping[phrase]

            if isinstance(cb, RepeatPreviousAction):
                if len(self.history) >= 1:
                    repetitions = extras['n']
                    rule, extras = self.history[-1]
                    for i in range(repetitions):
                        self.onMatch(rule, extras)
                return
            else:
                self.history.append(RuleMatchEvent(rule, extras))

            log.info('match %s -- %s -- %s -- %s -- %s' % (rule.name, phrase, words, extras, hash))
            self.utterance.extend(words)
            cb(extras)
        except Exception as e:
            # don't want the whole thing to crash just because
            # of one bad rule
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
