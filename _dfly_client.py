import mdlog
mdlog.initLogging("client")

log = mdlog.getLogger(__name__)

import logging
# logging.getLogger("grammar").setLevel(10)

log.info("-----------load--------------")

from hotCode import importOrReload, unloadCode, reloadCode, resetImportState
resetImportState()

import dragonfly as dfly
from dragonfly import (
    Grammar, CompoundRule, MappingRule, ActionBase, 
    Key, Text, MappingRule, Function, get_engine, List )
from dragonfly.actions.action_base import BoundAction
from dragonfly.grammar.elements import ElementBase
import natlink
import socket
import sys, os, traceback
from functools import partial
from copy import copy
from util import deepEmpty
from collections import namedtuple
import hashlib, time

import protocol
from protocol import (EnableRulesMsg, LoadRuleMsg, 
                      HeartbeatMsg, MatchEventMsg, MicStateMsg, RecognitionStateMsg,
                      RequestRulesMsg, WordListMsg, RuleType, parseMessage,
                      makeJSON, LoadStateMsg, ClientQuitMsg)

importOrReload("SeriesMappingRule", "SeriesMappingRule")
importOrReload("DragonflyNode", "DragonflyNode")

def reloadClient():
    client.cleanup()
    unloadCode()
    from hotcode import reloadCode
    reloadCode()

def unload():
    global client
    client.cleanup()
    log.info("----------unload-------------")
    mdlog.shutdown()
    # unload_code()

def snore_and_unload():
    natlink.setMicState('sleeping')
    client.setRecognitionState('success')
    client.updateMicState()
    #unload()

class GlobalRules(MappingRule):
    mandimusFlags = []
    isMergedSeries = False
    mapping = {
        "reload client code" : Function(reloadClient),
        "snore" : Function(snore_and_unload),
        }
    extras = []
    defaults = {}

class FailureReportingGrammar(Grammar):
    def setClient(self, client):
        self.client = client

    def _process_begin(self, executable, title, handle):
        try:
            self.client.setRecognitionState('thinking')
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            #self.cleanup()

    def process_recognition_failure(self):
        try:
            self.client.setRecognitionState('failure')
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            #self.cleanup()

    def process_recognition_other(self, words):
        try:
            self.client.setRecognitionState('success')
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            #self.cleanup()

class ReportingAction(ActionBase):
    """The client never actually executes actions, it just
    informs the server that grammar rules have been matched.
    Then it's up to the server to do whatever it wants."""
    def __init__(self, grammarString, dclient, ruleHash):
        self.dclient = dclient
        self.grammarString = grammarString
        self.ruleHash = ruleHash
        ActionBase.__init__(self)
        
    def _execute(self, data=None):
        try:
            self.dclient.onMatch(self.grammarString, data)
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            #self.cleanup()

    def __str__(self):
        return "ReportingAction," + self.grammarString

# We map hashes of rules we know about from rule refs
# but haven't loaded yet to these. This lets us store
# the grammars depending on a rule so that when that rule
# finally arrives we can finish building the grammar and
# activate it, provided at this point it's still the active
# master grammar. 
class NeedDependency(set): pass

class MissingDependency(Exception):
    def __init__(self, hashes):
        self.hashes = hashes

class MasterGrammar(object):
    """A MasterGrammar is built up from a specific set of active rules. They
    synthesize the different rule types into one dragonfly grammar. There is
    only ever one master grammar active at a time."""

    def __init__(self, baseRuleSet, client, ruleCache):
        self.client = client
        self.ruleCache = ruleCache
        
        # Hashes that are directly part of this grammar
        self.baseRuleSet = set(baseRuleSet)
        # Hashes of rules that we discover are dependencies
        # of the base rule set
        self.dependencyRuleSet = set()

        # hash -> dragonfly rule
        self.concreteRules = {}
        # one hash per merge group, hash is of hashes of rules that were merged
        self.seriesRules = set()
        # one hash, hash is of hashes of rules that were merged
        self.terminatorRule = ""
        # one hash per rule, hash is the rule's actual hash
        self.independentRules = set()

        # Rule references are stored as hashes, so rules that
        # contain rule refs already effectively include those
        # rules in their hash, so just hashing the base set is
        # all we need.
        x = hashlib.sha256()
        x.update("".join(sorted([r for r in self.baseRuleSet])))
        self.hash = x.hexdigest()

        # Hashes of rules we depend on but haven't arrived yet.
        # These will be discovered during the dfly grammar building
        # process.
        self.missing = set()
        self.checkDeps(self.fullRullSet) # build self.missing
        self.finalDflyRule = None
        self.dflyGrammar = None

        # word lists are *not* hashed. they are global state the
        # client can update at any time, and the change has to be
        # propogated into the currently active grammar. the client
        # can choose to make them rule specific by making the name
        # be the hash of the rule the word list applies to, but this
        # is only convention and not enforced
        self.concreteWordLists = {}

    @property
    def fullRullSet(self):
        return self.baseRuleSet | self.dependencyRuleSet

    def satisfyDependency(self, r):
        """Marks dependency on hash r as satisfied, and tries to build if no more known
        deps are missing. During the build process new indirect dependencies may still
        be discovered however."""
        assert r in self.missing
        self.missing.remove(r)
        if not self.missing:
            self.build()

    def checkDep(self, r):
        "Checks if dep r is present. Not recursive."
        if r not in self.ruleCache:
            self.ruleCache[r] = NeedDependency()
        if isinstance(self.ruleCache[r], NeedDependency):
            self.ruleCache[r].add(self.hash)
            self.missing.add(r)
            return False
        return True

    def checkMissing(self):
        if self.missing:
            raise MissingDependency(copy(self.missing))

    def checkDeps(self, ruleSet):
        "Recursively check if all deps in ruleSet are satisfied."
        if not ruleSet:
            return True

        newDeps = set()
        for r in ruleSet:
            if self.checkDep(r):
                rule = self.ruleCache[r] # HashedRule
                
                rule = rule.rule
                log.info("rule [%s]" % (rule,))
                for e in rule.extras:
                    if hasattr(e, "rule_ref"):
                        newDeps.add(e.rule_ref)
                        
        self.dependencyRuleSet.update(newDeps)
        self.checkDeps(newDeps)

    def ready(self):
        return len(self.missing) == 0

    def build(self):
        if self.dflyGrammar:
            # already built
            return

        buildStartTime = time.time()

        self.checkMissing()
        self.checkDeps(self.fullRullSet)
        self.checkMissing()

        # from here on we assume all deps are present all the way down
        seriesGroups = {}
        terminal = {}

        allRules = []

        mergeStartTime = time.time()

        # Merge series and terminal rules, set independent rules aside
        self.fullName = []
        for r in self.fullRullSet:
            rule = self.ruleCache[r].rule
            hash = self.ruleCache[r].hash
            if rule.ruleType == RuleType.SERIES:
                if rule.seriesMergeGroup not in seriesGroups:
                    seriesGroups[rule.seriesMergeGroup] = {}
                x = seriesGroups[rule.seriesMergeGroup]
            elif rule.ruleType == RuleType.TERMINAL:
                x = terminal
            elif rule.ruleType == RuleType.INDEPENDENT:
                x = {}

            if "mapping" not in x:
                x["mapping"] = {}
            if "extras" not in x:
                x["extras"] = {}
            if "defaults" not in x:
                x["defaults"] = {}
            if "name" not in x:
                x["name"] = ""
            if "hash" not in x:
                x["hash"] = set()    

            x["ruleType"] = rule.ruleType
            x["seriesMergeGroup"] = rule.seriesMergeGroup
            x["name"] = x["name"] + ("," if x["name"] else "") + rule.name
            x["mapping"].update(rule.mapping.items())
            for e in rule.extras:
                x["extras"][e.name] = e
            x["defaults"].update(rule.defaults.items())
            log.info("Adding hash [%s] to name [%s]" % (hash, x["name"]))
            x["hash"].add(hash)
            x["built"] = False

            # allRules will contain all the rules we have left
            # *after* merging. So only one series rule per merge
            # group and only one terminal rule.
            allRules.append(x)

        mergeEndTime = time.time()
        log.info("Grammar merge time: %ss" % (mergeEndTime - mergeStartTime))

        # We really should be doing a topological sort, but this
        # isn't a frequent operation so this inefficiency should
        # be OK. Keep trying to link deps until they're all good.
        uniqueRules = []
        for r in allRules:
            if r not in uniqueRules:
                uniqueRules.append(r)
                self.fullName.append(r["name"])
        self.fullName = ",".join(self.fullName)
        allRules = uniqueRules

        # collapse the hashes
        for r in allRules:
            assert type(r["hash"]) == set
            assert len(r["hash"]) >= 1
            if r["ruleType"] in (RuleType.SERIES, RuleType.TERMINAL):
                # We generate a composite hash for our new composite rules
                log.info("Multi-hash: [%s]" % r["hash"])
                hashes = sorted(list(r["hash"]))
                x = hashlib.sha256()
                x.update("".join(sorted([h for h in hashes])))
                hash = x.hexdigest()
                log.info("Composite: [%s]" % hash)
            else:
                # We just use the exising hash for a rule if it's not composite
                [hash] = r["hash"]
                log.info("Single hash: [%s]" % r["hash"])
            r["hash"] = hash

        allPrototypes = { i["hash"] : i for i in allRules }

        self.concreteTime = 0
        for k, v in allPrototypes.items():
            if not v["built"]:
                self.cleanupProtoRule(v, allPrototypes)

        log.info("Concrete time: %ss" % (self.concreteTime))

        #log.info("made it out of loop")
        self.buildFinalMergedRule()

        buildEndTime = time.time()
        log.info("Grammar build time: %ss" % (buildEndTime - buildStartTime))

        self.setupFinalDflyGrammar()

    def buildFinalMergedRule(self):
        #log.info("Building final merged rule.")
        if not self.seriesRules and not self.terminatorRule:
            return

        extras = []
        seriesRefNames = []
        for i, r in enumerate(self.seriesRules):
            name = "s" + str(i)
            seriesRefNames.append(name)
            ref = dfly.RuleRef(self.concreteRules[r], name)
            extras.append(ref)
        seriesPart = "[" + " | ".join([("<" + r + ">") for r in seriesRefNames]) + "]"

        terminatorPart = ""
        if self.terminatorRule:
            extras.append(dfly.RuleRef(self.concreteRules[self.terminatorRule], "terminator"))
            terminatorPart = " [<terminator>]"

        masterPhrase = seriesPart + terminatorPart
        mapping = {
            masterPhrase : ReportingAction(masterPhrase, self.client, self.hash)
        }

        log.info("Building master grammar rule with name [%s] mapping [%s] extras [%s] defaults [%s]"
                 % (self.fullName, mapping, extras, {}))
        self.finalDflyRule = MappingRule(name=self.hash, mapping=mapping, extras=extras,
                                         defaults={})

    def setupFinalDflyGrammar(self):
        log.info("Setting up final grammar.")
        
        assert not self.dflyGrammar
        self.dflyGrammar = Grammar(self.fullName + "Grammar")
        if self.finalDflyRule:
            self.dflyGrammar.add_rule(self.finalDflyRule)
        for r in self.independentRules:
            self.dflyGrammar.add_rule(self.concreteRules[r])
        loadStart = time.time()
        self.dflyGrammar.load()
        loadEnd = time.time()
        log.info("Grammar load time: %ss" % (loadEnd - loadStart))
        get_engine().set_exclusiveness(self.dflyGrammar, 1)

        # These should never be recognized on their own, only as part of the
        # master rule, quirk of dragonfly that you have to do this even though
        # they're only pulled in by ruleref.
        for r in self.seriesRules:
            self.concreteRules[r].disable()
        if self.terminatorRule:
            self.concreteRules[self.terminatorRule].disable()

        # independent rules only enabled via being a dependency need to have disable
        # called on their dragonfly version so that they don't get recognized by
        # themselves, same quirk.
        notEnabledRules = self.dependencyRuleSet - self.baseRuleSet
        for r in notEnabledRules:
            self.concreteRules[r].disable()

        # they're enabled by default, don't activate until explicitly made to
        self.dflyGrammar.disable()
        
    def active(self):
        #log.info("active check [%s %s %s]" % (self.dflyGrammar is None, self.dflyGrammar and self.dflyGrammar.loaded, self.dflyGrammar and self.dflyGrammar.enabled))
        return self.dflyGrammar and self.dflyGrammar.loaded and self.dflyGrammar.enabled

    def activate(self):
        self.build()
        self.dflyGrammar.enable()
        log.info("Grammar activated: [%s]" % self.hash)

    def deactivate(self):
        # it's possible we never built successfully
        if self.dflyGrammar:
            self.dflyGrammar.disable()
            log.info("Grammar deactivated: [%s]" % self.hash)

    def unload(self):
        self.deactivate()
        if self.dflyGrammar:
            self.dflyGrammar.unload()

    def buildConcreteRule(self, r):
        # for independent rules we can use the plain
        # name, but it turns out Dragon crashes if your
        # names get too long, so for combined rules we
        # just use the hash as the name... hopefully
        # that's under the limit
        name = r["name"]
        if r["ruleType"] == RuleType.SERIES:
            t = SeriesMappingRule
            name = r["hash"]
        elif r["ruleType"] == RuleType.TERMINAL:
            t = MappingRule
            name = r["hash"]
        else:
            t = MappingRule

        rule = t(name=name, mapping=r["mapping"], extras=r["extras"],
                 defaults=r["defaults"])
        log.info("Building rule with defaults: %s -- %s -- %s" % (r["name"], r["defaults"], r["hash"]))

        self.concreteRules[r["hash"]] = rule

        if r["ruleType"] == RuleType.SERIES:
            self.seriesRules.add(r["hash"])
        elif r["ruleType"] == RuleType.TERMINAL:
            self.terminatorRule = r["hash"]
        elif r["ruleType"] == RuleType.INDEPENDENT:
            self.independentRules.add(r["hash"])
        else:
            assert False

        log.info("done building")

    def cleanupProtoRule(self, r, allPrototypes):
        # have to uniquify in this round about way because lists
        # aren't hashable and we need them for ListRef. Also this
        # cleanup function can be caled more than once if deps are
        # missing so we have to have the check to make idempotent.
        if type(r["extras"]) == dict:
            r["extras"] = r["extras"].values()

        newExtras = []
        for e in r["extras"]:
            if isinstance(e, protocol.Integer):
                newExtras.append(dfly.Integer(e.name, e.min, e.max))
            elif isinstance(e, protocol.Dictation):
                newExtras.append(dfly.Dictation(e.name))
            elif isinstance(e, protocol.Repetition):
                if e.rule_ref not in self.concreteRules:
                    self.cleanupProtoRule(allPrototypes[e.rule_ref], allPrototypes)

                # Dragonfly wants RuleRef to take a RuleRef rather than an actual
                # Rule, so we just make one rather than forcing the server to
                # handle this, see protocol.py comments.
                concrete = self.concreteRules[e.rule_ref]
                log.info("concrete type: [%s]" % type(concrete))
                newExtras.append(dfly.Repetition(dfly.RuleRef(rule=concrete),
                                                 e.min, e.max, e.name))
            elif isinstance(e, protocol.RuleRef):
                if e.rule_ref not in self.concreteRules:
                    self.cleanupProtoRule(allPrototypes[e.rule_ref], allPrototypes)

                newExtras.append(dfly.RuleRef(self.concreteRules[e.rule_ref], e.name))
            elif isinstance(e, protocol.ListRef):
                self.concreteWordLists[e.name] = List(e.name + "ConcreteList")
                self.concreteWordLists[e.name].set(e.words)
                newExtras.append(dfly.ListRef(e.ref_name, self.concreteWordLists[e.name]))
            else:
                raise Exception("Unknown extra type: [%s]" % e)

        r["extras"] = newExtras
        
        concreteStartTime = time.time()
        self.buildConcreteRule(r)
        concreteEndTime = time.time()
        self.concreteTime += (concreteEndTime - concreteStartTime)
        
        r["built"] = True
        return True

    def updateWordList(self, name, words):
        if name not in self.concreteWordLists:
            # log.info("Word list [%s] not in grammar [%s], ignoring" % (name, self.hash))
            return
        
        # We want to check if the value has actually changed because List's
        # set method will blindly tell Dragon to delete its old list and replace
        # it with this one and we don't want to disturb Dragon unless we have to
        # because Dragon is slow.
        if sorted(words) != sorted(self.concreteWordLists[name]):
            log.info("Updating word list [%s] on grammar [%s] with contents [%s]" % (name, self.hash, words))
            # TODO: need to check existing load state, then send a loading message here, then restore
            # old state. This way we can see when word lists are taking a long time to load...
            self.concreteWordLists[name].set(words)

class DragonflyClient(DragonflyNode):
    def __init__(self):
        # Natlink doesn't provide a way to poll files or sockets,
        # and it runs in the same thread as Dragon itself so we can't
        # block, so we run on a periodic timer.
        DragonflyNode.__init__(self)
        self.timer = get_engine().create_timer(self._eventLoop, 1)
        self.buf = ""

        # rule cache, stores types:
        # hash -> HashedRule
        # hash of component rule hashes -> MasterGrammar
        # hash of unloaded rule -> NeedDependency
        self.hashedRules = {}
        self.activatedRules = set() # set of HashRules

        self.activeMasterGrammar = None

        self.lastMicState = None
        self.lastLoadState = None
        self.recognitionState = "success"

        # hashes we've asked for but haven't got a reply for yet
        self.requestedLoads = set()

        self.globalRule = GlobalRules(name="GlobalRules")
        self.globalRuleGrammar = FailureReportingGrammar(self.globalRule.name)
        self.globalRuleGrammar.setClient(self)
        self.globalRuleGrammar.add_rule(self.globalRule)
        self.globalRuleGrammar.load()
        get_engine().set_exclusiveness(self.globalRuleGrammar, 1)

        self.wordLists = {}

    def dumpOther(self):
        DragonflyNode.dumpOther(self)

    def _eventLoop(self):
        try:
            self.eventLoop()
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            #self.cleanup()
            #raise

    def eventLoop(self):
        if self.other is None and natlink.getMicState() == 'on':
            self.other = self.makeSocket()
            self.other.settimeout(0.05)
            try:
                #self.other.connect(("10.0.0.2", 23133))
                log.info("attempting to connect")
                self.other.connect(("192.168.56.1", 23133))
                log.info('connected')

                # Should reset variables here rather than on disconnect,
                # because at startup you've never been disconnected.
                if self.requestedLoads:
                    oldRequests = self.requestedLoads
                    self.requestedLoads = set()
                    self.sendLoadRequest(oldRequests)
                self.lastMicState = None
                self.lastLoadState = None
            except socket.error as e:
                log.info('connect error')
                self.dumpOther()
            except socket.timeout as e:
                log.info('connect timeout')
                self.dumpOther()
                return

        mdlog.flush()
        self.retrieveMessages()
        self.heartbeat()
        self.updateMicState()
        self.updateLoadState()
            
    def setRecognitionState(self, state):
        self.recognitionState = state
        log.info("Recognition state: [%s]" % state)
        self.sendMsg(makeJSON(RecognitionStateMsg(state)))

    def updateMicState(self):
        micState = natlink.getMicState()
        
        # filter redundant change events
        if self.lastMicState is None or micState != self.lastMicState:
            if micState == "off":
                # without this when you say 'snore' you get an utterance
                # that never has an end, because Natlink only gives the
                # processing beginning callback and not the end.
                self.sendMsg(makeJSON(RecognitionStateMsg("success")))

            if self.sendMsg(makeJSON(MicStateMsg(micState))):
                self.lastMicState = micState

    def onMessage(self, json_msg):
        msg = parseMessage(json_msg)
        #log.info("recv type: [%s]" % type(msg))
        try:
            if isinstance(msg, EnableRulesMsg):
                self.onEnableRulesMsg(msg)
            elif isinstance(msg, HeartbeatMsg):
                log.debug("Heartbeat")
            elif isinstance(msg, LoadRuleMsg):
                self.onLoadRuleMsg(msg)
            elif isinstance(msg, WordListMsg):
                self.onWordListMsg(msg)
            else:
                log.error("Unknown message type, ignoring: [%s]" % msg)
                return
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))

    def onWordListMsg(self, msg):
        log.info("Received word list update [%s] -- [%s]" % (msg.name, msg.words))
        self.wordLists[msg.name] = msg.words
        if self.activeMasterGrammar:
            self.updateLoadState('loading')
            self.hashedRules[self.activeMasterGrammar].updateWordList(msg.name, msg.words)
            log.info("Done updating word list")

    def updateLoadState(self, forceState=None):
        if forceState:
            state = forceState
        else:
            log.info("not forcing")
            state = 'loading'
            if self.activeMasterGrammar:
                log.info("grammars chosen")
                g = self.hashedRules[self.activeMasterGrammar]
                log.info("active? %s" % g.active())
                state = 'done' if g.active() else state
        log.info("last load state [%s] new load state [%s]" % (self.lastLoadState, state))
        if self.lastLoadState is None or self.lastLoadState != state: 
            if self.sendMsg(makeJSON(LoadStateMsg(state))):
                self.lastLoadState = state
    
    def sendLoadRequest(self, hashes):
        unrequested = hashes - self.requestedLoads
        if unrequested:
            self.requestedLoads.update(unrequested)
            self.sendMsg(makeJSON(RequestRulesMsg(unrequested)))

    def onLoadRuleMsg(self, msg):
        assert isinstance(msg.rule.rule.ruleType, int)
        assert isinstance(msg.rule.rule.seriesMergeGroup, int)
        assert isinstance(msg.rule.rule.mapping, dict)
        assert isinstance(msg.rule.rule.extras, list)
        assert isinstance(msg.rule.rule.defaults, dict)
        
        if msg.rule.hash in self.requestedLoads:
            self.requestedLoads.remove(msg.rule.hash)

        inNeed = set() 
        if msg.rule.hash in self.hashedRules:
            entry = self.hashedRules[msg.rule.hash]
            if isinstance(entry, NeedDependency):
                inNeed = entry 
            else:
                log.error("Received already cached rule, ignoring [%s of type %s]" % (msg, type(entry)))
                return

        log.info("Inserting [%s] in hashedRules" % (msg.rule,))
        self.hashedRules[msg.rule.hash] = msg.rule
        mapping = self.hashedRules[msg.rule.hash].rule.mapping 
        for k in mapping:
            mapping[k] = ReportingAction(k, self, msg.rule.hash)
        
        noneMissing = True
        for grammarHash in inNeed:
            grammar = self.hashedRules[grammarHash]
            try:
                grammar.satisfyDependency(msg.rule.hash)
            except MissingDependency as e:
                log.info("Can't build grammar yet, still missing deps: [%s]" % e.hashes)
                noneMissing = False
                self.sendLoadRequest(e.hashes)

        # try activating again if not already active, may not be
        # if we had missing deps before but don't now
        if noneMissing:
            self.switchMasterGrammar(self.activeMasterGrammar)

    def onEnableRulesMsg(self, msg):
        log.info("Called onEnableRulesMsg")

        x = hashlib.sha256()
        x.update("".join(sorted([r for r in msg.hashes])))
        hash = x.hexdigest()

        if hash in self.hashedRules:
            assert isinstance(self.hashedRules[hash], MasterGrammar)
        else:
            grammar = MasterGrammar(msg.hashes, self, self.hashedRules)
            self.hashedRules[hash] = grammar

        self.switchMasterGrammar(hash)

    def switchMasterGrammar(self, newHash):
        log.info("told to switch")
        if self.activeMasterGrammar != newHash:
            if self.activeMasterGrammar:
                log.info("deactivating old")
                self.hashedRules[self.activeMasterGrammar].deactivate()
                self.activeMasterGrammar = None

            assert newHash in self.hashedRules
            self.activeMasterGrammar = newHash

        grammar = self.hashedRules[self.activeMasterGrammar]
        log.info("checking if active")
        if not grammar.active():
            try:
                # TODO: test why this doesn't work. Would be nice for
                # seeing true amount of loading...
                log.info("Activating grammar...")
                self.updateLoadState('loading')
                grammar.activate()
                # Word lists may have changed since the last time the
                # grammar was activated, and the lists have to be
                # separately stored on a per grammar basis because
                # dfly/natlink link them that way.
                log.info("Updating word lists...")
                for name, words in self.wordLists.items():
                    grammar.updateWordList(name, words)
                log.info("Sending load state message...")
            except MissingDependency as e:
                log.info("Can't build grammar yet, still missing deps: [%s]" % e.hashes)
                self.sendLoadRequest(e.hashes)

    def getChildrenByActorType(self, node, actorType):
        """Get all nodes below this node with the given name."""
        matches = []
        for child in node.children:
            if isinstance(child.actor, actorType):
                matches.append(child)
            matches.extend(self.getChildrenByActorType(child, actorType))
        return matches

    def pprint(self, node, indent=""):
        if not node.children:
            return "%s%s :: %s -> %s :: %s, actor :: %s" % (indent, node.name, type(node).__name__, node.value(), type(node.value()).__name__, type(node.actor).__name__)
        else:
            return "%s%s :: %s -> %r :: %s, actor :: %s\n" % (indent, node.name, type(node).__name__, node.value(), type(node.value()).__name__, type(node.actor).__name__) \
                + "\n".join([self.pprint(n, indent + "  ") \
                             for n in node.children])

    def addValue(self, d, k, v):
        if k in d:
            # Single values get upgraded to lists if they occur multiple times 
            if type(d[k]) != list:
                d[k] = [d[k], v]
            else:
                d[k].append(v)
        else:
            d.update({ k : v })

    def collectValues(self, node, values=None):
        if values is None:
            values = {}

        refTypes = (dfly.Repetition, dfly.RuleRef)
        isRefType = any([isinstance(node.actor, t) for t in refTypes]) 

        if node.name and isinstance(node.actor, ElementBase) and not isRefType:
            v = node.value()
            w = node.words()
            if isinstance(v, get_engine().DictationContainer):
                # The value vs. words distinction is to help with things
                # like numbers where the value is 3 but the words are "three".
                # For dictated text there is no distinction.
                v = w
            # elif isinstance(v, ActionBase):
            #     # When we have rule refs to other mapping rules, the value ends up
            #     # being the looked up action, which is not what we want. We want to
            #     # know which phrse was triggered.
            #     v = v._action.grammarString
            log.info("extra [%s] value [%s] words [%s] realWords [%s]" % (node.name, v, w, node.words()))
            self.addValue(values, node.name, v)

        if node.name and isRefType:
            repValues = {}
            for n in node.children:
                self.collectValues(n, repValues)
            log.info("node.name [%s] repValues [%s]" % (node.name, repValues))
            repValues.update({ 'words' : node.words() })
            self.addValue(values, node.name, repValues)
            #values.update({ node.name : repValues })
        else:
            for n in node.children:
                self.collectValues(n, values)

        return values

    def getMatchFromNode(self, node):
        extras = {}
        for c in node.children:
            self.collectValues(c, extras)
        words = node.words()
        if isinstance(node.value(), ReportingAction):
            # independent case
            action = node.value()
        else:
            # series/terminator case
            action = node.value()._action
        phrase = action.grammarString
        hash = action.ruleHash

        rule = self.hashedRules[hash].rule
        for e in rule.extras:
            if e.name not in extras and e.name in rule.defaults:
                extras[e.name] = rule.defaults[e.name]
        
        log.info("Extras [%s] Words [%s] Phrase [%s] Hash [%s]" % (extras, words, phrase, hash))
        return MatchEventMsg(hash, phrase, extras, words)
        
    def onMatch(self, grammarString, data):
        if natlink.getMicState() != 'on':
            return

        matches = []
        root = data['_node']
        seriesNode = root.get_child_by_name('series')
        if seriesNode:
            #individualMatches = seriesNode.get_children_by_name('MappingRule')
            individualMatches = self.getChildrenByActorType(seriesNode, MappingRule)
            log.info("Matches: %s" % individualMatches)
            for m in individualMatches:
                # TODO: This is a less than ideal hack. MappingRule type children
                # happen for all matches, even those through RuleRefs, so when saying
                # 'pig' you get a match on the series rule, the char rule, and the alpha
                # rule. We filter the latter 2 by only including enabled things like
                # this.
                # ... probably we shuld really only be looking at ReportingAction nodes?
                if m.actor.enabled:
                    matches.append(self.getMatchFromNode(m))
        
        terminator = root.get_child_by_name('terminator')
        if terminator:
            matches.append(self.getMatchFromNode(terminator))

        if not seriesNode and not terminator:
            # we have a match on a independent rule
            matches.append(self.getMatchFromNode(root))

        # TODO: what about independent activated rules?

        log.info("node tree:")
        log.info(self.pprint(data['_node']))
        log.info("data: %s" % data)

        for m in matches:
            log.info("Sending match: %s" % (m,))
            self.sendMsg(makeJSON(m))

    def cleanup(self):
        self.globalRuleGrammar.disable()
        self.globalRuleGrammar.unload()
        for hash, grammar in self.hashedRules.items():
            if hasattr(grammar, "unload"):
                log.info("Unloading: [%s]" % grammar.hash)
                grammar.unload()
        self.timer.stop()
        try:
            self.sendMsg(makeJSON(ClientQuitMsg()))
        except socket.error:
            pass
        DragonflyNode.cleanup(self)

client = DragonflyClient()

### DRAGONSHARE RSYNC
