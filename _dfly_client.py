import mdlog
mdlog.initLogging("client")

log = mdlog.getLogger(__name__)
log.info("-----------load--------------")

from hotCode import importOrReload, unloadCode, reloadCode, resetImportState
resetImportState()

import dragonfly as dfly
from dragonfly import (
    Grammar, CompoundRule, MappingRule, ActionBase,
    Key, Text, MappingRule, Function, get_engine )
import natlink
import socket
import sys, os, traceback
from functools import partial
from copy import copy
from util import deepEmpty
from collections import namedtuple
import hashlib

import protocol
from protocol import (EnableRulesMsg, LoadRuleMsg, LoadRuleFinishedMsg,
                      HeartbeatMsg, MatchEventMsg, MicStateMsg, RecognitionStateMsg,
                      RequestRulesMsg, WordListMsg, RuleType, parseMessage)

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
    client.sendMicState()
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
        self.client.setRecognitionState('thinking')

    def process_recognition_failure(self):
        self.client.setRecognitionState('failure')

    def process_recognition_other(self, words):
        self.client.setRecognitionState('success')

class ReportingAction(ActionBase):
    """The client never actually executes actions, it just
    informs the server that grammar rules have been matched.
    Then it's up to the server to do whatever it wants."""
    def __init__(self, grammarString, dclient):
        self.dclient = dclient
        self.grammarString = grammarString
        ActionBase.__init__(self)
        
    def _execute(self, data=None):
        self.dclient.onMatch(self.grammarString, data)

    def __str__(self):
        return "ReportingAction," + self.grammarString

# We map hashes of rules we know about from rule refs
# but haven't loaded yet to these. This lets us store
# the grammars depending on a rule so that when that rule
# finally arrives we can finish building the grammar and
# activate it, provided at this point it's still the active
# master grammar. 
class NeedDependency(object): pass
class MissingDependency(Exception): pass

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
        x.update([r for r in self.baseRuleSet])
        self.hash = x.hexdigest()

        # Hashes of rules we depend on but haven't arrived yet.
        # These will be discovered during the dfly grammar building
        # process.
        self.missing = set()
        self.checkDeps(self.fullRullSet) # build self.missing
        self.dflyGrammar = None

    @property
    def fullRullSet(self):
        return self.baseRuleSet + self.dependencyRuleSet

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
            raise MissingDependency("Can't build MasterGrammar, missing hashes: [%s]" % self.missing)

    def checkDeps(self, ruleSet):
        "Recursively check if all deps in ruleSet are satisfied."
        if not ruleSet:
            return True

        newDeps = set()
        for r in ruleSet:
            if self.checkDep(r):
                rule = self.ruleCache[r] # HashedRule
                rule = rule.rule
                for e in rule.extras:
                    if hasattr(e, "rule_ref"):
                        newDeps.add(e.rule_ref)
                        
        self.dependencyRuleSet.update(newDeps)
        self.checkDeps(newDeps)

    def ready(self):
        return len(self.missing) == 0

    def build(self):
        self.checkDeps(self.fullRullSet)
        self.checkMissing()

        # from here on we assume all deps are present all the way down
        seriesGroups = {}
        terminal = {}
        independent = set()

        allRules = set()

        # Merge series and terminal rules, set independent rules aside
        self.fullName = []
        for r in self.fullRullSet:
            rule = self.ruleCache[r].rule
            if rule.ruleType == RuleType.SERIES:
                if rule.seriesMergeGroup not in seriesGroups:
                    seriesGroups[rule.seriesMergeGroup] = {}
                x = seriesGroups[rule.seriesMergeGroup]
            elif ruleruleType == RuleType.TERMINAL:
                x = terminal
            elif rule.ruleType == RuleType.INDEPENDENT:
                x = {}
                independent.add(x)

            if "mapping" not in x:
                x["mapping"] = {}
            if "extras" not in x:
                x["extras"] = set()
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
            x["extras"].update(rule.extras)
            x["defaults"].update(rule.defaults.items())
            x["hash"].add(rule.hash)

            self.fullName.append(x["name"])

            # allRules will contain all the rules we have left
            # *after* merging. So only one series rule per merge
            # group and only one terminal rule.
            allRules.add(x)

        self.fullName = ",".join(self.fullName)

        # We really should be doing a topological sort, but this
        # isn't a frequent operation so this inefficiency should
        # be OK. Keep trying to link deps until they're all good.
        allRules = list(allRules)
        while allRules:
            x = allRules.pop()
            if not self.cleanupProtoRule(x):
                allRules.append(x)
                continue
            self.buildConcreteRule(x)

        self.buildFinalMergedRule()

    def buildFinalMergedRule(self):
        extras = []
        seriesRefNames = []
        for i, r in enumerate(self.seriesRules):
            name = "s" + str(i)
            seriesRefNames.append(name)
            ref = dfly.RuleRef(self.concreteRules[r], name)
            extras.append(ref)
        seriesPart = "[" + " | ".join([("<" + r + ">") for r in seriesRefNames]) + "]"

        extras.append(dfly.RuleRef(self.concreteRules[self.terminatorRule], "terminator"))
        terminatorPart = "[<terminator>]"

        masterPhrase = seriesPart + " " + terminatorPart
        mapping = {
            masterPhrase : ReportingAction(masterPhrase, self.client)
        }

        self.finalDflyRule = MappingRule(name=self.fullName, mapping=mapping, extras=extras,
                                         defaults={})

        self.dflyGrammar = Grammar(self.fullName + "Grammar")
        self.dflyGrammar.add_rule(self.finalDflyRule)
        for r in self.independentRules:
            self.dflyGrammar.add_rule(self.concreteRules[r])
        get_engine().set_exclusiveness(grammar, 1)
        self.dflyGrammar.load()

        # rules only enabled via being a dependency need to have disable called
        # on their dragonfly version so that they don't get recognized by themselves,
        # this is a quirk of dragonfly
        notEnabledRules = self.dependencyRuleSet - self.baseRuleSet
        for r in notEnabledRules:
            self.concreteRules[r].disable()

    def activate(self):
        self.dflyGrammar.activate()

    def deactivate(self):
        self.dflyGrammar.deactivate()

    def unload(self):
        if self.dflyGrammar:
            self.dflyGrammar.unload()

    def buildConcreteRule(self, r):
        if r["ruleType"] == RuleType.SERIES:
            t = SeriesMappingRule
        else:
            t = MappingRule

        rule = t(name=r["name"], mapping=r["mapping"], extras=r["extras"],
                 defaults=r["defaults"])

        self.concreteRules[r["hash"]] = rule

        if r["ruleType"] == RuleType.SERIES:
            self.seriesRules.add(r["hash"])
        elif r["ruleType"] == RuleType.TERMINAL:
            self.terminatorRule = r["hash"]
        elif r["ruleType"] == RuleType.INDEPENDENT:
            self.independentRules.add(r["hash"])
        else:
            assert False

    def cleanupProtoRule(self, r):
        if len(r["hash"]) == 1:
            r["hash"] = r["hash"][0]
        else:
            hashes = sorted(list(r.hash))
            x = hashlib.sha256()
            x.update("".join([h for h in hashes]))
            r["hash"] = x.hexdigest()

        for k in r["mapping"]:
            r["mapping"][k] = ReportingAction(k, self.client)

        newExtras = []
        for e in r["extras"]:
            if isinstance(e, protocol.Integer):
                newExtras.append(dfly.Integer(e.name, e.min, e.max))
            elif isinstance(e, protocol.Dictation):
                newExtras.append(dfly.Dictation(e.name))
            elif isinstance(e, protocol.Repetition):
                if e.rule_ref in self.concreteRules:
                    newExtras.append(dfly.Repetition(self.concreteRules[e.rule_ref],
                                                     e.min, e.max, e.name))
                else:
                    return False
            elif isinstance(e, protocol.RuleRef):
                if e.rule_ref in self.concreteRules:
                    newExtras.append(dfly.RuleRef(self.concreteRules[e.rule_ref], e.name))
                else:
                    return False
            elif isinstance(e, protocol.ListRef):
                newExtras.append(dfly.ListRef(e.name, e.list))
            else:
                raise Exception("Unknown extra type: [%s]" % e)

        r["extras"] = newExtras
        return True

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
        self.recognitionState = "success"

        self.globalRule = GlobalRules(name="GlobalRules")
        self.globalRuleGrammar = FailureReportingGrammar(self.globalRule.name)
        self.globalRuleGrammar.setClient(self)
        self.globalRuleGrammar.add_rule(self.globalRule)
        self.globalRuleGrammar.load()
        get_engine().set_exclusiveness(self.globalRuleGrammar, 1)

    def dumpOther(self):
        # on disconnect unload all the rules
        self.pendingRules = {}
        self.lastMicState = None
        DragonflyNode.dumpOther(self)

    def _eventLoop(self):
        try:
            self.eventLoop()
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            self.cleanup()
            #raise

    def eventLoop(self):
        if self.other is None and natlink.getMicState() == 'on':
            self.other = self.makeSocket()
            self.other.settimeout(0.05)
            try:
                #self.other.connect(("10.0.0.2", 23133))
                self.other.connect(("192.168.56.1", 23133))
                log.info('connected')
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
        self.sendMicState()
            
    def setRecognitionState(self, state):
        self.recognitionState = state

        micOn = (natlink.getMicState() == "on")
        if self.recognitionState == "thinking" and micOn:
            pass
            #self.sendMsg("START_RECOGNITION")
        elif self.recognitionState in ["failure", "success"] and micOn:
            pass
            #self.sendMsg("STOP_RECOGNITION")

        self.sendMicState()

    def sendMicState(self):
        pass
        # if not self.other:
        #     return
        
        # micState = natlink.getMicState()
        # if micState == "on":
        #     micState = self.recognitionState
        
        # if self.lastMicState is None or micState != self.lastMicState:
        #     self.lastMicState = micState
        #     self.sendMsg("MICSTATE" + ARG_DELIMETER + self.lastMicState)

    def onMessage(self, json_msg):
        msg = parseMessage(json_msg)
        try:
            if isinstance(msg, EnableRulesMsg):
                self.onEnableRulesMsg(msg)
            elif isinstance(msg, HeartbeatMsg):
                log.debug("Heartbeat")
            elif isinstance(msg, LoadRuleMsg):
                self.onLoadRuleMsg(msg)
            else:
                log.error("Unknown message type, ignoring: [%s]" % json_msg)
                return
        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))

    def onLoadRuleMsg(msg):
        inNeed = set() 
        if msg.hash in self.hashedRules:
            if isinstance(self.hashedRules[msg.hash], NeedsDependency):
                inNeed = self.hashedRules[msg.hash]
            else:
                log.error("Sent already cached rule, ignoring [%s -- %s]" % msg)
                return

        self.hashedRules[msg.hash] = msg.rule
        for grammar in inNeed:
            try:
                grammar.satisfyDependency(msg.hash)
                if self.activeMasterGrammar == grammar.hash:
                    grammar.activate()
            except MissingDependency:
                pass

    def onEnableRulesMsg(self, msg):
        x = hashlib.sha256()
        x.update("".join([r for r in msg.hashes]))
        hash = x.hexdigest()

        grammar = None
        if hash in self.hashedRules:
            assert isinstance(self.hashedRules[hash], MasterGrammar)
            grammar = self.hashedRules[hash]
        else:
            grammar = MasterGrammar(msg.hashes, self, self.hashedRules)
            self.hashedRules[hash] = grammar

        self.activeMasterGrammar = hash

        if grammar.ready():
            try:
                grammar.build()
                grammar.activate()
            except MissingDependency:
                log.info("Can't build grammar yet, still missing deps.")
                return

    def onMatch(self, grammarString, data):
        # if natlink.getMicState() != 'on':
        #     return

        log.info('match -- %s -- %s -- %s' %
                 (unicode(data['_grammar'].name), grammarString, u' '.join(data['_node'].words())))
        log.info('data ' + unicode(data))
        log.info('node ' + u' '.join(data['_node'].words()))
        log.info('rule ' + unicode(data['_rule'].name))
        log.info('grammar ' + unicode(data['_grammar'].name))
        # msg = [MATCH_MSG_START, grammarString, ARG_DELIMETER]
        # msg += [u' '.join(data['_node'].words()), ARG_DELIMETER]
        # if data:
        #     # TODO: we really should be sending the whole node structure
        #     # so we can have more elaborate phrases that change meaning
        #     # based on what was actually said...
        #     for key, value in data.items():
        #         assert not isinstance(value, str) # should be unicode
        #         if isinstance(value, int) or isinstance(value, unicode):
        #             msg += [unicode(key), KEY_VALUE_SEPARATOR, unicode(value), ARG_DELIMETER]
        #         elif isinstance(value, get_engine().DictationContainer):
        #             # log.info(('valuecont',value.words,unicode(value)))
        #             msg += [unicode(key), KEY_VALUE_SEPARATOR, unicode(value.format()), ARG_DELIMETER]
        #self.sendMsg(u''.join(msg))

    def cleanup(self):
        self.globalRuleGrammar.disable()
        self.globalRuleGrammar.unload()
        for hash, grammar in self.hashedRules.items():
            if hasattr(grammar, "unload"):
                grammar.unload()
        self.timer.stop()
        try:
            pass
            #self.sendMsg("MICSTATE" + ARG_DELIMETER + "disconnected")
        except socket.error:
            pass
        DragonflyNode.cleanup(self)

client = DragonflyClient()

### DRAGONSHARE RSYNC
