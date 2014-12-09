import mdlog
mdlog.initLogging("client")

log = mdlog.getLogger(__name__)
log.info("-----------load--------------")

from hotCode import importOrReload, unloadCode, reloadCode, resetImportState
resetImportState()

from dragonfly import (
    Grammar, CompoundRule, MappingRule, ActionBase,
    Key, Text, Integer, Dictation, RuleRef, Repetition, MappingRule,
    Function )
from dragonfly import get_engine
import natlink
import socket
import sys, os, traceback
from functools import partial
from copy import copy
from util import deepEmpty

#log.setLevel(0)

# Without this stderr won't go to the Natlink
# message window.
#logging.basicConfig()

# import logging
# logging.basicConfig()
# #logging.basicConfig(filename="E:\\log.txt", filemode='w')
# log = logging.getLogger(__name__)

importOrReload("dfly_parser", "parseMessages", "MESSAGE_TERMINATOR", "ARG_DELIMETER",
               "MATCH_MSG_START", "KEY_VALUE_SEPARATOR")
importOrReload("SeriesMappingRule", "SeriesMappingRule", "combineSeriesMappingRules")
importOrReload("DragonflyNode", "DragonflyNode")

def reloadClient():
    client.cleanup()
    unloadCode()
    from hotcode import reloadCode
    reloadCode()

class NeedsDependency(Exception): pass

class FailureReportingGrammar(Grammar):
    def setClient(self, client):
        self.client = client

    def _process_begin(self, executable, title, handle):
        self.client.sendMsg("START_RECOGNITION")
        self.client.setRecognitionState('thinking')

    def process_recognition_failure(self):
        self.client.setRecognitionState('failure')
        self.client.sendMsg("STOP_RECOGNITION")

    def process_recognition_other(self, words):
        self.client.sendMsg("STOP_RECOGNITION")

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

class DragonflyClient(DragonflyNode):
    def __init__(self):
        # Natlink doesn't provide a way to poll files or sockets,
        # and it runs in the same thread as Dragon itself so we can't
        # block, so we run on a periodic timer.
        DragonflyNode.__init__(self)
        self.timer = get_engine().create_timer(self._eventLoop, 1)
        self.buf = ""

        self.rules = {}
        self.enabledRules = set() # set of names, not including combined series
        self.grammars = {}

        # maps needed rule name to the message that needs it
        self.pendingRules = {}

        self.globalRule = GlobalRules(name="GlobalRules") 

        self.rulesNewThisTick = []

        self.addRule(self.globalRule, "GlobalRules")
        self.resetEnabledRules()
        #self.makeRuleGrammar(self.globalRule, "GlobalRules")

        self.msgTypeHandlers = {}

        self.lastMicState = None

        # We always have one master SeriesMappingRule that is a
        # combination of all the active series rules. This way
        # you can say many commands together without pausing,
        # but still define them in separate files and having
        # separate contexts.
        self.combinedSeries = None

        self.startupCompleteRequested = False

        self.recognitionState = "success"

    def resetEnabledRules(self):
        self.enabledRules = set()
        self.enabledRules.add(self.globalRule.name)

    def addRule(self, rule, name):
        log.info('adding rule: %s %s' % (rule.name, name))

        if name in self.rules:
            log.info("Rule already exists: " + name)
            if self.rules[name].mapping.keys() == rule.mapping.keys():
                log.info("Rule hasn't changed, ignoring.")
                return

            log.info("Removing existing copy of rule.")
            self.removeRule(name, replacing=True)

        self.rules[name] = rule
        rule.disable()
        self.rulesNewThisTick.append(name)

    def addMsgTypeHandler(self, leadingTerm, handler):
        self.msgTypeHandlers[leadingTerm] = handler

    def removeRule(self, name, replacing=False):
        log.info('Removing rule: ' + name)
        if not replacing:
            try:
                self.enabledRules.remove(name)
            except KeyError:
                pass
        try:
            del self.rules[name]
        except KeyError:
            log.info('Rule not found: %s, ignoring' % name)
        try:
            self.grammars[name].unload()
            del self.grammars[name]
        except KeyError:
            log.info('Grammar not found: %s, ignoring' % name)

    def makeRuleGrammar(self, rule, name):
        #log.info("making grammar %s" % rule.name)
        if name == "GlobalRules":
            grammar = FailureReportingGrammar(name)
            grammar.setClient(self)
        else:
            grammar = Grammar(name)
        grammar.add_rule(rule)
        #grammar.disable()
        grammar.load()
        ##grammar.enable()
        get_engine().set_exclusiveness(grammar, 1)
        self.grammars[name] = grammar        

    def dumpOther(self):
        # on disconnect unload all the rules
        self.unloadAllRules()
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
                self.other.connect(("10.0.0.2", 23133))
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

        for name in self.rulesNewThisTick:
            if name in self.pendingRules:
                pendingLst = self.pendingRules[name]
                for msg in pendingLst:
                    while pendingLst:
                        x = pendingLst.pop()
                        try:
                            self._parseMessage(x)
                        except NeedsDependency as e:
                            # may have more deps needed
                            if e.message not in self.pendingRules:
                                self.pendingRules[e.message] = []
                            self.pendingRules[e.message].append(x)
        if self.rulesNewThisTick:
            self.commitRuleEnabledness()
        if deepEmpty(self.pendingRules) and self.startupCompleteRequested:
            self.startupCompleteRequested = False
            self.sendMsg("STARTUP_COMPLETE")
        self.rulesNewThisTick = []
            
    def setRecognitionState(self, state):
        self.recognitionState = state
        self.sendMicState()

    def sendMicState(self):
        if not self.other:
            return
        
        micState = natlink.getMicState()
        if micState == "on":
            micState = self.recognitionState
        
        if self.lastMicState is None or micState != self.lastMicState:
            self.lastMicState = micState
            self.sendMsg("MICSTATE" + ARG_DELIMETER + self.lastMicState)
        #log.info("Sending mic event %s" % natlink.getMicState())
        #self.sendMsg("MICSTATE" + ARG_DELIMETER + natlink.getMicState())

    def unloadAllRules(self):
        if len(self.grammars) > 1:
            log.info('Unloading all rules.')
        for key, val in self.rules.items():
            if key != "GlobalRules":
                self.removeRule(key)
        
    def parseEnableMsg(self, msg):
        log.info("Received enable message: %s" % msg)
        
        args = msg.split(ARG_DELIMETER)
        self.resetEnabledRules()

        rules = args[1:]
        #log.info("rule list from server: %s" % rules)
        for name in rules:
            #log.info("attempting to enable %s" % name)
            rule = self.getRule(name)
            if not rule:
                log.info("Marking %s as enabled, but can't find it yet." % name)
            self.enabledRules.add(name)

        self.commitRuleEnabledness()

    def commitRuleEnabledness(self):
        # load anything new that was registered or that changed
        allRules = set([v for k, v in self.rules.items()])

        active = {self.rules[name] for name in self.enabledRules if name in self.rules}

        #log.info("active: %s" % [i.name for i in active])

        combine_series = []
        
        for l in active:
            allowCombining = "ALLOWCOMBINING" in l.mandimusFlags
            if isinstance(l, SeriesMappingRule) and allowCombining and not l.isMergedSeries:
                #log.info("including in combo: %s" % l.name)
                combine_series.append(l)

        # can't be separately enabled in the series rule and on its own at
        # the same time
        for s in combine_series:
            active.remove(s)

        # sort so they'll compare reliably
        combine_series.sort(key=lambda x: x.name)
        combine_series_name = ','.join([x.name for x in combine_series])

        if combine_series:
            if combine_series_name in self.rules:
                # just make sure the already existing rule stays in the active set
                self.combinedSeries = self.rules[combine_series_name]
            else:
                # build a new combined rule
                self.combinedSeries = combineSeriesMappingRules(combine_series)
                log.info("Series combining: %s" % [x.name for x in self.combinedSeries.parts]) 
                self.addRule(self.combinedSeries, self.combinedSeries.name)
            active.add(self.combinedSeries)

        # note that these 'enabled' checks are for dragon,
        # separate from our own concept of enabledness. We
        # might have a rule 'enabled' as part of a series,
        # but it will be disabled here because the combined
        # series containing it already has it
        inactive = allRules - active        
        for u in inactive:
            if u.name not in self.grammars:
                continue
            if u.enabled:
                #log.info("disabling for real %s" % u.name)
                u.disable()
            else:
                #log.info("not disabling for real %s" % u.name)
                pass

        for l in active:
            if l.name not in self.grammars:
                self.makeRuleGrammar(l, l.name)
            if not l.enabled:
                #log.info("Enabling for real %s" % l.name)
                l.enable()
            else:
                #log.info("not Enabling for real %s" % l.name)
                pass

    def _parseMessage(self, msg):
        try:
            if msg.startswith("MappingRule"):
                self.parseMappingRuleMsg(msg, MappingRule)
            elif msg.startswith("SeriesMappingRule"):
                self.parseMappingRuleMsg(msg, SeriesMappingRule)
            elif msg.startswith("unload_all"):
                self.unloadAllRules()
            elif msg.startswith("unload"):
                self.parseUnloadMsg(msg)
            elif msg.startswith("enable"):
                self.parseEnableMsg(msg)
            elif msg.startswith("ack"):
                log.debug('received ack: ' + msg)
            elif msg.startswith("REQUEST_STARTUP_COMPLETE"):
                self.startupCompleteRequested = True
            elif len(msg) == 0:
                log.debug('received heartbeat')
            else:
                log.info("Received unknown message type!: " + msg)
                log.info('Message length %d' % (len(msg),))
        except Exception as e:
            if isinstance(e, NeedsDependency):
                raise
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))


    def onMessage(self, msg):
        try:
            self._parseMessage(msg)
        except NeedsDependency as e:
            log.info("Waiting to parse [%s ...], needs dependency" % msg[:min(len(msg), 30)]) 
            if e.message not in self.pendingRules:
                self.pendingRules[e.message] = []
            self.pendingRules[e.message].append(msg)
            return False

    def parseUnloadMsg(self, msg):
        allargs = msg.split(ARG_DELIMETER) 
        
        assert allargs[0] == "unload"
        self.removeRule(allargs[1])
        
    def parseMappingRuleMsg(self, msg, mappingCls):
        allargs = msg.split(ARG_DELIMETER)

        typ = allargs[0]
        rule_name = allargs[1]
        idx = 2

        rules = []
        for arg in allargs[idx:]:
            idx += 1
            if arg == "EXTRAS":
                break
            rules.append(arg)
        rules = self.transformMapping(rules)

        extras = []
        for arg in allargs[idx:]:
            idx += 1
            if arg == "DEFAULTS":
                break
            extras.append(arg)
        extras = self.parseExtras(extras)

        defaults = []
        for arg in allargs[idx:]:
            idx += 1
            if arg == "FLAGS":
                break
            defaults.append(arg)
        defaults = self.parseDefaults(defaults)

        flags = []
        for arg in allargs[idx:]:
            if arg:
                flags.append(arg)
            idx += 1

        try:
            new_rule = mappingCls(name=rule_name, mapping=rules, extras=extras, defaults=defaults)
            setattr(new_rule, "mandimusFlags", flags)
            setattr(new_rule, "isMergedSeries", False)
            setattr(new_rule, "mapping", rules)
            setattr(new_rule, "extras", extras)
            setattr(new_rule, "defaults", defaults)
            self.addRule(new_rule, rule_name)
        except SyntaxError:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            log.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
            log.info("Mapping rule %s: " % rule_name)
            log.info(rules)
            log.info("Extras:")
            log.info(extras)
            log.info("Defaults:")
            log.info(defaults)
            return
        
    def getRule(self, ruleName):                
        if ruleName not in self.rules:
            return None
        return self.rules[ruleName]
        
    def parseExtras(self, extras):
        parsed = []
        for e in extras:
            e = e.split()
            if e[0] == "INTEGER":
                parsed.append(Integer(e[1], int(e[2]), int(e[3])))
            elif e[0] == "DICTATION":
                parsed.append(Dictation(e[1]))
            elif e[0] == "REPETITION":
                elementRef = None
                for p in parsed:
                    if p.name == e[1]:
                        elementRef = p
                        break
                if elementRef is None:
                    raise Exception("Can't find element %s referred to by %s" % (e[1], e[4]))        
                parsed.append(Repetition(p, int(e[2]), int(e[3]), name=e[4]))
            elif e[0] == "RULEREF":
                r = self.getRule(e[1])
                if not r:
                    log.info("Missing dependency!: %s" % e[1]) 
                    raise NeedsDependency(e[1])
                parsed.append(RuleRef(rule=r, name=e[2]))
            else:
                raise Exception("Unknown element: %s" % e)
        return parsed
        
    def parseDefaults(self, defaults):
        parsed = {}
        for e in defaults:
            e = e.split(KEY_VALUE_SEPARATOR)
            try:
                parsed[e[0]] = int(e[1])
            except ValueError:
                parsed[e[0]] = e[1]
        return parsed
    
    def onMatch(self, grammarString, data):
        if natlink.getMicState() != 'on':
            return

        self.setRecognitionState('success')

        log.info('match -- %s -- %s -- %s' %
                 (unicode(data['_grammar'].name), grammarString, u' '.join(data['_node'].words())))
        # log.info('data ' + unicode(data))
        # log.info('node ' + u' '.join(data['_node'].words()))
        # log.info('rule ' + unicode(data['_rule'].name))
        #log.info('grammar ' + unicode(data['_grammar'].name))
        msg = [MATCH_MSG_START, grammarString, ARG_DELIMETER]
        msg += [u' '.join(data['_node'].words()), ARG_DELIMETER]
        if data:
            # TODO: we really should be sending the whole node structure
            # so we can have more elaborate phrases that change meaning
            # based on what was actually said...
            for key, value in data.items():
                assert not isinstance(value, str) # should be unicode
                if isinstance(value, int) or isinstance(value, unicode):
                    msg += [unicode(key), KEY_VALUE_SEPARATOR, unicode(value), ARG_DELIMETER]
                elif isinstance(value, get_engine().DictationContainer):
                    # log.info(('valuecont',value.words,unicode(value)))
                    msg += [unicode(key), KEY_VALUE_SEPARATOR, unicode(value.format()), ARG_DELIMETER]
        self.sendMsg(u''.join(msg))

    def cleanup(self):
        for name, grammar in self.grammars.items():
            grammar.unload()
        self.timer.stop()
        try:
            self.sendMsg("MICSTATE" + ARG_DELIMETER + "disconnected")
        except socket.error:
            pass
        DragonflyNode.cleanup(self)

    def transformMapping(self, grammarList):
        """We never perform actions directly, we just send
        back to the client that we have a match."""
        mapping = {}
        for g in grammarList:
            mapping[g] = ReportingAction(g, self)
        return mapping
        

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

client = DragonflyClient()

### DRAGONSHARE RSYNC
