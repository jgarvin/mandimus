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
importOrReload("SeriesMappingRule", "SeriesMappingRule")
importOrReload("DragonflyNode", "DragonflyNode")

def reloadClient():
    client.cleanup()
    unloadCode()
    from hotcode import reloadCode
    reloadCode()

class NeedsDependency(Exception): pass

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
        self.grammars = {}
        self.pendingRules = []

        self.globalRule = GlobalRules() 

        self.addRule(self.globalRule, "GlobalRules")
        #self.makeRuleGrammar(self.globalRule, "GlobalRules")
        self.enableRule(self.globalRule)

        self.msgTypeHandlers = {}

        self.lastMicState = None

    def addRule(self, rule, name, flags=[]):
        log.info('adding rule: %s %s' % (rule.name, name))

        if name in self.rules:
            log.info('Already have rule: ' + name)
            return

        self.rules[name] = rule
        rule.disable()

    def addMsgTypeHandler(self, leadingTerm, handler):
        self.msgTypeHandlers[leadingTerm] = handler

    def removeRule(self, name):
        log.info('Removing rule: ' + name)
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
        self.pendingRules = []
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

        if self.pendingRules:
            toParse = copy(self.pendingRules)
            while toParse:
                x = toParse.pop()
                try:
                    self._parseMessage(x)
                    self.pendingRules.remove(x)
                except NeedsDependency:
                    pass

    def sendMicState(self):
        if not self.other:
            return
        
        micState = natlink.getMicState()
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
        args = msg.split(ARG_DELIMETER)
        rule = args[1]
        r = self.getRule(rule)
        if not r:
            log.info("Can't enable %s, can't find it." % rule)
            return
        self.enableRule(r)

    def parseDisableMsg(self, msg):
        args = msg.split(ARG_DELIMETER)
        rule = args[1]
        r = self.getRule(rule)
        if not r:
            log.info("Can't disable %s, can't find it." % rule)
            return
        self.disableRule(r)

    def enableRule(self, rule):
        log.info("Enabling %s" % rule.name)
        if rule.name not in self.grammars:
            self.makeRuleGrammar(rule, rule.name)
        if not self.rules[rule.name].enabled:
            #log.info("Enabling for real %s" % rule.name)
            self.rules[rule.name].enable()
        else:
            #log.info("not Enabling for real %s" % rule.name)
            pass

    def disableRule(self, rule):
        log.info("Disabling %s" % rule.name)
        if rule.name not in self.grammars:
            return
        if self.rules[rule.name].enabled:
            #log.info("disabling for real %s" % rule.name)
            self.rules[rule.name].disable()
        else:
            #log.info("not disabling for real %s" % rule.name)
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
            elif msg.startswith("disable"):
                self.parseDisableMsg(msg)
            elif msg.startswith("ack"):
                log.debug('received ack: ' + msg)
            elif msg.startswith("REQUEST_STARTUP_COMPLETE"):
                self.sendMsg("STARTUP_COMPLETE")
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
        except NeedsDependency:
            log.info("Waiting to parse [%s ...], needs dependency" % msg[:min(len(msg), 30)]) 
            self.pendingRules.append(msg)
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
            self.addRule(new_rule, rule_name, flags)
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
                    raise NeedsDependency()
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
        self.sendMsg("MICSTATE" + ARG_DELIMETER + "disconnected")
        self.timer.stop()
        for name, grammar in self.grammars.items():
            grammar.unload()
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
    mapping = {
        "reload client code" : Function(reloadClient),
        "snore" : Function(snore_and_unload),
        }
    extras = []
    defaults = {}

client = DragonflyClient()

### DRAGONSHARE RSYNC
