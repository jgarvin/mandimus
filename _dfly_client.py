print "-----------load--------------"
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

# Without this stderr won't go to the Natlink
# message window.
import logging
# logging.basicConfig(filename="E:\\log.txt", filemode='w+')
logging.basicConfig()

importOrReload("dfly_parser", "parseMessages", "MESSAGE_TERMINATOR", "ARG_DELIMETER",
               "MATCH_MSG_START", "KEY_VALUE_SEPARATOR")
importOrReload("SeriesMappingRule", "SeriesMappingRule")
importOrReload("DragonflyNode", "DragonflyNode")

def reloadClient():
    client.cleanup()
    unloadCode()
    reloadCode()

class NeedsDependency(Exception): pass

class GlobalRules(MappingRule):
    mapping = {
        "reload client code" : Function(reloadClient),
        "snore" : Function(lambda: natlink.setMicState('sleeping')),
        }
    extras = []
    defaults = {}

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

        self.addRule(GlobalRules(), "GlobalRules")

    def addRule(self, rule, name, flags=[]):
        print 'adding rule: %s' % name

        if name in self.rules:
            print 'Already have rule: ' + name
            return

        self.rules[name] = rule
        if "REFONLY" not in flags:
            self.makeRuleGrammar(rule, name)

        toParse = copy(self.pendingRules)
        if self.pendingRules:
            while toParse:
                x = toParse.pop()
                try:
                    self._parseMessage(x)
                    self.pendingRules.remove(x)
                except NeedsDependency:
                    pass

    def removeRule(self, name):
        #print 'Removing rule: ' + name
        try:
            del self.rules[name]
        except KeyError:
            print 'Rule not found: %s, ignoring' % name
        try:
            self.grammars[name].unload()
            del self.grammars[name]
        except KeyError:
            print 'Grammar not found: %s, ignoring' % name

    def makeRuleGrammar(self, rule, name):
        grammar = Grammar(name)
        grammar.add_rule(rule)
        grammar.load()
        get_engine().set_exclusiveness(grammar, 1)
        self.grammars[name] = grammar        

    def dumpOther(self):
        # on disconnect unload all the rules
        for key, val in self.grammars.items():
            if key != "GlobalRules":
                self.removeRule(key)
        DragonflyNode.dumpOther(self)

    def _eventLoop(self):
        try:
            self.eventLoop()
        except Exception as e:
            traceback.print_exc()
            self.cleanup()
            #raise

    def eventLoop(self):
        if self.other is None and natlink.getMicState() == 'on':
            self.other = self.makeSocket()
            self.other.settimeout(0.05)
            try:
                self.other.connect(("10.0.0.2", 23133))
                print 'connected'
            except socket.error as e:
                print 'connect error'
                self.dumpOther()
            except socket.timeout as e:
                print 'connect timeout'
                self.dumpOther()
                return

        self.retrieveMessages()
        self.heartbeat()

    def _parseMessage(self, msg):
        try:
            if msg.startswith("MappingRule"):
                self.parseMappingRuleMsg(msg, MappingRule)
            elif msg.startswith("SeriesMappingRule"):
                self.parseMappingRuleMsg(msg, SeriesMappingRule)
            elif msg.startswith("unload"):
                self.parseUnloadMsg(msg)
            elif msg.startswith("ack"):
                #print 'received ack: ' + msg
                pass
            elif len(msg) == 0:
                #print 'received heartbeat'
                pass
            else:
                print "Received unknown message type!: " + msg
                print 'Message length %d' % (len(msg),)
        except Exception as e:
            if isinstance(e, NeedsDependency):
                raise
            traceback.print_exc()

    def onMessage(self, msg):
        try:
            self._parseMessage(msg)
        except NeedsDependency:
            print "Waiting to parse [%s ...], needs dependency" % msg[:min(len(msg), 30)] 
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
            traceback.print_exc()
            print "Mapping rule %s: " % rule_name
            print rules
            print "Extras:"
            print extras
            print "Defaults:"
            print defaults
            return
        
    def getRule(self, ruleName):                
        if ruleName not in self.rules:
            print "Missing dependency!: %s" % ruleName 
            raise NeedsDependency()
        # we only ever have one 'rule' per grammar
        print 'Found dependency!: %s' % ruleName
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
                parsed.append(Repetition(e[1], int(e[2]), int[e3]))
            elif e[0] == "RULEREF":
                parsed.append(RuleRef(rule=self.getRule(e[1]), name=e[2]))
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

        print 'match ' + grammarString
        print 'data ' + unicode(data)
        print 'node ' + u' '.join(data['_node'].words())
        print 'rule ' + unicode(data['_rule'].name)
        print 'grammar ' + unicode(data['_grammar'].name)
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
                    print 'valuecont',value.words,unicode(value)
                    msg += [unicode(key), KEY_VALUE_SEPARATOR, unicode(value.format()), ARG_DELIMETER]
        self.sendMsg(u''.join(msg))

    def cleanup(self):
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
        
client = DragonflyClient()

def unload():
    client.cleanup()
    # unload_code()
    print "----------unload-------------"
    
### DRAGONSHARE RSYNC
