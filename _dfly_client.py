print "-----------load--------------"

# modified from aenea, taken from:
# https://raw.githubusercontent.com/calmofthestorm/aenea/4b0f91ca82aa994cd4912b17cdb4ae700adc65fe/client/_aenea.py
def unload_code():
    import natlinkmain, sys, os

    def topy(path):
        if path.endswith == ".pyc":
            return path[:-1]

        return path

    # Do not reload anything in these directories or their subdirectories.
    dir_reload_blacklist = set(["core"])

    # TODO: should only care about path ending in Natlink/Natlink/MacroSystem
    macro_dir = "E:\\NatLink\\NatLink\\MacroSystem"

    # Unload all grammars.
    natlinkmain.unloadEverything()

    # Unload all modules in macro_dir except for those in directories on the
    # blacklist.

    for name, module in sys.modules.items():
        if module and hasattr(module, "__file__"):
            # Some builtin modules only have a name so module is None or
            # do not have a __file__ attribute.  We skip these.
            path = module.__file__

            # Convert .pyc paths to .py paths.
            path = topy(path)

            # Do not unimport this module!  This will cause major problems!
            if (path.startswith(macro_dir) and
                not bool(set(path.split(os.path.sep)) & dir_reload_blacklist)
                and path != topy(os.path.abspath(__file__))):

                print "unloading %s" % name
                if hasattr(sys.modules[name], "unload") and callable(sys.modules[name].unload):
                    sys.modules[name].unload()
                
                print "removing %s from cache" % name

                # Remove the module from the cache so that it will be reloaded
                # the next time # that it is imported.  The paths for packages
                # end with __init__.pyc so this # takes care of them as well.
                del sys.modules[name]
                
# now invoke it, making sure that all of the importing we do that follows
# is fresh
unload_code()
    
from dragonfly import (
    Grammar, CompoundRule, MappingRule, ActionBase,
    Key, Text, Integer, Dictation, RuleRef, Repetition, MappingRule )
from dragonfly import get_engine
import natlink
import socket
import sys, os
from functools import partial

# Without this stderr won't go to the Natlink
# message window.
import logging
# logging.basicConfig(filename="E:\\log.txt", filemode='w+')
logging.basicConfig()

from dfly_parser import (parseMessages, MESSAGE_TERMINATOR, ARG_DELIMETER,
                         MATCH_MSG_START)
from SeriesMappingRule import SeriesMappingRule
from DragonflyNode import DragonflyNode

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

class DragonflyClient(DragonflyNode):
    def __init__(self):
        # Natlink doesn't provide a way to poll of files or sockets,
        # and it runs in the same thread as Dragon itself so we can't
        # block, so we run on a periodic timer.
        DragonflyNode.__init__(self)
        self.timer = get_engine().create_timer(self._eventLoop, 1)
        self.testSent = False
        self.buf = ""
        self.grammar = None

    def _eventLoop(self):
        try:
            self.eventLoop()
        except Exception as e:
            print str(e)
            self.cleanup()

    def eventLoop(self):
        if self.other is None:
            self.other = self.makeSocket()
            self.other.settimeout(5)
            try:
                self.other.connect(("10.0.0.2", 23133))
                print 'connected'
            except socket.timeout as e:
                print 'connection timed out, resetting to none'
                self.other = None
                return

        if not self.testSent:
            self.testSent = True
            self.sendMsg("test")
            self.sendMsg("foo")
            
        self.retrieveMessages()
        self.heartbeat()

    def onMessage(self, msg):
        if msg.startswith("MappingRule "):
            self.parseMappingRuleMsg(msg[len("MappingRule "):], MappingRule)
        elif msg.startswith("SeriesMappingRule "):
            self.parseMappingRuleMsg(msg[len("SeriesMappingRule "):], SeriesMappingRule)
        elif msg.startswith("ack"):
            print 'received ack: ' + msg
        elif len(msg) == 0:
            pass
            #print 'received heartbeat'
        else:
            print "Received unknown message type!: " + msg
            print 'Message length %d' % (len(msg),)

    def parseMappingRuleMsg(self, msg, mappingCls):
        grammars, extras = msg.split("EXTRAS")
        extras, defaults = extras.split("DEFAULTS")
        
        omapping = filter(lambda a: a != '', grammars.split(ARG_DELIMETER))
        omapping = self.transformMapping(omapping)
        
        oextras = filter(lambda a: a != '', extras.split(ARG_DELIMETER))
        oextras = self.parseExtras(oextras)
        
        odefaults = filter(lambda a: a != '', defaults.split(ARG_DELIMETER)) 
        odefaults = self.parseDefaults(odefaults)
        
        new_rule = mappingCls(omapping, oextras, odefaults)

        if self.grammar is not None:
            self.grammar.unload()
        self.grammar = Grammar("The grammar")
        self.grammar.add_rule(new_rule)
        self.grammar.load()
        
    def parseExtras(self, extras):
        parsed = []
        # print extras
        for e in extras:
            # print e
            e = e.split()
            if e[0] == "INTEGER":
                parsed.append(Integer(e[1], int(e[2]), int(e[3])))
            elif e[0] == "DICTATION":
                parsed.append(Dictation(e[1]))
        return parsed
        
    def parseDefaults(self, defaults):
        parsed = {}
        for e in defaults:
            e = e.split(':')
            try:
                parsed[e[0]] = int(e[1])
            except ValueError:
                parsed[e[0]] = e[1]
        return parsed
    
    def onMatch(self, grammarString, data):
        # print 'data ' + str(data)
        # print 'node ' + str(' '.join(data['_node'].words()))
        # print 'rule ' + str(data['_rule'].name)
        # print 'grammar ' + str(data['_grammar'].name)
        msg = [MATCH_MSG_START, grammarString, ARG_DELIMETER]
        msg += [' '.join(data['_node'].words()), ARG_DELIMETER]
        if data:
            # TODO: we really should be sending the whole node structure
            # so we can have more elaborate phrases that change meaning
            # based on what was actually said...
            for key, value in data.items():
                if isinstance(value, int) or isinstance(value, str):
                    msg += [str(key), ":", str(value), ARG_DELIMETER]
        self.sendMsg(''.join(msg))

    def cleanup(self):
        DragonflyNode.cleanup(self)
        self.timer.stop()
        if self.grammar:
            self.grammar.unload()

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
    print "----------unload-------------"
    
### DRAGONSHARE RSYNC
