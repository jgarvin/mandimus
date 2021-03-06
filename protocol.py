import mdlog
log = mdlog.getLogger(__name__)

from collections import namedtuple, OrderedDict
import hashlib
from util import enum
import json
from copy import copy
import struct

# So here is how the protocol works. The server decides which rules should be
# enabled. It sends an ENABLE_RULES with a list of sha256 hashes of the rules
# it wants enabled. All other rules are implicitly disabled. The client
# receives the message and sends back a REQUEST_RULES for any rules it
# currently doesn't have in its sha256 keyed cache. This means that rules don't
# get sent to the client until the first time they're used, and the server
# doesn't have to track which rules it has or hasn't sent the client.
#
# When the client detects mic activity it updates the server with MIC_STATE,
# START_RECOGNITION, and STOP_RECOGNITION messages.
#
# When a match occurs the client sends MATCH_EVENT w/ the sha256 of the matching
# rule and any associated extras data.
#
# The client and server also regularly send each other heartbeat messages. If
# no other message type has been sent recently. The absence of any messages
# for an extended period tells us the connection is de facto dead even if the
# OS stack thinks it's still alive for some reason (e.g. program is in an
# infinite loop so socket is maintained but nothing can really happen now).

# SERIES means the rule should be merged into the master grammar
# so it can be chained together into a larger utterance.
# TERMINAL means the rule should be merged into the master grammar,
# but only allow it to appear at the end of utterances, typically
# because it contains a dictation element that would get confused
# by commands in series occurring after.
# INDEPENDENT means to not merge this rule into the terminator or
# terminal master rules. Typically this means you will only be
# using the rule by reference. You may also want to put infrequent
# commands in this category to improve recognition since it avoids
# the recognizer from needing to discern when these rules are used
# in combination with others, since they can only be used alone.
RuleType = enum(SERIES=0, TERMINAL=1, INDEPENDENT=2)

dataTypes = set()

def _newDataType(name, members):
    newType = namedtuple(name, members)
    global dataTypes
    dataTypes.add(newType)
    return newType

# type is rule type
# seriesMergeGroup lets you have mutually exclusive series rules, to
#   avoid for example having window commands mixed with editing.
# mapping, extras, default have their normal dragonfly MappingRule meanings
Rule = _newDataType("Rule", "ruleType seriesMergeGroup name mapping extras defaults")
HashedRuleBase = namedtuple("HashedRule", "rule hash")

class HashedRule(HashedRuleBase):
    def __eq__(self, other):
        return self.hash == other.hash
    def __neq__(self, other):
        return self.hash != other.hash
    def __hash__(self):
        return hash(self.hash)
dataTypes.add(HashedRule)

EnableRulesMsg = _newDataType("EnableRulesMsg", "hashes")
HeartbeatMsg = _newDataType("HeartbeatMsg", "unused")
LoadStateMsg = _newDataType("LoadStateMsg", "state")
LoadRuleMsg = _newDataType("LoadRuleMsg", "rule")
MatchEventMsg = _newDataType("MatchEventMsg", "hash phrase extras words")
MicStateMsg = _newDataType("MicStateMsg", "state")
RecognitionStateMsg = _newDataType("RecognitionStateMsg", "state")
RequestRulesMsg = _newDataType("RequestRulesMsg", "hashes")
WordListMsg = _newDataType("WordListMsg", "name words")
ClientQuitMsg = _newDataType("ClientQuitMsg", [])
ToggleVolumeMsg = _newDataType("ToggleVolumeMsg", [])

Integer = _newDataType("Integer", "name min max")
Dictation = _newDataType("Dictation", "name")
# The "rule_ref" attribute is special. On the server side it
# will be an actual rule instance, but when we serialize to send
# to the client we just send a hash. The client handles looking
# up the hash and substituting the rule accordingly. Technically
# in dragonfly Repetition takes a RuleRef object rather than a
# rule directly, but we patch this up in the client to make rules
# easier to write on the server.
Repetition = _newDataType("Repetition", "rule_ref min max name")
RuleRef = _newDataType("RuleRef", "rule_ref name")
# 'name' is used to key the global state on the client
# 'list_name' is the name expected to be used inside rule specs
# we draw this distiction so we don't have to worry about patching
# specs for dynamically generated rules
ListRef = _newDataType("ListRef", "name ref_name words")

def makeJSONRepresentable(t):
    toEncode = t

    if hasattr(t, "_fields"):
        d = OrderedDict()
        d["dataType"] = type(t).__name__
        if "rule_ref" in toEncode._fields:
            toEncode = t._replace(rule_ref=t.rule_ref.hash)
        objDict = toEncode._asdict()
        for key, val in objDict.items():
            d[makeJSONRepresentable(key)] = makeJSONRepresentable(val)
        return d
    elif type(t) == dict:
        d = OrderedDict()
        keys = sorted(t.keys())
        for k in keys:
            d[makeJSONRepresentable(k)] = makeJSONRepresentable(t[k])
        return d
    elif type(t) in (tuple, list):
        return [makeJSONRepresentable(e) for e in t]
    elif type(t) == set:
        l = sorted(list(t))
        return [makeJSONRepresentable(e) for e in l]
    return t

def makeJSON(t):
    d = makeJSONRepresentable(t)
    return json.dumps(d)

def makeHashedRule(name, mapping, extras=[], defaults={}, ruleType=RuleType.SERIES, seriesMergeGroup=0):
    # Make copies so we can't accidentally make changes to the inputs and
    # break the hash.
    mapping = copy(mapping)
    extras = copy(extras)
    defaults = copy(defaults)

    # For the mapping hash we only care about the spoken part of the rule, not the action
    # taken in response.
    forHashMapping = {k : None for k in mapping.keys()}

    # So generate the hash with the actions missing
    r = Rule(ruleType, seriesMergeGroup, name, forHashMapping, extras, defaults)
    x = hashlib.sha256()
    x.update(makeJSON(r))

    # Then remake the rule with the actions included. Up to server to strip them again
    # before sending to client.
    r = Rule(ruleType, seriesMergeGroup, name, mapping, extras, defaults)
    return HashedRule(r, x.hexdigest()[:32])

def parseNamedTuple(p, t):
    del p["dataType"]
    return t(**p)

def asDataType(dct):
    if "dataType" in dct:
        for t in dataTypes:
            if t.__name__ == dct["dataType"]:
                return parseNamedTuple(dct, t)
    return dct

def parseMessage(json_msg):
    p = json.loads(json_msg, object_hook=asDataType)
    #log.info("type: [%s]" % type(p))
    return p

def parseStream(msgs, buf, nextMsgSize):
    """Parses the TCP stream, returning new buf and nextMsgSize state."""
    idx = 0

    del msgs[:]

    while idx < len(buf):
        if nextMsgSize == 0:
            if len(buf) - idx >= 4:
                nextMsgSize = struct.unpack("!I", buf[idx:idx+4])[0]
                idx += 4
            else:
                break

        if len(buf) - idx >= nextMsgSize:
            msgs.append(buf[idx:idx+nextMsgSize])
            idx += nextMsgSize
            nextMsgSize = 0
        else:
            break

    return (msgs, buf[idx:], nextMsgSize)

### DRAGONSHARE RSYNC
