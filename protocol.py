from collections import namedtuple, OrderedDict
import hashlib
from util import enum
import json
import md5
from copy import deepcopy
import struct

# So here is how the protocol works. The server decides which rules should be
# enabled. It sends an ENABLE_RULES with a list of md5 hashes of the rules
# it wants enabled. All other rules are implicitly disabled. The client
# receives the message and sends back a REQUEST_RULES for any rules it
# currently doesn't have in its md5 keyed cache. This means that rules don't
# get sent to the client until the first time they're used, and the server
# doesn't have to track which rules it has or hasn't sent the client.
#
# When the client detects mic activity it updates the server with MIC_STATE,
# START_RECOGNITION, and STOP_RECOGNITION messages.
#
# When a match occurs the client sends MATCH_EVENT w/ the md5 of the matching
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
# INDEPENDENT means to make a separate grammar just for this rule.
# This is ideal for constantly changing dynamic grammars, where you
# want to be able to individually enable/disable phrases. They have
# to appear as an isolated utterance however.
RuleType = enum(SERIES=0, TERMINAL=1, INDEPENDENT=2)

# type is rule type
# seriesMergeGroup lets you have mutually exclusive series rules, to
#   avoid for example having window commands mixed with editing.
# mapping, extras, default have their normal dragonfly MappingRule meanings
Rule = namedtuple("Rule", "ruleType seriesMergeGroup name mapping extras defaults")
HashedRuleBase = namedtuple("HashedRule", "rule hash")

class HashedRule(HashedRuleBase):
    def __eq__(self, other):
        return self.hash == other.hash
    def __neq__(self, other):
        return self.hash != other.hash
    def __hash__(self):
        return hash(self.hash)

msgTypes = set()

def _newMsgType(name, members):
    newType = namedtuple(name, members)
    global msgTypes
    msgTypes.add(newType)
    return newType

EnableRulesMsg = _newMsgType("EnableRulesMsg", "hashes")
HeartbeatMsg = _newMsgType("HeartbeatMsg", "unused")
LoadRuleFinishedMsg = _newMsgType("LoadRuleFinishedMsg", "hash")
LoadRuleMsg = _newMsgType("LoadRuleMsg", "rule hash")
MatchEventMsg = _newMsgType("MatchEventMsg", "hash phrase extras")
MicStateMsg = _newMsgType("MicStateMsg", "state")
RecognitionStateMsg = _newMsgType("RecognitionStateMsg", "state")
RequestRulesMsg = _newMsgType("RequestRulesMsg", "hashes")
WordListMsg = _newMsgType("WordListMsg", "name list")

def makeJSONRepresentable(t):
    toEncode = t

    if hasattr(t, "_fields"):
        d = OrderedDict()
        d["dataType"] = type(t).__name__
        if "rule_ref" in toEncode._fields:
            toEncode = t
            toEncode.rule_ref = t.rule_ref.hash
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

def makeHashedRule(name, mapping, extras, defaults, ruleType=RuleType.SERIES, seriesMergeGroup=0):
    # For the mapping hash we only care about the spoken part of the rule, not the action
    # taken in response.
    mapping = {k : None for k in mapping.keys()}

    # Make copies so we can't accidentally make changes to the inputs that
    # break the hash.
    r = Rule(ruleType, seriesMergeGroup, name,
             deepcopy(mapping), deepcopy(extras), deepcopy(defaults))
    x = hashlib.sha256()
    x.update(makeJSON(r))
    return HashedRule(r, x.hexdigest())

def parseNamedTuple(p, t):
    del p["dataType"]
    return t(**p)

def parseMessage(json_msg, object_hook=None):
    p = json.loads(json_msg, object_hook=object_hook)
    if "dataType" not in p:
        raise ValueError("Missing dataType key in msg: [%s]" % json_msg)
    for t in msgTypes:
        print "comparing %s %s" % (t.__name__, p["dataType"])
        if t.__name__ == p["dataType"]:
            return parseNamedTuple(p, t)
    raise ValueError("Unknown message type [%s] in msg: [%s]" % (p["dataType"], json_msg))

def parseStream(msgs, buf, nextMsgSize):
    """Parses the TCP stream, returning new buf and nextMsgSize state."""
    idx = 0

    del msgs[:]

    while idx < len(buf):
        #print "idx: %d" % idx
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
