from collections import namedtuple, OrderedDict
import hashlib
from util import enum
import json
import md5
from copy import copy

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
HashedRule = namedtuple("HashedRule", "rule hash")

EnableRulesMsg = namedtuple("EnableRulesMsg", "hashes")
LoadRuleMsg = namedtuple("LoadRuleMsg", "rule hash")
MicStateMsg = namedtuple("MicStateMsg", "state")
LoadingStateMsg = namedtuple("LoadingStateMsg", "state")
RequestRulesMsg = namedtuple("RequestRulesMsg", "hashes")
RecognitionStateMsg = namedtuple("StartRecognitionMsg", "state")
MatchEventMsg = namedtuple("MatchEventMsg", "rule_ref phrase words extras")
HeartbeatMsg = namedtuple("HeartbeatMsg", "unused")
WordListMsg = namedtuple("WordListMsg", "name list")

def makeJSON(t):
    d = OrderedDict()
    toEncode = t
    if "rule_ref" in toEncode._fields:
        toEncode = t
        toEncode.rule_ref = t.rule_ref.hash
    objDict = toEncode._asdict()
    d["dataType"] = type(toEncode).__name__
    for key, val in objDict.items():
        d[key] = val
    return json.dumps(d)

def makeHashedRule(ruleType, seriesMergeGroup, name, mapping, extras, defaults):
    r = Rule(ruleType, seriesMergeGroup, name, mapping, extras, defaults)
    x = hashlib.sha256()
    x.update(makeJSON(r))
    return HashedRule(r, x.hexdigest())

### DRAGONSHARE RSYNC
