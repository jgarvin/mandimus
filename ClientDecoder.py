from collections import namedtuple
from dragonfly import ( Integer, Dictation, RuleRef, Repetition, ListRef, MappingRule )
import json
from protocol import LoadRuleMsg, msgType

def getRule(ruleHash):
    assert False
    return None

RuleRefPlaceholder = namedtuple("RuleRefPlaceholder", "rule_hash name")
RepetitionPlaceholder = namedtuple("RepetitionPlaceholder", "rule_hash name")

class MdMappingRule(MappingRule):
    def __init__(self, name, mapping, extras, defaults,
                 mdRuleType, seriesMergeGroup):
        MappingRule.__init__(self, name=name, mapping=mapping, extras=extras,
                             defaults=defaults)
        self.mdRuleType = mdRuleType
        self.seriesMergeGroup = seriesMergeGroup

    # todo: storage of hash? we'll have outside this object in
    # the HashedRule but not on this one when we're inside decode...

def decode(dct):
    if "dataType" in dct:
        if dct["dataType"] == "Integer":
            return Integer(dct["name"], dct["min"], dct["max"])
        elif dct["dataType"] == "Dictation":
            return Dictation(dct["name"])
        elif dct["dataType"] == "RuleRef":
            return RuleRefPlaceholder(dct["rule_hash", dct["name"]])
        elif dct["dataType"] == "Repetition":
            return RepetitionPlaceholder(dct["rule_hash", dct["name"]])
        elif dct["dataType"] == "ListRef":
            return ListRef(dct["name"])
        elif dct["dataType"] == "HashedRule":
            # do this construction here or delay it?
            # if mapping rule constructor cares about extras being real we have to wait..
            return MappingRule(mapping=dct["mapping"], extras=dct["extras"], defaults=dct["defaults"])
    return dct
