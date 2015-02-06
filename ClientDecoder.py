from collections import namedtuple
# from dragonfly import ( Integer, Dictation, RuleRef, Repetition, ListRef, MappingRule )
from rules.Elements import *
import json
from protocol import *

def getRule(ruleHash):
    assert False
    return None

RuleRefPlaceholder = namedtuple("RuleRefPlaceholder", "rule_hash name")
RepetitionPlaceholder = namedtuple("RepetitionPlaceholder", "rule_hash name")

class Decoder(object):
    def __init__(self):
        self.placeholders = []

    def decode(self, dct):
        if "dataType" in dct:
            if dct["dataType"] == "Integer":
                return Integer(dct["name"], dct["min"], dct["max"])
            elif dct["dataType"] == "Dictation":
                return Dictation(dct["name"])
            elif dct["dataType"] == "RuleRef":
                self.placeholders.append(RuleRefPlaceholder(dct["rule_hash", dct["name"]]))
                return self.placeholders[-1]
            elif dct["dataType"] == "Repetition":
                self.placeholders.append(RepetitionPlaceholder(dct["rule_hash", dct["name"]]))
                return self.placeholders[-1]
            elif dct["dataType"] == "ListRef":
                return ListRef(dct["name"])
            elif dct["dataType"] == "HashedRule":
                return HashedRule(rule=dct["rule"], hash=dct["hash"])
        return dct