from collections import namedtuple

Integer = namedtuple("Integer", "name min max")
Dictation = namedtuple("Dictation", "name")
Repetition = namedtuple("Repetition", "rule_ref min max name")
RuleRef = namedtuple("RuleRef", "rule_ref name")
ListRef = namedtuple("ListRef", "name list")
