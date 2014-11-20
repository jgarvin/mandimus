from MappingRule import MappingRule
from Rule import registerRule

# TODO: would be nice for all of these
# to be ace'able
@registerRule
class UnitRule(MappingRule):
    refOnly = True
    mapping = {
        "word"     : "'word",
        "line"     : "'line",
        "ace"      : "'ace",
        "ace line" : "'ace-line",
        "graph"    : "'paragraph",
        "block"    : "'block",
        "larp"     : "'parens",
        "lack"     : "'brackets",
        "lace"     : "'lace",
        "lesser"   : "'angles",
    }


@registerRule
class ActionRule(MappingRule):
    refOnly = True
    mapping = {
        "mark" : "'mark",
        "cut"  : "'cut",
        "copy" : "'copy",
        "dupe" : "'dupe",
        "swap" : "'swap",
    }


