from protocol import RuleRef, makeHashedRule, RuleType
from EventList import RuleRegisterEvent
from EventLoop import pushEvent

mapping = {
    "abe"   : "a",
    "brov"  : "b",
    "coup"  : "c",
    "dealt" : "d",
    "echo"  : "e",
    "fox"   : "f",
    "golf"  : "g",
    "his"   : "h",
    "ivy"   : "i", 
    "john"  : "j",
    "king"  : "k",
    "lima"  : "l",
    "mike"  : "m",
    "no"    : "n",
    "osh"   : "o", 
    "pig"   : "p",
    "quid"  : "q",
    "robe"  : "r", 
    "shoe"  : "s",
    "tang"  : "t",
    "unk"   : "u",
    "vict"  : "v",
    "web"   : "w", 
    "xray"  : "x",
    "yes"   : "y",
    "zulu"  : "z",
}

AlphaRule = makeHashedRule("AlphaRule", mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(AlphaRule))
    
mapping = {
    "zero"  : "0",
    "one"   : "1",
    "two"   : "2",
    "three" : "3",
    "four"  : "4",
    "five"  : "5",
    "six"   : "6",
    "seven" : "7",
    "eight" : "8",
    "nine"  : "9",
}

DigitRule = makeHashedRule("DigitRule", mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(DigitRule))

mapping = {
    "backtick"    : "backtick",
    "backslash"   : "backslash",
    "equal"       : "equal",
    "slash"       : "slash",
    "exclamation" : "exclamation",
    "question"    : "question",
    "swirl"       : "at",
    "pound"       : "hash",
    "cash"        : "dollar",
    "frak"        : "percent", # short for fraction
    "caret"       : "caret",
    "ampersand"   : "ampersand",
    "glob"        : "asterisk",
    "cool"        : "colon",
    "cusp"        : "semicolon",
    "point"       : "period",
    "arg"         : "comma",
    "squiggle"    : "tilde",
    "soot"        : "squote",
    "quote"       : "dquote",
    "lesser"      : "langle",
    "greater"     : "rangle",
    "lace"        : "lbrace",
    "race"        : "rbrace",
    "lack"        : "lbracket",
    "rack"        : "rbracket",
    "larp"        : "lparen",
    "ralp"        : "rparen",
    "dash"        : "hyphen",
    "band"        : "bar",
    "plus"        : "plus",
    "underscore"  : "underscore",
    "pa"          : "space",
}

_literalMapping = {
    "backtick"    : "`",
    "backslash"   : "\\",
    "equal"       : "=",
    "slash"       : "/",
    "exclamation" : "!",
    "question"    : "?",
    "swirl"       : "@",
    "pound"       : "#",
    "cash"        : "$",
    "frak"        : "%",
    "caret"       : "^",
    "ampersand"   : "&",
    "glob"        : "*",
    "cool"        : ":",
    "cusp"        : ";",
    "point"       : ".",
    "arg"         : ",",
    "squiggle"    : "~",
    "soot"        : "'",
    "quote"       : "\"",
    "lesser"      : "<",
    "greater"     : ">",
    "lace"        : "{",
    "race"        : "}",
    "lack"        : "[",
    "rack"        : "]",
    "larp"        : "(",
    "ralp"        : ")",
    "dash"        : "-",
    "band"        : "|",
    "plus"        : "+",
    "underscore"  : "_",
    "pa"          : " ",
}

SymRule = makeHashedRule("SymRule", mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(SymRule))

# TODO: another rule for all the keys that *don't*
# have a character, so you can request shift+pgdown
# and ctrl+enter

mapping = {
    "(<alpharule> | num <digitrule> | <symrule>)" : ""
}

extras = [
    RuleRef(AlphaRule, "alpharule"),
    RuleRef(DigitRule, "digitrule"),
    RuleRef(SymRule, "symrule"),
    ]

CharRule = makeHashedRule("CharRule", mapping, extras, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(CharRule))

def lookup(i):
    for rule in [AlphaRule.rule, DigitRule.rule]:
        try:
            return rule.mapping[i]
        except KeyError:
            pass
    return _literalMapping[i]


