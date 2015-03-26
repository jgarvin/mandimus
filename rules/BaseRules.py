from protocol import RuleRef, makeHashedRule, RuleType
from EventList import RuleRegisterEvent
from EventLoop import pushEvent

_mapping = {
    "abe"   : "a",
    "braid" : "b",
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
                    
AlphaRule = makeHashedRule("AlphaRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(AlphaRule))
    
_mapping = {
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

DigitRule = makeHashedRule("DigitRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(DigitRule))

_mapping = {
    "backtick"   : "backtick",
    "backslash"  : "backslash",
    "equal"      : "equal",
    "slash"      : "slash",
    "bang"       : "exclamation",
    "question"   : "question",
    "swirl"      : "at",
    "pound"      : "hash",
    "cash"       : "dollar",
    "frak"       : "percent", # short for fraction
    "caret"      : "caret",
    "ref"        : "ampersand",
    "glob"       : "asterisk",
    "dots"       : "colon",
    "cusp"       : "semicolon",
    "point"      : "period",
    "arg"        : "comma",
    "squiggle"   : "tilde",
    "soot"       : "squote",
    "quote"      : "dquote",
    "lesser"     : "langle",
    "greater"    : "rangle",
    "lace"       : "lbrace",
    "race"       : "rbrace",
    "lack"       : "lbracket",
    "rack"       : "rbracket",
    "larp"       : "lparen",
    "ralp"       : "rparen",
    "dash"       : "hyphen",
    "band"       : "bar",
    "plus"       : "plus",
    "underscore" : "underscore",
    "pa"         : "space",
}

_literalMapping = {
    "backtick"   : "`",
    "backslash"  : "\\",
    "equal"      : "=",
    "slash"      : "/",
    "bang"       : "!",
    "question"   : "?",
    "swirl"      : "@",
    "pound"      : "#",
    "cash"       : "$",
    "frak"       : "%",
    "caret"      : "^",
    "ref"        : "&",
    "glob"       : "*",
    "dots"       : ":",
    "cusp"       : ";",
    "point"      : ".",
    "arg"        : ",",
    "squiggle"   : "~",
    "soot"       : "'",
    "quote"      : "\"",
    "lesser"     : "<",
    "greater"    : ">",
    "lace"       : "{",
    "race"       : "}",
    "lack"       : "[",
    "rack"       : "]",
    "larp"       : "(",
    "ralp"       : ")",
    "dash"       : "-",
    "band"       : "|",
    "plus"       : "+",
    "underscore" : "_",
    "pa"         : " ",
}

SymRule = makeHashedRule("SymRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(SymRule))

# TODO: another rule for all the keys that *don't*
# have a character, so you can request shift+pgdown
# and ctrl+enter

_mapping = {
    "(<alpharule> | num <digitrule> | <symrule>)" : ""
}

_charExtras = [
    RuleRef(AlphaRule, "alpharule"),
    RuleRef(DigitRule, "digitrule"),
    RuleRef(SymRule, "symrule"),
    ]

CharRule = makeHashedRule("CharRule", _mapping, _charExtras, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(CharRule))

def lookup(charrule, keyNames=False):
    for e in _charExtras:
        if e.name in charrule:
            mapping = e.rule_ref.rule.mapping 
            if e.name == "symrule" and not keyNames:
                mapping = _literalMapping
            return mapping[charrule[e.name]["words"][0]]
        

