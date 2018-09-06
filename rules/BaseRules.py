import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key

from protocol import RuleRef, makeHashedRule, RuleType
from EventList import RuleRegisterEvent
from EventLoop import pushEvent

_mapping = {
    "air"   : "a", # not great
    "boy"   : "b",
    "coup"  : "c",
    "dog"   : "d",
    "echo"  : "e",
    "fox"   : "f",
    "golf"  : "g",
    "his"   : "h", # not great
    "igloo"   : "i", # "sky ivy" not reliably recognized
    "john"  : "j",
    "king"  : "k",
    "lima"  : "l",
    "mike"  : "m",
    "knave" : "n",
    "osh"   : "o",
    "pig"   : "p",
    "quid"  : "q",
    "robe"  : "r",
    "shoe"  : "s",
    "toss"  : "t",
    "use"   : "u",
    "vict"  : "v",
    "was"   : "w",
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
    "thorpe"      : "hash",
    "cash"       : "dollar",
    "frak"       : "percent", # short for fraction
    "caret"      : "caret",
    "sand"       : "ampersand",
    "glob"       : "asterisk",
    "dots"       : "colon",
    "cusp"       : "semicolon",
    "point"      : "period",
    "arg"        : "comma",
    "squiggle"   : "tilde",
    "sing"       : "squote",
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
    "pow"        : "space",
}

_literalMapping = {
    "backtick"   : "`",
    "backslash"  : "\\",
    "equal"      : "=",
    "slash"      : "/",
    "bang"       : "!",
    "question"   : "?",
    "swirl"      : "@",
    "thorpe"      : "#",
    "cash"       : "$",
    "frak"       : "%",
    "caret"      : "^",
    "sand"       : "&",
    "glob"       : "*",
    "dots"       : ":",
    "cusp"       : ";",
    "point"      : ".",
    "arg"        : ",",
    "squiggle"   : "~",
    "sing"       : "'",
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
    "pow"        : " ",
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

class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift

    def __call__(self, extras):
        log.info("extras: [%s]" % (extras,))
        words = extras['words']
        log.info('w: ' + str(words))
        keystring = []
        foundModifier = True

        if self.force_shift and "sky" not in words:
            words = ["sky"] + words

        repetitions = extras['i']
        if "control" in words:
            keystring.append('c')
            foundModifier = True
        if "alt" in words:
            keystring.append('a')
            foundModifier = True
        if "sky" in words:
            keystring.append('s')
            foundModifier = True

        if foundModifier:
            keystring.append('-')

        keystring.append(lookup(extras["charrule"], keyNames=True))
        for r in range(repetitions):
            Key(''.join(keystring))()
