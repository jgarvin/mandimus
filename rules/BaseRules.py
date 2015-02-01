from MappingRule import MappingRule
from Rule import registerRule
from Elements import RuleRef

@registerRule
class AlphaRule(MappingRule):
    refOnly = True
    mapping = {
        "abe"    : "a",
        "brov"   : "b",
        "coup"   : "c",
        "dealt"  : "d",
        "echo"   : "e",
        "fox"    : "f",
        "golf"   : "g",
        "his"    : "h",
        "ivy"    : "i", 
        "john"   : "j",
        "king"   : "k",
        "lima"   : "l",
        "mike"   : "m",
        "no"     : "n",
        "osk"    : "o", 
        "pig"    : "p",
        "quid"   : "q",
        "robert" : "r",
        "shoe"   : "s",
        "tang"   : "t",
        "unk"    : "u",
        "vict"   : "v",
        "whisk"  : "w",
        "xray"   : "x",
        "yes"    : "y",
        "zulu"   : "z",
    }
    
@registerRule
class DigitRule(MappingRule):
    refOnly = True
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

@registerRule
class SymRule(MappingRule):
    refOnly = True
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

    literalMapping = {
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


# TODO: another rule for all the keys that *don't*
# have a character, so you can request shift+pgdown
# and ctrl+enter

@registerRule
class CharRule(MappingRule):
    refOnly = True
    mapping = {
        "(<alpharule> | num <digitrule> | <symrule>)" : ""
    }

    @classmethod
    def lookup(cls, i):
        for rule in [AlphaRule, DigitRule]:
            try:
                return rule.mapping[i]
            except KeyError:
                pass
        return SymRule.literalMapping[i]

    alpharef = RuleRef(AlphaRule, "alpharule")
    digitref = RuleRef(DigitRule, "digitrule")
    symref   = RuleRef(SymRule, "symrule")

    extras = [
        alpharef,
        digitref,
        symref,
        ]


