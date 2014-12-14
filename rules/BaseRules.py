from MappingRule import MappingRule
from Rule import registerRule
from Elements import RuleRef

@registerRule
class AlphaRule(MappingRule):
    refOnly = True
    mapping = {
        "abe"     : "a",
        "bravo"   : "b",
        "club"    : "c",
        "delta"   : "d",
        "echo"    : "e",
        "fox"     : "f",
        "golf"    : "g",
        "how"     : "h",
        "india"   : "i",
        "johnny"  : "j",
        "king"    : "k",
        "lima"    : "l",
        "mike"    : "m",
        "nab"     : "n",
        "oscar"   : "o",
        "peter"   : "p",
        "quebec"  : "q",
        "robert"  : "r",
        "sugar"   : "s",
        "tang"    : "t",
        "uncle"   : "u",
        "victor"  : "v",
        "whiskey" : "w",
        "xray"    : "x",
        "yoke"    : "y",
        "zulu"    : "z",
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
        "percent"     : "percent",
        "caret"       : "caret",
        "ampersand"   : "ampersand",
        "star"        : "asterisk",
        "cool"        : "colon",
        "cusp"        : "semicolon",
        "point"       : "period",
        "arg"         : "comma",
        "tilde"       : "tilde",
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
        "bar"         : "bar",
        "plus"        : "plus",
        "underscore"  : "underscore",
        "pa"          : "space",
    }

    literalMapping = {
        "backtick"    : "``",
        "backslash"   : "\\",
        "equal"       : "=",
        "slash"       : "/",
        "exclamation" : "!",
        "question"    : "?",
        "at"          : "@",
        "pound"       : "#",
        "cash"        : "$",
        "percent"     : "%",
        "caret"       : "^",
        "ampersand"   : "&",
        "star"        : "*",
        "cool"        : ":",
        "cusp"        : ";",
        "dot"         : ".",
        "arg"         : ",",
        "tilde"       : "~",
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
        "bar"         : "|",
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


