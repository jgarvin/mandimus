from MappingRule import MappingRule
from Rule import registerRule

@registerRule
class AlphaRule(MappingRule):
    refOnly = True
    mapping = {
        "alpha"    : "a",
        "bravo"    : "b",
        "charlie"  : "c",
        "delta"    : "d",
        "echo"     : "e",
        "foxtrot"  : "f",
        "golf"     : "g",
        "hotel"    : "h",
        "india"    : "i",
        "juliet"   : "j",
        "kilo"     : "k",
        "lima"     : "l",
        "mike"     : "m",
        "november" : "n",
        "oscar"    : "o",
        "papa"     : "p",
        "quebec"   : "q",
        "romeo"    : "r",
        "sierra"   : "s",
        "tango"    : "t",
        "uniform"  : "u",
        "victor"   : "v",
        "whiskey"  : "w",
        "xray"     : "x",
        "yankee"   : "y",
        "zulu"     : "z",
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
        "at"          : "at",
        "pound"       : "hash",
        "dollar"      : "dollar",
        "cash"        : "dollar",
        "percent"     : "percent",
        "caret"       : "caret",
        "ampersand"   : "ampersand",
        "star"        : "asterisk",
        "cool"        : "colon",
        "cusp"        : "semicolon",
        "period"      : "period",
        "dot"         : "period",
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
    }
