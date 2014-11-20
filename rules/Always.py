from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, Repeat
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Elements import Integer, Dictation, RuleRef, Repetition
from collections import OrderedDict
from listHelpers import dictReplace
from rules.emacs.Emacs import Emacs

import string

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
        "tree"  : "3",
        "four"  : "4",
        "fife"  : "5",
        "six"   : "6",
        "seven" : "7",
        "eight" : "8",
        "niner" : "9",
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
        "at"          : "at",
        "pound"       : "hash",
        "dollar"      : "dollar",
        "cash"        : "dollar",
        "percent"     : "percent",
        "caret"       : "caret",
        "ampersand"   : "ampersand",
        "asterisk"    : "asterisk",
        "cool"        : "colon",
        "cusp"        : "semicolon",
        "period"      : "period",
        "dot"         : "period",
        "arg"         : "comma",
        "tilde"       : "tilde",
        "soak"        : "squote",
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
    }


class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift
    
    def __call__(self, extras):
        words = extras['words']
        print 'w: ' + str(words)
        words = words.split()
        #words = words[1:] # cut off num/dir/sym
        keystring = []
        foundModifier = True

        if self.force_shift and "shift" not in words:
            words = ["shift"] + words
        
        for i, word in enumerate(words):
            if word in ["control", "alt", "shift"]:
                keystring.append(word[0])
                foundModifier = True
            else:
                break

        if foundModifier:
            keystring.append('-')
                                  
        finalkey = ' '.join(words[i:]) # everything not a modifier
        finalkey = dictReplace(finalkey, dict(AlphaRule.mapping.items() + DigitRule.mapping.items() + SymRule.mapping.items()))
        keystring.append(finalkey)
        print "keystring: %s" % keystring
        Key(''.join(keystring))()

# TODO: 'blend' all the active grammars including this one together
# so we always have one nice big series mapping rule and commands
# can chain across grammars
@registerRule
class AlwaysRule(SeriesMappingRule):
    mapping = {
        "command tally"                                                   : (lambda x: Speak(str(commandTally()))()),
        "[control] [alt] [shift] (<alpharule> | <digitrule> | <symrule>)" : PressKey(),
        'rep [<n>]'                                                       : Repeat(),
        'tab'                                                             : Key("tab"),
        "num <big>"                                                       : Text("%(big)d"),
    }

    alpharef = RuleRef(AlphaRule, "alpharule")
    digitref = RuleRef(DigitRule, "digitrule")
    symref   = RuleRef(SymRule, "symrule")

    extras = [
        Integer("n", 2, 20),
        Integer("big", 0, 2**14),
        Dictation("text"),
        alpharef,
        digitref,
        symref,
        ]
    
    defaults = {
        "n": 1,
        }

    @classmethod
    def activeForWindow(cls, window):
        return True

class TypingBase(MappingRule):
    extras = [
        Integer("n", 1, 20),
        Dictation("text")
        ]
    
    defaults = {
        "n": 1,
        }    

    @classmethod
    def activeForWindow(cls, window):
        return not Emacs.activeForWindow(window)
    
@registerRule
class TypingRule(TypingBase):
    mapping = {
        "type <text>" : Text("%(text)s", False),
    }

@registerRule
class CamelRule(TypingBase):
    mapping = {
        "camel <text>" : Camel("%(text)s"),
    }

@registerRule
class StudRule(TypingBase):
    mapping = {
        "stud <text>" : Camel("%(text)s", True),
    }

@registerRule
class HyphenRule(TypingBase):
    mapping = {
        "fen <text>"     : Hyphen("%(text)s"),
        "cap fen <text>" : Hyphen("%(text)s", True),
    }    

@registerRule
class UnderscoreRule(TypingBase):
    mapping = {
        "score <text>"     : Underscore("%(text)s"),
        "cap score <text>" : Underscore("%(text)s", True),
    }
    

