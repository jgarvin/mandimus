import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, Repeat
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Elements import Integer, Dictation, RuleRef, Repetition
from collections import OrderedDict
from listHelpers import dictReplace
from rules.emacs.Emacs import Emacs
from rules.BaseRules import AlphaRule, DigitRule, SymRule

import string

class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift
    
    def __call__(self, extras):
        words = extras['words']
        log.info('w: ' + str(words))
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
        log.info("keystring: %s" % keystring)
        Key(''.join(keystring))()

# TODO: 'blend' all the active grammars including this one together
# so we always have one nice big series mapping rule and commands
# can chain across grammars
@registerRule
class AlwaysRule(SeriesMappingRule):
    mapping = {
        "command tally"                                     : (lambda x: Speak(str(commandTally()))()),
        "[control] [alt] [shift] (<alpharule> | <symrule>)" : PressKey(),
        'rep [<n>]'                                         : Repeat(),
        'tab'                                               : Key("tab"),
    }

    alpharef = RuleRef(AlphaRule, "alpharule")
    symref   = RuleRef(SymRule, "symrule")

    extras = [
        Integer("n", 2, 20),
        Integer("digit", 0, 10),
        Dictation("text"),
        alpharef,
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
        "hyphen <text>"     : Hyphen("%(text)s"),
        "cap hyphen <text>" : Hyphen("%(text)s", True),
    }    

@registerRule
class UnderscoreRule(TypingBase):
    mapping = {
        "score <text>"     : Underscore("%(text)s"),
        "cap score <text>" : Underscore("%(text)s", True),
    }
    

