import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, RepeatPreviousAction
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Elements import Integer, Dictation, RuleRef, Repetition
from collections import OrderedDict
from listHelpers import dictReplace
from rules.emacs.Emacs import Emacs
from rules.BaseRules import AlphaRule, DigitRule, SymRule, CharRule

import string

class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift
    
    def __call__(self, extras):
        words = extras['words']
        log.info('w: ' + str(words))
        words = words.split()
        keystring = []
        foundModifier = True

        if self.force_shift and "cap" not in words:
            words = ["cap"] + words
        
        numIndex = None
        repetitions = extras['n']
        for i, word in enumerate(words):
            if word in ["control", "alt", "cap"]:
                keystring.append(word[0] if word != "cap" else "s") 
                foundModifier = True
            elif word == "num":
                continue
            else:
                break

        if foundModifier:
            keystring.append('-')
                                  
        finalkey = words[i]
        #log.info('finalkey1: %s' % finalkey)
        finalkey = dictReplace(finalkey, dict(AlphaRule.mapping.items() + DigitRule.mapping.items() + SymRule.mapping.items()))
        #log.info('finalkey2: %s' % finalkey)
        keystring.append(finalkey)
        #log.info("keystring: %s" % keystring)
        for r in range(repetitions):
            Key(''.join(keystring))()

@registerRule
class AlwaysRule(SeriesMappingRule):
    mapping = {
        "command tally"                            : (lambda x: Speak(str(commandTally()))()),
        'rep [<n>]'                                : RepeatPreviousAction(),
        "[control] [alt] [cap] <charrule> [<n>]" : PressKey(),
        'scoot [<n>]'                              : Key("tab:%(n)d"),
        'cap scoot [<n>]'                        : Key("s-tab:%(n)d"),
    }

    charref = RuleRef(CharRule, "charrule")
    
    extras = [
        Integer("n", 2, 20),
        Integer("digit", 0, 10),
        Dictation("text"),
        charref,
        ]
    
    defaults = {
        "n": 1,
        }

    @classmethod
    def activeForWindow(cls, window):
        return True

class TypingBase(MappingRule):
    extras = [
        Dictation("text")
        ]
    
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
    

