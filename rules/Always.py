import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, RepeatPreviousAction
from listHelpers import dictReplace
#from rules.BaseRules import AlphaRule, DigitRule, SymRule, CharRule
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs, NotEmacs
from EventList import RuleActivateEvent
import string
from protocol import Integer, Dictation, RuleRef, Repetition, RuleType

# class PressKey(object):
#     def __init__(self, force_shift=False):
#         self.force_shift = force_shift
    
#     def __call__(self, extras):
#         words = extras['words']
#         log.info('w: ' + str(words))
#         words = words.split()
#         keystring = []
#         foundModifier = True

#         if self.force_shift and "cap" not in words:
#             words = ["cap"] + words
        
#         numIndex = None
#         repetitions = extras['n']
#         for i, word in enumerate(words):
#             if word in ["control", "alt", "cap"]:
#                 keystring.append(word[0] if word != "cap" else "s") 
#                 foundModifier = True
#             elif word == "num":
#                 continue
#             else:
#                 break

#         if foundModifier:
#             keystring.append('-')
                                  
#         finalkey = words[i]
#         #log.info('finalkey1: %s' % finalkey)
#         finalkey = dictReplace(finalkey, dict(AlphaRule.mapping.items() + DigitRule.mapping.items() + SymRule.mapping.items()))
#         #log.info('finalkey2: %s' % finalkey)
#         keystring.append(finalkey)
#         #log.info("keystring: %s" % keystring)
#         for r in range(repetitions):
#             Key(''.join(keystring))()

mapping = {
    #"command tally"                            : (lambda x: Speak(str(commandTally()))()),
    'rep [<n>]'                                : RepeatPreviousAction(),
    #"[control] [alt] [cap] <charrule> [<n>]"   : PressKey(),
    'scoot [<n>]'                              : Key("tab:%(n)d"),
    'cap scoot [<n>]'                          : Key("s-tab:%(n)d"),
}

# charref = RuleRef(CharRule, "charrule")

extras = [
    Integer("n", 2, 20),
    Integer("digit", 0, 10),
    Dictation("text"),
#    charref,
]

defaults = {
    "n": 1,
}

AlwaysRule = makeContextualRule("Always", mapping, extras, defaults)
AlwaysRule.activate()

extras = [
    Dictation("text")
]
    
mapping = {
    "type <text>" : Text("%(text)s", False),
}

TypingRule = makeContextualRule("TypingRule", mapping, extras, {}, RuleType.TERMINAL)
TypingRule.context.addRequirement(NotEmacs)

mapping = {
    "camel <text>" : Camel("%(text)s"),
}

CamelRule = makeContextualRule("CamelRule", mapping, extras, {}, RuleType.TERMINAL)
CamelRule.context.addRequirement(NotEmacs)

mapping = {
    "stud <text>" : Camel("%(text)s", True),
}

StudRule = makeContextualRule("StudRule", mapping, extras, {}, RuleType.TERMINAL)
StudRule.context.addRequirement(NotEmacs)

mapping = {
    "hyphen <text>"     : Hyphen("%(text)s"),
    "cap hyphen <text>" : Hyphen("%(text)s", True),
}    

HypenRule = makeContextualRule("HypenRule", mapping, extras, {}, RuleType.TERMINAL)
HypenRule.context.addRequirement(NotEmacs)

mapping = {
    "score <text>"     : Underscore("%(text)s"),
    "cap score <text>" : Underscore("%(text)s", True),
}

UnderscoreRule = makeContextualRule("UnderscoreRule", mapping, extras, {}, RuleType.TERMINAL)
UnderscoreRule.context.addRequirement(NotEmacs)
