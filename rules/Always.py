import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, RepeatPreviousAction
from listHelpers import dictReplace
import rules.BaseRules as BaseRules
from rules.BaseRules import AlphaRule, DigitRule, SymRule, CharRule
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import NotEmacs
from EventList import RuleActivateEvent
import string
from protocol import Integer, Dictation, RuleRef, Repetition, RuleType

class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift
    
    def __call__(self, extras):
        log.info("extras: [%s]" % (extras,))
        words = extras['words']
        log.info('w: ' + str(words))
        keystring = []
        foundModifier = True

        if self.force_shift and "cap" not in words:
            words = ["cap"] + words
        
        repetitions = extras['i']
        if "control" in words:
            keystring.append('c')
            foundModifier = True
        if "alt" in words:
            keystring.append('a')
            foundModifier = True
        if "cap" in words:
            keystring.append('s')
            foundModifier = True
        
        if foundModifier:
            keystring.append('-')

        keystring.append(BaseRules.lookup(extras["charrule"], keyNames=True))
        for r in range(repetitions):
            Key(''.join(keystring))()

_mapping = {
    'rep [<n>]'                                : RepeatPreviousAction(),
    "[control] [alt] [cap] <charrule> [<i>]"   : PressKey(),
    'scoot [<i>]'                              : Key("tab:%(i)d"),
    'cap scoot [<i>]'                          : Key("s-tab:%(i)d"),
}

_extras = [
    Integer("i", 2, 5),
    Integer("n", 2, 20),
    Integer("digit", 0, 10),
    Dictation("text"),
    RuleRef(AlphaRule, "alpharule"),
    RuleRef(CharRule, "charrule"),
]

_defaults = {
    "n": 1,
}

AlwaysRule = makeContextualRule("Always", _mapping, _extras, _defaults)
AlwaysRule.activate()

_extras = [
    Dictation("text")
]
    
_mapping = {
    "type <text>" : Text("%(text)s", False),
}

TypingRule = makeContextualRule("TypingRule", _mapping, _extras, {}, RuleType.TERMINAL)
TypingRule.context.addRequirement(NotEmacs)

_mapping = {
    "camel <text>" : Camel("%(text)s"),
}

CamelRule = makeContextualRule("CamelRule", _mapping, _extras, {}, RuleType.TERMINAL)
CamelRule.context.addRequirement(NotEmacs)

_mapping = {
    "stud <text>" : Camel("%(text)s", True),
}

StudRule = makeContextualRule("StudRule", _mapping, _extras, {}, RuleType.TERMINAL)
StudRule.context.addRequirement(NotEmacs)

_mapping = {
    "hyphen <text>"     : Hyphen("%(text)s"),
    "cap hyphen <text>" : Hyphen("%(text)s", True),
}    

HypenRule = makeContextualRule("HypenRule", _mapping, _extras, {}, RuleType.TERMINAL)
HypenRule.context.addRequirement(NotEmacs)

_mapping = {
    "score <text>"     : Underscore("%(text)s"),
    "cap score <text>" : Underscore("%(text)s", True),
}

UnderscoreRule = makeContextualRule("UnderscoreRule", _mapping, _extras, {}, RuleType.TERMINAL)
UnderscoreRule.context.addRequirement(NotEmacs)
