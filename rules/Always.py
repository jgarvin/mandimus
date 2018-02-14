import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, RepeatPreviousAction
from listHelpers import dictReplace
import rules.BaseRules as BaseRules
from rules.BaseRules import AlphaRule, DigitRule, SymRule, CharRule, PressKey
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import NotEmacs
from EventList import RuleActivateEvent
import string
from protocol import Integer, Dictation, RuleRef, Repetition, RuleType


# class PrintLetter(object):
#     def __call__(self, extras):
#         log.info("Heard letter! %s" % extras['words'])

_mapping = {
    'rep [<n>]'                              : RepeatPreviousAction(),
    "[control] [alt] [sky] <charrule> [<i>]" : PressKey(),
    'scoot [<i>]'                            : Key("tab:%(i)d"),
    'cap scoot [<i>]'                        : Key("s-tab:%(i)d"),
}

_extras = [
    Integer("i", 3, 8),
    Integer("n", 3, 72),
    Integer("digit", 0, 10),
    Dictation("text"),
    RuleRef(AlphaRule, "alpharule"),
    RuleRef(CharRule, "charrule"),
]

_defaults = {
    "n": 1,
    "i": 1,
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

class WriteLetters(object):
    def __call__(self, extras={}):
        for word in extras['letters']['words']:
            Key(AlphaRule.rule.mapping[word])()

_mapping = {
    "spell <letters>" : WriteLetters()
}

_extras = [
    Repetition(AlphaRule, 1, 5, "letters")
]

SpellRule = makeContextualRule("SpellRule", _mapping, _extras, {}, RuleType.TERMINAL)
SpellRule.activate()