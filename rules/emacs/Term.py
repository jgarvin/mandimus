from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "line mode" : Key("c-c,c-j"),
    "care mode" : Key("c-c,c-k"),
}

TermRule = makeContextualRule("Term", _mapping, emacsExtras, emacsDefaults)
TermRule.context.addRequirement(IsEmacs)
TermRule.context.addRequirement(ModeRequirement(modes="term-mode"))

