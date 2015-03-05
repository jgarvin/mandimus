from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults

# Term mode issues:
# -Can wreck prompt and previous output
# -Completion won't work, because we need to emulate moving cursor and pressing TAB

_mapping = {
    "line mode" : Key("c-c,c-j"),
    "care mode" : Key("c-c,c-k"),
}

TermRule = makeContextualRule("Term", _mapping, emacsExtras, emacsDefaults)
TermRule.context.addRequirement(IsEmacs)
TermRule.context.addRequirement(ModeRequirement(modes="term-mode"))

