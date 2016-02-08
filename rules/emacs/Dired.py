from Actions import Key, Text
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults


_mapping = {
    "dired copy"  : Key("c-c,a-w"),
    "dired cut"   : Key("c-c,c-w"),
    "dired paste" : Key("c-c,c-y")
}

DiredRule = makeContextualRule("Dired", _mapping, emacsExtras, emacsDefaults)
DiredRule.context.addRequirement(IsEmacs)
DiredRule.context.addRequirement(ModeRequirement(modes="dired-mode"))
