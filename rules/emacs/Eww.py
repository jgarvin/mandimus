from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "external"          : Key("ampersand"),
}

EwwRule = makeContextualRule("Eww", _mapping, emacsExtras, emacsDefaults)
EwwRule.context.addRequirement(IsEmacs)
EwwRule.context.addRequirement(ModeRequirement(modes="eww-mode"))

