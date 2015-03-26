from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.VarRequirement import VarRequirement
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "near <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "nearest" %s)'),
    "name <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "nick" %s)'),
    "jerk <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "kill" %s)'),
    "beat <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "frequency" %s)'),  
    "chong <charrule> [<i>]" : CharCmd('(md-activate-belt-item "recent" %s)'),
}

BeltRule = makeContextualRule("Belt", _mapping, emacsExtras, emacsDefaults)
BeltRule.context.addRequirement(VarRequirement("md-belt-mode", "t"))

_mapping = {
    "toggle belt"            : Cmd("(md-toggle-belt-mode)"),
}
BeltToggleRule = makeContextualRule("BeltToggle", _mapping, emacsExtras, emacsDefaults)
BeltToggleRule.context.addRequirement(IsEmacs)
