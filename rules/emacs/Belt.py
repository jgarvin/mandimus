from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "near <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "nearest" %s)'),
    "name <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "nick" %s)'),
    "jerk <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "kill" %s)'),
    "beat <charrule> [<i>]"  : CharCmd('(md-activate-belt-item "frequency" %s)'),  
    "chong <charrule> [<i>]" : CharCmd('(md-activate-belt-item "recent" %s)'),
    "toggle belt"            : Cmd("(md-toggle-belt-mode)"),
}

BeltRule = makeContextualRule("Belt", _mapping, emacsExtras, emacsDefaults)
BeltRule.context.addRequirement(IsEmacs)
