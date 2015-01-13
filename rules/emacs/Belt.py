from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd, Cmd

@registerRule
class BeltRules(EmacsBase):
    mapping = {
        "near <charrule> [<n>]"  : CharCmd('(md-activate-belt-item "nearest" ?%s)'),
        "name <charrule> [<n>]"  : CharCmd('(md-activate-belt-item "nick" ?%s)'),
        "jerk <charrule> [<n>]"  : CharCmd('(md-activate-belt-item "kill" ?%s)'),
        "beat <charrule> [<n>]"  : CharCmd('(md-activate-belt-item "frequency" ?%s)'),  
        "chong <charrule> [<n>]" : CharCmd('(md-activate-belt-item "recent" ?%s)'),
        "toggle belt"            : Cmd("(md-toggle-belt-mode)"),
    }

    
