from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd

@registerRule
class BeltRules(EmacsBase):
    mapping = {
        "near <charrule> [<n>]"  : CharCmd('(md-activate-belt-item "nearest" ?%s)'),
        "jerk <charrule> [<n>]"  : CharCmd('(md-activate-belt-item "kill" ?%s)'),
        "beat <charrule> [<n>]"  : CharCmd('(md-activate-belt-item "frequency" ?%s)'),  
        "chong <charrule> [<n>]" : CharCmd('(md-activate-belt-item "recent" ?%s)'),
    }

    
