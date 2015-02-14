from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

# TODO: vertical versions of commands? could be handy for navigation to have
# a jump that considers vertical to cost 0 and horizontal to cost normal,
# so you could say whatever mike to go from the closing brace to mapping.

_mapping = {
    "go <charrule> [<n>]"   : CharCmd("(md-move-up-to-char 1 %s)"),
    "come <charrule> [<n>]" : CharCmd("(md-move-up-to-char -1 %s)"),
    "snug [<n>]"            : Cmd("(md-find-indentation-change 1 '>)"),
    "guns [<n>]"            : Cmd("(md-find-indentation-change -1 '>)"),
    "loosen [<n>]"          : Cmd("(md-find-indentation-change 1 '<)"),
    "nesool [<n>]"          : Cmd("(md-find-indentation-change -1 '<)"),
    # these were made obsolete by 'go'
    #"sled [<n>]"           : Cmd("(md-next-whitespace-separated-thing)"),
    #"dels [<n>]"           : Cmd("(md-previous-whitespace-separated-thing)"),
    "sym <charrule> [<n>]"  : CharCmd("(md-move-up-to-symbol-starting-with-char 1 %s)"),
    "miss <charrule> [<n>]" : CharCmd("(md-move-up-to-symbol-starting-with-char -1 %s)"),
    "line <charrule> [<n>]" : CharCmd("(md-find-line-starting-with-char 1 %s)"),
    "Nile <charrule> [<n>]" : CharCmd("(md-find-line-starting-with-char -1 %s)"),
    "store <charrule>"      : CharCmd("(point-to-register %s)"),
    "load <charrule>"       : CharCmd("(jump-to-register %s)"),
    "previous [<n>]"        : Cmd("(md-get-previous-instance-of-symbol)"),
    "next [<n>]"            : Cmd("(md-get-next-instance-of-symbol)"),
}

NavRule = makeContextualRule("Nav", _mapping, emacsExtras, emacsDefaults)
NavRule.context.addRequirement(IsEmacs)


