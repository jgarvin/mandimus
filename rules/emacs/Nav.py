from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults
from Actions import Key

# TODO: vertical versions of commands? could be handy for navigation to have
# a jump that considers vertical to cost 0 and horizontal to cost normal,
# so you could say whatever mike to go from the closing brace to mapping.

# TODO: maybe different commands for letters vs. symbols? help break up
# possibilities?

_mapping = {
    "home"                    : Key("home"),
    "edge"                    : Key("end"),
    "top side"                : Key("a-langle"),
    "bottom"                  : Key("a-rangle"),
    "pro [<n>]"               : Key("a-f:%(n)d"),
    "per [<n>]"               : Key("a-b:%(n)d"),
    "over [<n>]"              : Cmd("(forward-symbol 1)"),
    "under [<n>]"             : Cmd("(forward-symbol -1)"),
    "leaf [<n>]"              : Key("pgdown:%(n)d"),
    "feel [<n>]"              : Key("pgup:%(n)d"),

    "gruff [<n>]"             : Key("a-lbrace:%(n)d"),
    "graph [<n>]"             : Key("a-rbrace:%(n)d"),
    "left [<n>]"              : Key("c-b:%(n)d"),
    "right [<n>]"             : Key("c-f:%(n)d"),
    "hike [<n>]"              : Key("c-p:%(n)d"),
    "slide [<n>]"             : Key("c-n:%(n)d"),

    "go <charrule> [<i>]"     : CharCmd("(md-move-up-to-char 1 %s)"),
    "doog <charrule> [<i>]"   : CharCmd("(md-move-up-to-char -1 %s)"),
    "function"                : Key("ca-a"),
    "snug [<i>]"              : Cmd("(md-find-indentation-change 1 '>)"),
    "guns [<i>]"              : Cmd("(md-find-indentation-change -1 '>)"),
    "loosen [<i>]"            : Cmd("(md-find-indentation-change 1 '<)"),
    "nesool [<i>]"            : Cmd("(md-find-indentation-change -1 '<)"),
    "store <charrule>"        : CharCmd("(copy-to-register %s (region-beginning) (region-end))"),
    "insert <charrule>"       : CharCmd("(insert-register %s)"),
    "bookmark <charrule>"     : CharCmd("(point-to-register %s)"),
    #"store buff <charrule>"  : CharCmd("(window-configuration-to-register %s)"),
    "load <charrule>"         : CharCmd("(jump-to-register %s)"),
    "previous [<n>]"          : Cmd("(md-get-previous-instance-of-symbol)"),
    "next [<n>]"              : Cmd("(md-get-next-instance-of-symbol)"),

    "line <charrule> [<i>]"   : CharCmd("(md-find-line-starting-with-char 1 %s)"),
    "Nile <charrule> [<i>]"   : CharCmd("(md-find-line-starting-with-char -1 %s)"),
    # chopping block?
    "sym <charrule> [<i>]"    : CharCmd("(md-move-up-to-symbol-starting-with-char nil %s)"),
    # "sym <charrule> [<i>]"  : Key("c-c,s,s,%(charrule)s"),
    "miss <charrule> [<i>]"   : CharCmd("(md-move-up-to-symbol-starting-with-char t %s)"),
    # "miss <charrule> [<i>]" : CharCmd("(md-move-up-to-symbol-starting-with-char -1 %s)"),

    "lookup"                  : Key("a-dot"),
    "references"              : Key("a-comma"),
}

NavRule = makeContextualRule("Nav", _mapping, emacsExtras, emacsDefaults)
NavRule.context.addRequirement(IsEmacs)
