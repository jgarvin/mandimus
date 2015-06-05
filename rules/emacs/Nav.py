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
    "home"                  : Cmd("(md-beginning-or-indentation-toggle)"),
    "edge"                  : Cmd("(end-of-line)"),
    "cliff"                 : Cmd("(md-go-to-cliff)"),
    "top side"              : Key("a-langle"),
    "bottom"                : Key("a-rangle"),
    "window top side"       : Cmd("(goto-char (window-start))"),
    "window bottom"         : Cmd("(goto-char (- (window-end) 1)) (previous-line) (beginning-of-line)"),
    "pro [<n>]"             : Key("a-f:%(n)d"),
    "per [<n>]"             : Key("a-b:%(n)d"),
    "over [<n>]"            : Cmd("(forward-symbol 1)"),
    "under [<n>]"           : Cmd("(forward-symbol -1)"),
    "leaf [<n>]"            : Key("pgdown:%(n)d"),
    "feel [<n>]"            : Key("pgup:%(n)d"),

    "gruff [<n>]"           : Key("a-lbrace:%(n)d"),
    "graph [<n>]"           : Key("a-rbrace:%(n)d"),
    "left [<n>]"            : Key("left:%(n)d"),
    "right [<n>]"           : Key("right:%(n)d"),
    "up [<n>]"              : Key("up:%(n)d"),
    "down [<n>]"            : Key("down:%(n)d"),

    "go <charrule> [<i>]"   : CharCmd("(md-move-up-to-char 1 %s)"),
    "come <charrule> [<i>]" : CharCmd("(md-move-up-to-char -1 %s)"),
    "snug [<i>]"            : Cmd("(md-find-indentation-change 1 '>)"),
    "guns [<i>]"            : Cmd("(md-find-indentation-change -1 '>)"),
    "loosen [<i>]"          : Cmd("(md-find-indentation-change 1 '<)"),
    "nesool [<i>]"          : Cmd("(md-find-indentation-change -1 '<)"),
    "store <charrule>"      : CharCmd("(copy-to-register %s (region-beginning) (region-end))"),
    "insert <charrule>"     : CharCmd("(insert-register %s)"),
    "bookmark <charrule>"   : CharCmd("(point-to-register %s)"),
    #"store buff <charrule>" : CharCmd("(window-configuration-to-register %s)"),
    "load <charrule>"       : CharCmd("(jump-to-register %s)"),
    "previous [<n>]"        : Cmd("(md-get-previous-instance-of-symbol)"),
    "next [<n>]"            : Cmd("(md-get-next-instance-of-symbol)"),

    "line <charrule> [<i>]" : CharCmd("(md-find-line-starting-with-char 1 %s)"),
    "Nile <charrule> [<i>]" : CharCmd("(md-find-line-starting-with-char -1 %s)"),
    # chopping block?
    "sym <charrule> [<i>]"  : CharCmd("(md-move-up-to-symbol-starting-with-char 1 %s)"),
    "miss <charrule> [<i>]" : CharCmd("(md-move-up-to-symbol-starting-with-char -1 %s)"),

    "lookup"                : Key("a-dot"),
    "references"            : Key("a-comma"),
}

NavRule = makeContextualRule("Nav", _mapping, emacsExtras, emacsDefaults)
NavRule.context.addRequirement(IsEmacs)
