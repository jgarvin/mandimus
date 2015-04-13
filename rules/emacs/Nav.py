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
    "go <charrule> [<i>]"      : CharCmd("(md-move-up-to-char 1 %s)"),
    "come <charrule> [<i>]"    : CharCmd("(md-move-up-to-char -1 %s)"),
    # these weren't as useful as I thought because you can't switch the context
    # mid-utterance so that dragon knows what chars are eligible
    # "north <charrule> [<i>]" : CharCmd("(md-move-up-to-char-after-line -1 %s)"),
    # "south <charrule> [<i>]" : CharCmd("(md-move-up-to-char-after-line 1 %s)"),
    # "east <charrule> [<i>]"  : CharCmd("(md-move-up-to-char-same-line 1 %s)"),
    # "west <charrule> [<i>]"  : CharCmd("(md-move-up-to-char-same-line -1 %s)"),
    "snug [<i>]"               : Cmd("(md-find-indentation-change 1 '>)"),
    "guns [<i>]"               : Cmd("(md-find-indentation-change -1 '>)"),
    "loosen [<i>]"             : Cmd("(md-find-indentation-change 1 '<)"),
    "nesool [<i>]"             : Cmd("(md-find-indentation-change -1 '<)"),
    # these were made obsolete by 'go'
    #"sled [<i>]"              : Cmd("(md-next-whitespace-separated-thing)"),
    #"dels [<i>]"              : Cmd("(md-previous-whitespace-separated-thing)"),
    "sym <charrule> [<i>]"     : CharCmd("(md-move-up-to-symbol-starting-with-char 1 %s)"),
    "miss <charrule> [<i>]"    : CharCmd("(md-move-up-to-symbol-starting-with-char -1 %s)"),
    "line <charrule> [<i>]"    : CharCmd("(md-find-line-starting-with-char 1 %s)"),
    "Nile <charrule> [<i>]"    : CharCmd("(md-find-line-starting-with-char -1 %s)"),
    "store <charrule>"         : CharCmd("(copy-to-register %s)"),
    "insert <charrule>"        : CharCmd("(insert-to-register %s)"),
    "store point <charrule>"   : CharCmd("(point-to-register %s)"),
    "store buff <charrule>"    : CharCmd("(window-configuration-to-register %s)"),
    "load <charrule>"          : CharCmd("(jump-to-register %s)"),
    "previous [<n>]"           : Cmd("(md-get-previous-instance-of-symbol)"),
    "next [<n>]"               : Cmd("(md-get-next-instance-of-symbol)"),
    "lookup"                   : Key("a-dot"),
    "references"               : Key("a-comma"),
}

NavRule = makeContextualRule("Nav", _mapping, emacsExtras, emacsDefaults)
NavRule.context.addRequirement(IsEmacs)


