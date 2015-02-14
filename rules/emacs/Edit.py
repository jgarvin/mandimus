from Actions import Key
from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

# # TODO: would be nice for all of these
# # to be ace'able
#     mapping = {
#         "word"     : "'word",
#         "line"     : "'line",
#         "ace"      : "'ace",
#         "ace line" : "'ace-line",
#         "graph"    : "'paragraph",
#         "block"    : "'block",
#         "larp"     : "'parens",
#         "lack"     : "'brackets",
#         "lace"     : "'lace",
#         "lesser"   : "'angles",
#     }

#     mapping = {
#         "mark" : "'mark",
#         "cut"  : "'cut",
#         "copy" : "'copy",
#         "dupe" : "'dupe",
#         "swap" : "'swap",
#     }

_mapping = {
    "zap <charrule> [<n>]"   : CharCmd("(zap-up-to-char 1 %s)"),
    "taze <charrule> [<n>]"  : CharCmd("(zap-up-to-char -1 %s)"),
    #"fizz <charrule> [<n>]" : CharCmd("(md-copy-up-to-char 1 %s)"),
    #"buzz <charrule> [<n>]" : CharCmd("(md-copy-up-to-char -1 %s)"),
    "trans [<n>]"            : Cmd("(transpose-sexps 1)"),
    "snart [<n>]"            : Cmd("(transpose-sexps -1)"),
    "rise [<n>]"             : Key("a-up:%(n)d"),
    "drop [<n>]"             : Key("a-down:%(n)d"),
    "var <charrule> [<n>]"   : CharCmd("(md-insert-text (char-to-string %s) t nil)"),
    "phone [<n>]"              : Cmd("(md-cycle-homophones-at-point)"),
}

EditRule = makeContextualRule("Edit", _mapping, emacsExtras, emacsDefaults)
EditRule.context.addRequirement(IsEmacs)

