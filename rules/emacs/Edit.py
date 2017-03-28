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
    "zap <charrule> [<i>]"   : CharCmd("(zap-up-to-char 1 %s)"),
    "taze <charrule> [<i>]"  : CharCmd("(zap-up-to-char -1 %s)"),
    #"fizz <charrule> [<i>]" : CharCmd("(md-copy-up-to-char 1 %s)"),
    #"buzz <charrule> [<i>]" : CharCmd("(md-copy-up-to-char -1 %s)"),
    "trans [<i>]"            : Cmd("(transpose-sexps 1)"),
    "back trans [<i>]"       : Cmd("(transpose-sexps -1)"),
    "rise [<n>]"            : Key("a-up:%(n)d"),
    "drop [<n>]"             : Key("a-down:%(n)d"),
    #"var <charrule> [<n>]"  : CharCmd("(md-insert-text (char-to-string %s) t nil)"),
    "phone"                  : Cmd("(md-cycle-homophones-at-point)"),
    "sort lines"             : Cmd("(call-interactively #'sort-lines)"),
    "make score"             : Key("c-c,m,s"),
    "make stud"              : Key("c-c,m,c"),
    "make camel"             : Key("c-c,m,l"),
    "make upper"             : Key("c-c,m,u")
}

EditRule = makeContextualRule("Edit", _mapping, emacsExtras, emacsDefaults)
EditRule.context.addRequirement(IsEmacs)
