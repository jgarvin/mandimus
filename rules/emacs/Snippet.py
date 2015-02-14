from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "jump <charrule>" : CharCmd('(md-sn-find-slot %s)'),
    "blank"           : Cmd('(md-sn-next-slot)'),
    "make blank"      : Cmd('(md-sn-drop-slot)'),
    "call [<i>]"      : Cmd("(md-insert-call-snippet %(i)d)"),
}

SnippetRule = makeContextualRule("Snippet", _mapping, emacsExtras, emacsDefaults)
SnippetRule.context.addRequirement(IsEmacs)

