from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd, Cmd

@registerRule
class SnippetRules(EmacsBase):
    mapping = {
        "jump <charrule>" : CharCmd('(md-sn-find-slot %s)'),
        "slot"            : Cmd('(md-sn-next-slot)'),
        "make slot"       : Cmd('(md-sn-drop-slot)'),
        "call [<i>]"      : Cmd("(md-insert-call-snippet %(i)d)"),
    }

