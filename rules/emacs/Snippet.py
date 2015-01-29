from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd, Cmd

@registerRule
class SnippetRules(EmacsBase):
    mapping = {
        "jump <charrule>" : CharCmd('(md-sn-find-slot %s)'),
        "blank"           : Cmd('(md-sn-next-slot)'),
        "make blank"      : Cmd('(md-sn-drop-slot)'),
        "call [<i>]"      : Cmd("(md-insert-call-snippet %(i)d)"),
    }

