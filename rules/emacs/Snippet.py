from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd, Cmd

@registerRule
class SnippetRules(EmacsBase):
    mapping = {
        "slot <charrule>" : CharCmd('(md-sn-find-slot %s)'),
        "next slot"       : Cmd('(md-sn-next-slot)'),
        "make slot"       : Cmd('(md-sn-drop-slot)'),
    }

