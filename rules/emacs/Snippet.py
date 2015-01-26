from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd, Cmd

@registerRule
class SnippetRules(EmacsBase):
    mapping = {
        "jump <charrule>" : CharCmd('(md-sn-find-slot %s)'),
        "tolls"           : Cmd('(md-sn-next-slot)'),
        "make slot"       : Cmd('(md-sn-drop-slot)'),
    }

