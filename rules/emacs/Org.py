from rules.SeriesMappingRule import SeriesMappingRule
from rules.MappingRule import MappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from Actions import Key, Text, Repeat
from rules.emacs.Base import EmacsBase

@registerRule
class OrgAnywhere(MappingRule):
    mapping = {
        "save link"   : Key("c-c,l"),
        "agenda menu" : Key("c-c,a"),
        "agenda"      : Key("c-c,a,a"),
        "list to do"  : Key("c-c,a,t"),
    }

    @classmethod
    def activeForWindow(cls, window):
        return Emacs.activeForWindow(window)
    

@registerRule
class Org(EmacsBase):
    majorMode = ["org-mode", "org-agenda-mode"]
    
    # "scoot" -> expand section
    # "cap scoot" -> collapse all sections
    # "lima" -> log view to see record for the day
    mapping = {
        "new"                     : Key("a-enter"),
        "new todo"                : Key("as-enter"),
        "make headline"           : Key("c-c,asterisk"),
        "archive [<n>]"           : Key("c-c,c-x,c-a") * Repeat(extra="n"),
        "task"                    : Key("c-c,c-t"),
        "follow"                  : Key("c-c,c-o"),
        "insert link"             : Key("c-c,c-l"),
        "schedule"                : Key("c-c,c-s"),
        "increase priority [<n>]" : Key("s-up:%(n)d"),
        "decrease priority [<n>]" : Key("s-down:%(n)d"),
        "priority one"            : Key("c-c,comma,a"),
        "priority two"            : Key("c-c,comma,b"),
        "priority three"          : Key("c-c,comma,c"),
    }

