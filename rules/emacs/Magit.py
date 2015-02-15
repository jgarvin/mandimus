from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "refresh"       : Key("g"),
    "stage"         : Key("s"),
    "unstage"       : Key("u"),
    "commit"        : Key("c"),
    "done"          : Key("q"),
    "level one"     : Key("a-1"),
    "level two"     : Key("a-2"),
    "level three"   : Key("a-3"),
    "level four"    : Key("a-4"),
    "more context"  : Key("plus"),        
    "less context"  : Key("hyphen"),
    "discard"       : Key("k"),
    "branch"        : Key("b"),
    "merge"         : Key("m"),
    "diff"          : Key("d"),
    "push"          : Key("P"),
    "push remote"   : Key("c-u,P"),
    "remote update" : Key("f"),
    "pull"          : Key("F"),
}

MagitRule = makeContextualRule("Magit", _mapping, emacsExtras, emacsDefaults)
MagitRule.context.addRequirement(IsEmacs)
MagitRule.context.addRequirement(ModeRequirement(modes="magit-status-mode"))


    
