from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Minibuf
from protocol import RuleType

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

## Turns out that since these are all single letter triggered,
## it's better to jus remember the letter and say the phrase for
## that. That way there's nothing different to remember between
## using voice vs. keyboard. So we comment out.

# MagitRule = makeContextualRule("Magit", _mapping, emacsExtras, emacsDefaults)
# MagitRule.context.addRequirement(IsEmacs)
# MagitRule.context.addRequirement(ModeRequirement(modes="magit-status-mode"))

_mapping = {
    "inquisition" : Minibuf("magit-blame-mode"),
}

MagitMiscRule = makeContextualRule("MagitMisc", _mapping, emacsExtras, emacsDefaults, ruleType=RuleType.INDEPENDENT)
MagitMiscRule.context.addRequirement(IsEmacs)
