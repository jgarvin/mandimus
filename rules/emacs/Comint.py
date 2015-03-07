from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Text import EmacsText

_mapping = {
    "hiss [<i>]"        : Key("a-p:%(i)d"),
    "piss [<i>]"        : Key("a-n:%(i)d"),
    "history"           : Key("a-r"),
    "interrupt"         : Key("c-c,c-c"),
    "exit"              : Key("c-d"),
    "prompt up [<n>]"   : Key("c-c,c-p:%(n)d"),
    "prompt down [<n>]" : Key("c-c,c-n:%(n)d"),
}

ComintRule = makeContextualRule("Comint", _mapping, emacsExtras, emacsDefaults)
ComintRule.context.addRequirement(IsEmacs)
ComintRule.context.addRequirement(ModeRequirement(modes="comint-mode"))
