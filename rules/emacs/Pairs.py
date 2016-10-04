import mdlog
log = mdlog.getLogger(__name__)
from protocol import RuleType, RuleRef, makeHashedRule
from EventLoop import pushEvent
from EventList import RuleRegisterEvent
import rules.BaseRules as BaseRules
from rules.emacs.Cmd import CharCmd, Cmd, Minibuf, Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping  = {
    "rewrap"     : Key("ca-z"),
    "after"      : Key("ca-f"),
    "before"     : Key("ca-b"),
    "dive"       : Key("ca-d"),
    "back dive"  : Key("cs-d"),
    "away"       : Key("ca-o"),
    "back away"  : Key("cs-u"),
    "peak"       : Key("ca-n"),
    "boo"        : Key("ca-p"),
    "start"      : Key("ca-home"),
    "close"      : Key("ca-end"),
    "slurp"      : Key("ca-y"),
    "back slurp" : Key("ca-g"),
    "barf"       : Key("ca-v"),
    "back barf"  : Key("ca-k"),
    "peel"       : Key("ca-j"),
    "split"      : Key("ca-i"),

}

PairCmdRule = makeContextualRule("PairCmd", _mapping, emacsExtras, emacsDefaults)
PairCmdRule.context.addRequirement(IsEmacs)
