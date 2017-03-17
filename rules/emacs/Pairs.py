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
    # hot
    "after"      : Key("ca-f"),
    "before"     : Key("ca-b"),
    "dive"       : Key("ca-d"),
    "back dive"  : Key("cs-d"),
    "away"       : Key("ca-o"),
    "back away"  : Key("cs-u"),
    "peak"       : Key("ca-n"),
    "boo"        : Key("ca-p"),
    "start"      : Key("c-home"),
    "close"      : Key("c-end"),
    "slurp"      : Key("ca-y"),
    "back slurp" : Key("ca-g"),
    "peel"       : Key("ca-j"),
    # cold
    "pair barf"       : Key("ca-v"),
    "pair back barf"  : Key("ca-k"),
    "pair split"      : Key("ca-i"),
    "pair rewrap"     : Key("ca-z"),
}

PairCmdRule = makeContextualRule("PairCmd", _mapping, emacsExtras, emacsDefaults)
PairCmdRule.context.addRequirement(IsEmacs)
