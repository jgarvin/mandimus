import mdlog
log = mdlog.getLogger(__name__)
from protocol import RuleType, RuleRef, makeHashedRule
from EventLoop import pushEvent
from EventList import RuleRegisterEvent
import rules.BaseRules as BaseRules
from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "larp"   : "(",
    "lace"   : "{",
    "lack"   : "[",
    "quote"  : "\\\"",
    "soot"   : "'",
    "lesser" : "<",
}

PairsRule = makeHashedRule("PairsRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(PairsRule))

_mapping = {
    "after"           : "sp-forward-sexp",
    "before"          : "sp-backward-sexp",
    "dive"            : "sp-down-sexp",
    "back dive"       : "sp-backward-down-sexp",
    "out"             : "sp-up-sexp",
    "back out"        : "sp-backward-up-sexp",
    "peak"            : "sp-next-sexp",
    "boo"             : "sp-previous-sexp",
    "start"           : "sp-beginning-of-sexp",
    #"after start"    : "sp-beginning-of-next-sexp",
    #"before start"   : "sp-beginning-of-previous-sexp",
    "close"           : "sp-end-of-sexp",
    #"after close"    : "sp-end-of-next-sexp",
    #"before close"   : "sp-end-of-previous-sexp",
    # building on this is needed for selecting current sexp
    # "select next"     : "sp-select-next-thing",
    # "select previous" : "sp-select-previous-thing",
    "slurp"           : "sp-forward-slurp-sexp",
    "gulp"            : "sp-backward-slurp-sexp",
    "barf"            : "sp-forward-barf-sexp",
    "chuck"           : "sp-backward-barf-sexp",
    "strip"           : "sp-splice-sexp",
    "split"           : "sp-split-sexp",
}

PairOpsRule = makeHashedRule("PairOpsRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(PairOpsRule))

class PairCmd(Cmd):
    def _lisp(self, extras={}):
        op = PairOpsRule.rule.mapping[" ".join(extras['sexpFunction']['words'])]

        pair = " ".join(extras['sexpPair']['words']) if 'sexpPair' in extras else None
        pair = PairsRule.rule.mapping[pair] if pair else None 

        if pair:
            return "(single-pair-only-sexp \"%s\" '%s)" % (pair, op)
        return "(call-interactively '%s)" % op

_mapping  = {
    "<sexpFunction> [<sexpPair>] [<i>]" : PairCmd(),
    "rewrap" : Cmd("(sp-rewrap-sexp)"),
}

_extras = emacsExtras + [
    RuleRef(PairsRule, "sexpPair"),
    RuleRef(PairOpsRule, "sexpFunction"),
]

PairCmdRule = makeContextualRule("PairCmd", _mapping, _extras, emacsDefaults)
PairCmdRule.context.addRequirement(IsEmacs)

