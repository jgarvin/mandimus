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
    "select next"     : "sp-select-next-thing",
    "select previous" : "sp-select-previous-thing",
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
        words = extras['words']
        pair = None

        for p, k in PairsRule.rule.mapping.items():
            if p in words:
                pair = k

        func = ""
        for p, k in PairOpsRule.rule.mapping.items():
            if p in words and len(p) > len(func):
                func = k
        assert func

        if pair:
            return "(single-pair-only-sexp \"%s\" '%s)" % (pair, func)
        return "(call-interactively '%s)" % func

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

