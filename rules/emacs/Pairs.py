from rules.Rule import registerRule
from rules.emacs.Emacs import Emacs
from rules.Elements import RuleRef, Integer
from rules.MappingRule import MappingRule
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Cmd import runEmacsCmd, Cmd
from Actions import Key, Text

@registerRule
class SexpPairs(MappingRule):
    refOnly = True
    mapping = {
        "larp"   : "(",
        "lace"   : "{",
        "lack"   : "[",
        "quote"  : "\\\"",
        "soak"   : "'",
        "lesser" : "<",
    }

@registerRule
class SexpFunctions(MappingRule):
    refOnly = True
    mapping = {
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
    }

class PairCmd(Cmd):
    def _lisp(self, extras={}):
        words = extras['words']
        pair = None

        for p, k in SexpPairs.mapping.items():
            if p in words:
                pair = k

        func = ""
        for p, k in SexpFunctions.mapping.items():
            if p in words and len(p) > len(func):
                func = k
        assert func

        if pair:
            return "(single-pair-only-sexp \"%s\" '%s)" % (pair, func)
        return "(%s)" % func

@registerRule
class PairRule(SeriesMappingRule):
    mapping  = {
        "<sexpFunction> [<sexpPair>] [<n>]" : PairCmd(),
    }

    sexpPairRef = RuleRef(SexpPairs, "sexpPair")
    sexpFunctionRef = RuleRef(SexpFunctions, "sexpFunction")

    extras = [
        sexpPairRef,
        sexpFunctionRef,
        Integer("n", 2, 20),
    ]

    defaults = {
        "n"    : 1,
    }    

    def activeForWindow(self, w):
        return Emacs.activeForWindow(w)

