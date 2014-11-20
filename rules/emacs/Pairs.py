from rules.Rule import registerRule
from rules.emacs.Emacs import Emacs
from rules.Elements import RuleRef
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
        "in"              : "sp-down-sexp",
        "before in"       : "sp-backward-down-sexp",
        "out"             : "sp-up-sexp",
        "before out"      : "sp-backward-up-sexp",
        "next"            : "sp-next-sexp",
        "previous"        : "sp-previous-sexp",
        "start"           : "sp-beginning-of-sexp",
        "end"             : "sp-end-of-sexp",
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
        "[<sexpPair>] <sexpFunction>" : PairCmd(),
    }

    sexpPairRef = RuleRef(SexpPairs, "sexpPair")
    sexpFunctionRef = RuleRef(SexpFunctions, "sexpFunction")

    extras = [
        sexpPairRef,
        sexpFunctionRef,
    ]

    def activeForWindow(self, w):
        return Emacs.activeForWindow(w)

# sexpRules = {}
# for words, func in sexpFuncs.items():
#     for pairWord, p in sexpPairs.items():
#         sexpRules[(words + ' ' + pairWord).strip()] = PairCmd(p, func)

# TODO: not working?
# for pairWord, p in sexpPairs.items():
#     sexpRules[('inner ' + pairWord).strip()] = Key("a-i") + Text("%s" % p)
#     sexpRules[('outer ' + pairWord).strip()] = Key("a-o") + Text("%s" % p)
    
# Emacs.mapping.update(sexpRules)        

