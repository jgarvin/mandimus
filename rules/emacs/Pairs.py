from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from Actions import Key, Text

sexpFuncs = {
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

sexpPairs = {
    ""      : "(", # parens are default
    "lace"  : "{",
    "lack"  : "[",
    "quote" : "\\\"",
    "soak"  : "'",
    "angle" : "<",
}

class PairCmd(Cmd):
    def __init__(self, pair, cmd):
        x = "(single-pair-only-sexp \"%s\" '%s)" % (pair, cmd)
        Cmd.__init__(self, x)

sexpRules = {}
for words, func in sexpFuncs.items():
    for pairWord, p in sexpPairs.items():
        sexpRules[(words + ' ' + pairWord).strip()] = PairCmd(p, func)

# TODO: not working?
# for pairWord, p in sexpPairs.items():
#     sexpRules[('inner ' + pairWord).strip()] = Key("a-i") + Text("%s" % p)
#     sexpRules[('outer ' + pairWord).strip()] = Key("a-o") + Text("%s" % p)
    
Emacs.mapping.update(sexpRules)        

