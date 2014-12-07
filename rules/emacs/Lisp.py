from Actions import Text
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Text import EmacsText
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase

@registerRule
class Lisp(EmacsBase):
    majorMode = "emacs-lisp-mode"
    
    mapping = {
        "conned"        : EmacsText("cond", lower=False),
        "cooder"        : EmacsText("cdr", lower=False),
        "set Q"         : EmacsText("setq", lower=False),
        "set Q default" : EmacsText("setq-default", lower=False),
    }

mainWords = {
    "and",
    "car",
    "catch",
    "cons",
    "condition-case",
    "defadvice",
    "defconst",
    "defmacro",
    "defun",
    "defvar",
    "defvar-local",
    "dolist",
    "format",
    "function",
    "if",
    "interactive",
    "lambda",
    "let",
    "nil",
    "map",
    "mapcar",
    "message",
    "optional",
    "or",
    "prog1",
    "prog2",
    "progn",
    #"quote",
    "save-current-buffer",
    "save-excursion",
    "save-restriction",
    "track-mouse",
    "unless",
    "unwind-protect",
    "with-selected-frame",
    #"when",
    "while",
}
Lisp.mapping.update({i : EmacsText("%s" % i, lower=False) for i in mainWords})
