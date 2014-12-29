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
        "key conned"        : EmacsText("cond", lower=False),
        "key cooder"        : EmacsText("cdr", lower=False),
        "key set Q"         : EmacsText("setq", lower=False),
        "key del Q"         : EmacsText("delq", lower=False),
        "key mem Q"         : EmacsText("memq", lower=False),
        "key rem Q"         : EmacsText("remq", lower=False),
        "key set Q default" : EmacsText("setq-default", lower=False),
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
    "defstruct",
    "defun",
    "defvar",
    "defvar-local",
    "dolist",
    "dotimes",
    "format",
    "function",
    "funcall",
    "gensym",
    "if",
    "interactive",
    "lambda",
    "let",
    "length",
    "loop",
    "nil",
    "map",
    "mapcar",
    "mapconcat",
    "member",
    "message",
    "not",
    "null",
    "optional",
    "or",
    "point",
    "prog1",
    "prog2",
    "progn",
    "provide",
    "require",
    "rest",
    "quote",
    "save-current-buffer",
    "save-excursion",
    "save-restriction",
    "setcar",
    "subseq",
    "throw",
    "track-mouse",
    "unless",
    "unwind-protect",
    "with-current-buffer",
    "with-selected-frame",
    "when",
    "while",
}
Lisp.mapping.update({"key " + i : EmacsText("%s" % i, lower=False) for i in mainWords})
