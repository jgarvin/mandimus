from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule
    
keywords = [
    ["1-", "one minus"],
    ["1+", "one plus"],
    "add-hook",
    "add-to-list",
    "and",
    ["aref", "A ref"],
    "assert",
    ["assoc", "uh soak"],
    "beginning-of-thing",
    ["boundp", "bound P"],
    "bounds-of-thing-at-point",
    "buffer-substring",
    "call-interactively",
    "car",
    "case",
    "catch",
    ["cdr", "cooder"],
    "char-to-string",
    "char-after",
    "char-before",
    "concat",
    ["cond", "conned"],
    "condition-case",
    "cons",
    "defadvice",
    "defconst",
    "define-key",
    "defmacro",
    "defstruct",
    "defun",
    "defvar",
    "defvar-local",
    ["delq", "del Q"],
    ["derived-mode-p", "derived mode P"],
    "dolist",
    "dotimes",
    "error",
    ["eq", "E Q"],
    "equal",
    "eval",
    ["fboundp", "F bound P"],
    "format",
    "function",
    "funcall",
    "gensym",
    "global-set-key",
    "goto-char",
    "identity",
    "if",
    "ignore-errors",
    ["incf", "ink F"],
    "insert",
    "interactive",
    "lambda",
    "let",
    ["let*", "let star"],
    "length",
    "list",
    "local-set-key",
    "loop",
    ["&key", "key"],
    "nil",
    "number-sequence",
    "make-local-variable",
    "make-marker",
    "make-string",
    "make-vector",
    "map",
    "mapcar",
    "mapconcat",
    "marker-position",
    "max",
    "member",
    ["memq", "mem Q"],
    "message",
    "min",
    ["-", "minus"],
    "not",
    ["nth", "enth"],
    "null",
    ["&optional", "optional"],
    "or",
    ["plist-get", "P list get"],
    ["+", "plus"],
    "point",
    "point-min",
    "point-max",
    "prog1",
    "prog2",
    "progn",
    "propertize",
    "provide",
    "push",
    ["rassoc", "R uh soak"],
    ["remq", "rem Q"],
    "re-search-backward",
    "re-search-forward",
    "regexp-quote",
    "region-beginning",
    "region-end",
    "remove-hook",
    "replace-regexp-in-string",
    "require",
    ["&rest", "rest"],
    "quote",
    "save-current-buffer",
    "save-excursion",
    "save-match-data",
    "save-restriction",
    "set",
    "setcar",
    "set-window-point",
    "set-marker",
    ["setf", "set F"],
    ["setq", "set Q"],
    ["setq-default", "set Q default"],
    "split-string",
    ["string=", "string equal"],
    "string-match",
    "sort",
    "subseq",
    "thing-at-point",
    "time-add",
    "throw",
    "track-mouse",
    "unless",
    "unwind-protect",
    "user-error",
    "window-start",
    "window-end",
    "with-current-buffer",
    "with-selected-frame",
    "with-selected-window",
    "when",
    "while",
]

LispKeywordRule = KeywordRule(["emacs-lisp-mode", "eshell-mode"], keywords)
