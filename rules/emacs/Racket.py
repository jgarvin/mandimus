from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule

keywords = [
    "All",
    "Any",
    "Boolean",
    "Char",
    "Float",
    "Integer",
    "Listof",
    "Number",
    "Real",
    "String"
    "Symbol",
    "andmap",
    "ann",
    "apply",
    "assert",
    "begin",
    "car",
    "case",
    "case-lambda",
    "cond",
    "cons",
    "define",
    "define",
    "define-type",
    "display",
    "else",
    "empty",
    "error",
    "filter",
    "guard",
    "inst",
    "lambda",
    "lang",
    "length",
    "let",
    "let-values",
    "letrec",
    "list",
    "map",
    "methods",
    "module",
    "mutable",
    "null",
    "ormap",
    "print",
    "datum",
    "syntax",
    "racket",
    "provide",
    "require",
    "struct",
    "transparent",
    "undefined",
    "format",
    "values",
    "vector",
    "void",
    "when",
    ["add1", "add one"],
    ["cdr", "could-er"],
    ["let*", "let star"],
    ["let*-values", "let star values"],
    ["require/typed", "typed require"],
    ["set!", "set"],
    "raise",
    "String",
    "unless"
]

RacketKeywordRule = KeywordRule(["racket-mode", "racket-repl-mode"], keywords)

_mapping = {
    "send buff"   : Key("c-c,c-c"),
    "send region" : Key("c-c,c-r"),
    "interpreter" : Key("c-c,c-z"),
    "help racket" : Key("c-c,c-d"),
    "macro expand" : Key("c-c,c-e,e"),
    "macro again" : Key("c-c,c-e,a"),
    "macro region" : Key("c-c,c-e,r"),
    "macro definition" : Key("c-c,c-e,x"),
}

RacketRule = makeContextualRule("Racket", _mapping, emacsExtras, emacsDefaults)
RacketRule.context.addRequirement(IsEmacs)
RacketRule.context.addRequirement(ModeRequirement(modes=["racket-mode", "racket-repl-mode"]))
