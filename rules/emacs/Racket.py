from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule

keywords = [
    "Number",
    "Real",
    "String"
    "cond",
    "define",
    "define",
    "define-type",
    "else",
    "lang",
    "let",
    "let-values",
    "letrec",
    "struct",
    ["let*", "let star"],
    ["let*-values", "let star values"],
    "lambda",
    "case-lambda",
    "apply",
    "ann",
    "Integer",
    "Listof",
    "Any",
    "Boolean",
    "Char",
    "Symbol",
    "Float",
    "All",
    ["add1", "add one"],
    "list",
    "vector",
    "ormap",
    "when",
    "length",
    "assert",
    "error",
    "require",
    ["require/typed", "typed require"],
    "provide",
    "module",
    "inst"
]

RacketKeywordRule = KeywordRule(["racket-mode", "racket-repl-mode"], keywords)

_mapping = {
    "send buff"   : Key("c-c,c-c"),
    "send region" : Key("c-c,c-r"),
    "interpreter" : Key("c-c,c-z"),
}

RacketRule = makeContextualRule("Racket", _mapping, emacsExtras, emacsDefaults)
RacketRule.context.addRequirement(IsEmacs)
RacketRule.context.addRequirement(ModeRequirement(modes=["racket-mode", "racket-repl-mode"]))
