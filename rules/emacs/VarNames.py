from rules.emacs.Cmd import runEmacsCmd, Cmd
from Actions import Text, Camel, Hyphen, Underscore
from protocol import Integer, Dictation, RuleType
from rules.ContextualRule import makeContextualRule
from rules.emacs.Text import EmacsText
from requirements.Emacs import IsEmacs

def emacsTextPrint(self, words):
    return EmacsText('')._print(words)

class EmacsCamel(Camel): pass
class EmacsHyphen(Hyphen): pass
class EmacsUnderscore(Underscore): pass
EmacsCamel._print = emacsTextPrint
EmacsHyphen._print = emacsTextPrint
EmacsUnderscore._print = emacsTextPrint

_extras = [
    Dictation("text")
    ]

_defaults = {}    

_mapping = {
    "type <text>" : EmacsText("%(text)s", False),
}

EmacsTypingRule = makeContextualRule("EmacsTypingRule", _mapping, _extras, _defaults, RuleType.TERMINAL)
EmacsTypingRule.context.addRequirement(IsEmacs)
    
_mapping = {
    "fasten <text>" : EmacsText("%(text)s", False, spaceCheck=False),
    "cap fasten <text>" : EmacsText("%(text)s", False, spaceCheck=False, allCaps=True),
}

EmacsFastenRule = makeContextualRule("EmacsFastenRule", _mapping, _extras, _defaults, RuleType.TERMINAL)
EmacsFastenRule.context.addRequirement(IsEmacs)

_mapping = {
    "camel <text>" : EmacsCamel("%(text)s"),
}

EmacsCamelRule = makeContextualRule("EmacsCamelRule", _mapping, _extras, _defaults, RuleType.TERMINAL)
EmacsCamelRule.context.addRequirement(IsEmacs)

_mapping = {
    "stud <text>" : EmacsCamel("%(text)s", True),
}

EmacsStudRule = makeContextualRule("EmacsStudRule", _mapping, _extras, _defaults, RuleType.TERMINAL)
EmacsStudRule.context.addRequirement(IsEmacs)

_mapping = {
    "hyphen <text>"     : EmacsHyphen("%(text)s"),
    "cap hyphen <text>" : EmacsHyphen("%(text)s", True),
}    

EmacsHyphenRule = makeContextualRule("EmacsHyphenRule", _mapping, _extras, _defaults, RuleType.TERMINAL)
EmacsHyphenRule.context.addRequirement(IsEmacs)

_mapping = {
    "score <text>"     : EmacsUnderscore("%(text)s"),
    "cap score <text>" : EmacsUnderscore("%(text)s", True),
}

EmacsScoreRule = makeContextualRule("EmacsScoreRule", _mapping, _extras, _defaults, RuleType.TERMINAL)
EmacsScoreRule.context.addRequirement(IsEmacs)
