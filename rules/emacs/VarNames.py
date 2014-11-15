from rules.MappingRule import MappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from Actions import Text, Camel, Hyphen, Underscore, Action, FormatState
from rules.Elements import Integer, Dictation

class EmacsText(Text):
    def _print(self, words):
        needSpace = runEmacsCmd("(md-need-space)").strip() is 't'
        if needSpace:
            words = ' ' + words
            
        # There's no good elisp way to handle putting characters into
        # the search box AFAIK. You can get text in there but giving it
        # focus disables search as you type.
        inSearchMode = runEmacsCmd("isearch-mode").strip() != 'nil'
        inMiniBuffer = '*Minibuf-' in runEmacsCmd("(with-current-buffer (buffer-name))").strip()
        if inSearchMode or inMiniBuffer:
            Text._print(self, words)
        else:
            runEmacsCmd("(undo-boundary)")
            runEmacsCmd("(insert \"%s\")" % words)
            runEmacsCmd("(undo-boundary)")


def emacsTextPrint(self, words):
    return EmacsText('')._print(words)

class EmacsCamel(Camel): pass
class EmacsHyphen(Hyphen): pass
class EmacsUnderscore(Underscore): pass
EmacsCamel._print = emacsTextPrint
EmacsHyphen._print = emacsTextPrint
EmacsUnderscore._print = emacsTextPrint

class TypingBase(MappingRule):
    extras = [
        Integer("n", 1, 20),
        Dictation("text")
        ]
    
    defaults = {
        "n": 1,
        }    

    @classmethod
    def activeForWindow(cls, window):
        return Emacs.activeForWindow(window)
        
@registerRule
class EmacsTextRule(TypingBase):
    mapping = {
        "type <text>" : EmacsText("%(text)s"),
    }

@registerRule
class EmacsCamelRule(TypingBase):
    mapping = {
        "camel <text>" : EmacsCamel("%(text)s"),
    }

@registerRule
class EmacsStudRule(TypingBase):
    mapping = {
        "stud <text>" : EmacsCamel("%(text)s", True),
    }

@registerRule
class EmacsHyphenRule(TypingBase):
    mapping = {
        "fen <text>"     : EmacsHyphen("%(text)s"),
        "cap fen <text>" : EmacsHyphen("%(text)s", True),
    }    

@registerRule
class EmacsUnderscoreRule(TypingBase):
    mapping = {
        "score <text>"     : EmacsUnderscore("%(text)s"),
        "cap score <text>" : EmacsUnderscore("%(text)s", True),
    }
