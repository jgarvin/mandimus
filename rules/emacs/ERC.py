from Actions import Key, Text
from rules.Elements import Dictation, Integer
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule

@registerRule
class ERC(SeriesMappingRule):
    mapping = {
        "hiss" : Key("a-p"),
        "piss" : Key("a-n"),
        "join [<text>]" : Text("/join #%(text)s"),
    }

    extras = [
        Integer("n", 2, 20),
        Dictation("text"),
        ]

    defaults = {
        "n"    : 1,
        "text" : "",
        }        

    @classmethod
    def activeForWindow(cls, window):
        isemacs = Emacs.activeForWindow(window)
        if not isemacs:
            return False
        out = runEmacsCmd("major-mode").strip()
        return out == "erc-mode"
