from Actions import Key, Text
from Elements import Dictation, Integer
from SeriesMappingRule import SeriesMappingRule
from Emacs import EmacsRule
from EmacsCmd import runEmacsCmd, Cmd
from Rule import registerRule

@registerRule
class EmacsERC(SeriesMappingRule):
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
        isemacs = EmacsRule.activeForWindow(window)
        if not isemacs:
            return False
        out = runEmacsCmd("major-mode").strip()
        return out == "erc-mode"
