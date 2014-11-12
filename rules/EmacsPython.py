from SeriesMappingRule import SeriesMappingRule
from Emacs import EmacsRule
from EmacsCmd import runEmacsCmd, Cmd
from Rule import registerRule

@registerRule
class EmacsPython(SeriesMappingRule):
    mapping = {
        "align dic"                     : Cmd("(align-dict)"),
    }

    @classmethod
    def activeForWindow(cls, window):
        isemacs = EmacsRule.activeForWindow(window)
        if not isemacs:
            return False
        out = runEmacsCmd("major-mode").strip()
        return out == "python-mode"

