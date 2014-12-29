import mdlog
log = mdlog.getLogger(__name__)

from rules.MappingRule import MappingRule
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from Actions import Text, Camel, Hyphen, Underscore, Action, FormatState
from rules.Elements import Integer, Dictation

def emacsBool(b):
    if b:
        return "t"
    return "nil"

class EmacsText(Text):
    def __init__(self, data, lower=True, capitalCheck=True):
        Text.__init__(self, data, lower=lower)
        self.capitalCheck = capitalCheck

    def _print(self, words):
        # There's no good elisp way to handle putting characters into
        # the search box AFAIK. You can get text in there but giving it
        # focus disables search as you type.
        inSearchMode = runEmacsCmd("isearch-mode") != 'nil'
        inMiniBuffer = '*Minibuf-' in runEmacsCmd("(with-current-buffer (buffer-name))")
        if inSearchMode or inMiniBuffer:
            Text._print(self, words)
        else:
            runEmacsCmd("(undo-boundary)")
            runEmacsCmd("(md-insert-text \"%s\" %s %s)" % (words, "t", emacsBool(self.capitalCheck)), dolog=True)
            runEmacsCmd("(undo-boundary)")
