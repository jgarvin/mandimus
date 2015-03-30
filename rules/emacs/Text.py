import mdlog
log = mdlog.getLogger(__name__)

from Actions import Text
from rules.emacs.Cmd import runEmacsCmd

def emacsBool(b):
    if b:
        return "t"
    return "nil"

class EmacsText(Text):
    def __init__(self, data, lower=True, capitalCheck=True, spaceCheck=True,
                 allCaps=False):
        Text.__init__(self, data, lower=lower)
        self.capitalCheck = capitalCheck
        self.spaceCheck = spaceCheck 

    def _print(self, words):
        # There's no good elisp way to handle putting characters into
        # the search box AFAIK. You can get text in there but giving it
        # focus disables search as you type.
        inSearchMode = runEmacsCmd("isearch-mode") != 'nil'
        inMiniBuffer = '*Minibuf-' in runEmacsCmd("(with-current-buffer (buffer-name))")
        if inSearchMode or inMiniBuffer:
            Text._print(self, words)
        else:
            runEmacsCmd("(md-insert-text \"%s\" %s %s)" % (words, emacsBool(self.spaceCheck),
                        emacsBool(self.capitalCheck)), dolog=True,
                        queryOnly=False)
            
