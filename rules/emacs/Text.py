from rules.MappingRule import MappingRule
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from Actions import Text, Camel, Hyphen, Underscore, Action, FormatState
from rules.Elements import Integer, Dictation

class EmacsText(Text):
    def __init__(self, data, lower=True, capitalCheck=True):
        Text.__init__(self, data, lower=lower)
        self.capitalCheck = capitalCheck

    def _print(self, words):
        needCapital = False
        if self.capitalCheck:
            needCapital = runEmacsCmd("(md-need-capitalization)").strip() is 't'
        needSpace = runEmacsCmd("(md-need-space \"%s\")" % words).strip() is 't'
        if needCapital:
            print 'capitalize'
            words = words[0].upper() + words[1:]
        if needSpace:
            print 'need space'
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
