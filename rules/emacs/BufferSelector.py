from rules.emacs.Cmd import runEmacsCmd 
from rules.emacs.EmacsSelector import EmacsSelector
from wordUtils import extractWords

# TODO: oops, all selectors use 'win' as their command!
# TODO: also for some reason emacs selectors don't activate?

class BufferSelector(EmacsSelector):
    def __init__(self):
        EmacsSelector.__init__(self, "Buffer", "buff", "(mapcar 'buffer-name (buffer-list))")

    def _update(self, newList):
        self.words = set()
        self.selectionMap = []
        for n in newList:
            w = extractWords(n)
            self.words.update(w)
            self.selectionMap.append((w, n))

    def _currentChoice(self):
        buf = runEmacsCmd("(buffer-name (current-buffer))")
        return buf.strip().strip('"')

    def _select(self, choice):
        runEmacsCmd("(switch-to-buffer \"%s\")" % choice)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)")

_selector = BufferSelector()
