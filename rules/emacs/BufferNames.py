from rules.emacs.Cmd import runEmacsCmd 
from rules.WordSelector import WordSelector
from rules.emacs.EmacsEventGenerator import EmacsEventGenerator
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import BufferListEvent
from requirements.Emacs import IsEmacs

bufferListGen = EmacsEventGenerator("Buffer", "(mapcar 'buffer-name (buffer-list))", BufferListEvent)

class BufferNames(WordSelector):
    def __init__(self):
        WordSelector.__init__(self, "BufferNames", "buff")
        self.rule.context.addRequirement(IsEmacs)
        getLoop().subscribeEvent(BufferListEvent, self.onBufferList)

    def onBufferList(self, ev):
        self.words = set()
        self.selectionMap = []
        for n in ev.choices:
            w = extractWords(n)
            self.words.update(w)
            self.selectionMap.append((w, n))
        self._sendWords()

    def _currentChoice(self):
        buf = runEmacsCmd("(buffer-name (current-buffer))")
        return buf.strip().strip('"')

    def _select(self, choice):
        runEmacsCmd("(switch-to-buffer \"%s\")" % choice)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)")

_selector = BufferNames()
