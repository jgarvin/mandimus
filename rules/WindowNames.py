import mdlog
log = mdlog.getLogger(__name__)
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import WindowListEvent
from Actions import runCmd
from Window import getFocusedWindow
from rules.WordSelector import WordSelector

class WindowNameSelector(WordSelector):
    def __init__(self):
        getLoop().subscribeEvent(WindowListEvent, self.onWindowList)
        WordSelector.__init__(self, "WindowNames", "win")
        self.rule.activate() # always on

    def onWindowList(self, ev):
        if isinstance(ev, WindowListEvent):
            self.words = set()
            self.selectionMap = []
            for w in ev.windows:
                winWords = extractWords(w.name)
                self.words.update(winWords)
                self.selectionMap.append((winWords, w))
        self._sendWords()

    def _currentChoice(self):
        return getFocusedWindow()

    def _select(self, choice):
        cmd = "xdotool windowactivate %d" % (choice.winId)
        runCmd(cmd)

    def _noChoice(self):
        pass
        
_selector = WindowNameSelector()
_selector.activate() # always active
