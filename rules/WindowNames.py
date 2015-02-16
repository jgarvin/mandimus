import mdlog
log = mdlog.getLogger(__name__)
from rules.ContextualRule import makeContextualRule
from wordUtils import extractWords
from EventLoop import getLoop, pushEvent
from EventList import WindowListEvent, RuleRegisterEvent, WordListEvent, ConnectedEvent
from protocol import makeHashedRule, RuleType, ListRef, Repetition, RuleRef
from Actions import runCmd
from Window import getFocusedWindow
from functools import partial
from copy import copy
from rules.WordSelector import WordSelector

class WindowNameManager(WordSelector):
    def __init__(self):
        getLoop().subscribeEvent(WindowListEvent, self.onWindowList)
        WordSelector.__init__(self, "WindowNames")

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
        
_mgr = WindowNameManager()
