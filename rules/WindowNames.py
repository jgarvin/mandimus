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
        getLoop().subscribeEvent(WindowListEvent, self._onWindowList)
        WordSelector.__init__(self, "WindowNames", "win", allowNoChoice=False)
        self.rule.activate() # always on

    def _onWindowList(self, ev):
        self._update(ev.windows)

    def _extractWords(self, n):
        return extractWords(n.name)

    def _currentChoice(self):
        return getFocusedWindow()

    def _select(self, cmd, choice):
        cmd = "xdotool windowactivate %d" % (choice.winId)
        runCmd(cmd)

    def _noChoice(self):
        # Implement toggling here! Use the focus history.
        pass
        
_selector = WindowNameSelector()
_selector.activate() # always active
