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

class WindowNameManager(object):
    def __init__(self):
        getLoop().subscribeEvent(WindowListEvent, self.onWindowList)
        getLoop().subscribeEvent(ConnectedEvent, self.onWindowList)
        self.rule = self.buildRule()
        self.words = set()
        self.selectionMap = []

    def buildRule(self):
        mapping = {
            "<winWord>" : None
        }
        extras = [
            ListRef("MasterWindowWordList", "winWord", [])
        ]
        WindowWordRule = makeHashedRule("WindowWordRule", mapping, extras, ruleType=RuleType.INDEPENDENT)
        pushEvent(RuleRegisterEvent(WindowWordRule))

        mapping = {
            "win <winWords>" : self.onSelection
        }

        extras = [
            Repetition(WindowWordRule, 1, 8, "winWords"),
        ]
        WinRule = makeContextualRule("Win", mapping, extras, ruleType=RuleType.INDEPENDENT)
        WinRule.activate()
        
    def onWindowList(self, ev):
        if isinstance(ev, WindowListEvent):
            self.words = set()
            self.selectionMap = []
            for w in ev.windows:
                winWords = extractWords(w.name)
                self.words.update(winWords)
                self.selectionMap.append((winWords, w))
        pushEvent(WordListEvent("MasterWindowWordList", self.words))

    def onSelection(self, extras={}):
        # Selection process works as follows
        # -Not all words are required to be given, only a subset
        # -But all candidates must include all given words in given order
        # -Consecutive words score higher than separated ones
        # -If a candidate is already selected, we go for the
        # next highest score, cycling if necessary.
        words = extras["winWords"]["words"]
        candidates = []
        for winWords, window in self.selectionMap:
            totalHoleSize = 0
            lastIdx = None
            # We do the search backwards so say the window name
            # is "file: emacs/emacs.py" and the spoken form is "emacs py"
            # it's probably more likely we meant to match the emacs
            # after the slash rather than the former. 
            revWinWords = list(reversed(winWords))
            try:
                for w in reversed(words):
                    idx = revWinWords.index(w)
                    if lastIdx is not None:
                        # we subract one because there should be no
                        # penalty if the words are adjacent.
                        totalHoleSize += lastIdx - idx - 1
                    lastIdx = idx
                candidates.append((totalHoleSize, window))
            except ValueError:
                # all words that were given must be present
                continue

        if not candidates:
            log.error("No window with name containing words in order: [%s]" % words)
            pushEvent(MicrophoneEvent("failure"))
            return

        # sort by total hole size
        candidates.sort(key=lambda x: x[0], reverse=True)
        # remove hole sizes leaving just the windows
        candidates = [c[1] for c in candidates]

        # check if the current window is a candidate. if so
        # just cycle through to the next best scoring candidate.
        currentSelection = getFocusedWindow()
        try:
            idx = candidates.index(currentSelection)
            self._select(candidates[(idx+1) % len(candidates)])
            return
        except ValueError:
            pass

        # otherwise go with the best
        self._select(candidates[0])

    def _select(self, choice):
        cmd = "xdotool windowactivate %d" % (choice.winId)
        runCmd(cmd)
        
_mgr = WindowNameManager()
