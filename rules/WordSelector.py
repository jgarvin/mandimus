import mdlog
log = mdlog.getLogger(__name__)

from rules.ContextualRule import makeContextualRule
from wordUtils import extractWords
from EventLoop import getLoop, pushEvent
from EventList import RuleRegisterEvent, WordListEvent, ConnectedEvent, MicrophoneEvent
from protocol import makeHashedRule, RuleType, ListRef, Repetition, RuleRef
from Actions import runCmd
from functools import partial
from Context import Context
from copy import copy

class WordSelector(object):
    def __init__(self, name, cmdWord):
        self.name = name
        self.cmdWord = cmdWord
        getLoop().subscribeEvent(ConnectedEvent, self._sendWords)
        self.rule = self._buildRule()
        # self.words is the list of all the words that should be in
        # the word list sent to dragon
        self.words = set()
        # self.selectionMap is a list of tuples where the first element
        # is a list of words in order that map to the second element in
        # the tuple.
        self.selectionMap = []
        self.activated = False
        self.context = Context(set([self]))

    def _extractWords(self, n):
        return extractWords(n)

    def _update(self, choices):
        self.words = set()
        self.selectionMap = []
        for n in choices:
            w = self._extractWords(n)
            self.words.update(w)
            self.selectionMap.append((w, n))
        self._sendWords()

    @property
    def _wordListRefName(self):
        return self.name + "Word"

    @property
    def _wordListName(self):
        return "Master" + self.name + "List"

    @property
    def _wordRuleName(self):
        return self.name + "WordRule"

    @property
    def _repetitionName(self):
        return self._wordListRefName + "s"

    @property
    def _ruleName(self):
        return self.name + "Rule"

    def _buildRule(self):
        mapping = {
            "<" + self._wordListRefName + ">" : None
        }
        extras = [
            ListRef(self._wordListName, self._wordListRefName, [])
        ]
        WordRule = makeHashedRule(self._wordRuleName, mapping, extras, ruleType=RuleType.INDEPENDENT)
        pushEvent(RuleRegisterEvent(WordRule))

        mapping = {
            ("%s [<%s>]" % (self.cmdWord, self._repetitionName)) : self._onSelection
        }

        extras = [
            Repetition(WordRule, 1, 8, self._repetitionName),
        ]
        r = makeContextualRule(self._ruleName, mapping, extras, ruleType=RuleType.TERMINAL)
        return r
    
    def _onSelection(self, extras={}):
        if not self._repetitionName in extras:
            self._noChoice()
            return

        # Selection process works as follows
        # -Not all words are required to be given, only a subset
        # -But all candidates must include all given words in given order
        # -Consecutive words score higher than separated ones
        # -If a candidate is already selected, we go for the
        # next highest score, cycling if necessary.
        # TODO: need to handle exact matches, they should win
        words = extras[self._repetitionName]["words"]
        candidates = []
        for winWords, window in self.selectionMap:
            totalHoleSize = 0
            lastIdx = None

            # We do the search backwards so say the window name
            # is "file: emacs/emacs.py" and the spoken form is "emacs py"
            # it's probably more likely we meant to match the emacs
            # after the slash rather than the former.
            # TODO: we should just do multiple passes over the list
            # assuming different instances of "emacs" is the start of
            # what's being spoken and take the best total hole size.
            revWinWords = list(reversed(winWords))
            try:
                for w in reversed(words):
                    idx = revWinWords.index(w)
                    if lastIdx is not None:
                        # we subract one because there should be no
                        # penalty if the words are adjacent.
                        totalHoleSize += (idx - lastIdx) - 1
                    lastIdx = idx
                candidates.append((totalHoleSize, window))
            except ValueError:
                # all words that were given must be present
                continue

        if not candidates:
            log.error("No choice with name containing words in order: [%s]" % words)
            pushEvent(MicrophoneEvent("failure"))
            return

        # sort by total hole size
        candidates.sort(key=lambda x: x[0])
        # remove hole sizes leaving just the windows
        candidates = [c[1] for c in candidates]

        # check if the current window is a candidate. if so
        # just cycle through to the next best scoring candidate.
        currentSelection = self._currentChoice()
        try:
            log.info("current selection: [%s] candidates: [%s]" % (currentSelection, candidates))
            idx = candidates.index(currentSelection)
            self._select(candidates[(idx+1) % len(candidates)])
            return
        except ValueError:
            pass

        # otherwise go with the best
        self._select(candidates[0])

    def _sendWords(self, ev=None):
        pushEvent(WordListEvent(self._wordListName, self.words))

    def activate(self):
        log.info("Activating selector [%s]" % type(self))
        self.activated = True
        self._activate()

    def _activate(self):
        pass

    def deactivate(self):
        log.info("Deactivating selector [%s]" % type(self))
        self.activated = False
        self._deactivate()

    def _deactivate(self):
        pass

    def _currentChoice(self):
        return None

    def _select(self, choice):
        assert False

    def _generateList(self, ev=None):
        assert False

    def _noChoice(self):
        assert False

