#!/usr/bin/env python
# -*- coding: utf-8 -*-

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
from util import enum

# WORDS means that if an object is associated with "foo bar" that
# "foo" and "bar" get added to the word list.
# PHRASES means that if an object is associated with "foo bar" that
# "foo bar" gets added to the word list.
PhraseType = enum(SINGLE_WORD=0, WORDS=1, PHRASES=2, BOTH=3)

class WordSelector(object):
    def __init__(self, name, cmdWords,
                 allowNoChoice=True,
                 phraseType=PhraseType.WORDS,
                 ruleType=RuleType.TERMINAL):
        if type(cmdWords) in (unicode, str):
            self.cmdWords = [cmdWords]
        else:
            self.cmdWords = cmdWords

        self.name = name
        self.ruleType = ruleType
        self.phraseType = phraseType
        self.allowNoChoice = allowNoChoice
        getLoop().subscribeEvent(ConnectedEvent, self._sendWords)
        self.actionRule = None
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
        x = extractWords(n)
        # log.info("[%s] extracted words [%s] [%s]" % (type(self).__name__, n, x))
        return x

    def _update(self, choices):
        self.words = set()
        self.selectionMap = []
        for n in choices:
            w = self._extractWords(n)
            if self.phraseType == PhraseType.SINGLE_WORD or self.phraseType == PhraseType.WORDS or self.phraseType == PhraseType.BOTH:
                # log.info("Adding word: [%s]" % w)
                self.words.update(w)
            if self.phraseType == PhraseType.PHRASES or self.phraseType == PhraseType.BOTH:
                # log.info("Adding phrase: [%s]" % " ".join(w))
                self.words.update([" ".join(w)])
            self.selectionMap.append((w, n))
        log.info("New words [%d] for %s :: %s: [%s]" % (len(self.words), self.name, type(self), self.words))
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
    def _actionRuleName(self):
        return self.name + "ActionRule"

    @property
    def _actionRuleRefName(self):
        return self.name + "ActionRuleRef"

    @property
    def _repetitionName(self):
        return self._wordListRefName + "s"

    @property
    def _ruleName(self):
        return self.name + "Rule"

    def _buildRule(self):
        self.actionRule = self._buildActionRule()
        if self.phraseType == PhraseType.SINGLE_WORD:
            return self._buildSingleWordRule()
        else:
            return self._buildMultiWordRule()

    def _getWords(self, extras):
        if self.phraseType == PhraseType.SINGLE_WORD:
            words = [extras[self._wordListRefName]] if self._wordListRefName in extras else None
        else:
            words = extras[self._repetitionName]["words"] if self._repetitionName in extras else None

        if self.phraseType == PhraseType.PHRASES or self.phraseType == PhraseType.BOTH:
            flatWords = []
            for w in words:
                flatWords.extend(w.split())
            words = flatWords

        return words

    def _buildActionRule(self):
        mapping = {}
        for command in self.cmdWords:
            mapping[command] = None
        ActionRule = makeHashedRule(self._actionRuleName, mapping, ruleType=RuleType.INDEPENDENT)
        pushEvent(RuleRegisterEvent(ActionRule))
        return ActionRule
    
    def _buildSingleWordRule(self):
        wordPart = ("[<%s>]" if self.allowNoChoice else "<%s>") % self._wordListRefName
        
        mapping = {
            (("<%s>" % self._actionRuleRefName) + " " + wordPart) : self._onSelection
        }

        extras = [
            RuleRef(self.actionRule, self._actionRuleRefName),
            ListRef(self._wordListName, self._wordListRefName, []),
        ]
        r = makeContextualRule(self._ruleName, mapping, extras, ruleType=self.ruleType)
        return r

    def _buildMultiWordRule(self):
        mapping = {
            "<" + self._wordListRefName + ">" : None
        }
        extras = [
            ListRef(self._wordListName, self._wordListRefName, [])
        ]
        WordRule = makeHashedRule(self._wordRuleName, mapping, extras, ruleType=RuleType.INDEPENDENT)
        pushEvent(RuleRegisterEvent(WordRule))

        repetitionPart = ("[<%s>]" if self.allowNoChoice else "<%s>") % self._repetitionName
        
        mapping = {
            (("<%s>" % self._actionRuleRefName) + " " + repetitionPart) : self._onSelection
        }

        extras = [
            RuleRef(self.actionRule, self._actionRuleRefName),
            Repetition(WordRule, 1, 5, self._repetitionName),
        ]
        r = makeContextualRule(self._ruleName, mapping, extras, ruleType=self.ruleType)
        return r
    
    def _onSelection(self, extras={}):
        words = self._getWords(extras)
        if not words:
            self._noChoice()
            return

        # Selection process works as follows
        # -Not all words are required to be given, only a subset
        # -But all candidates must include all given words in given order
        # -Consecutive words score higher than separated ones
        # -If a candidate is already selected, we go for the
        # next highest score, cycling if necessary.
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
                lengthDifference = abs(len(winWords) - len(words))
                candidates.append((totalHoleSize, lengthDifference, window))
            except ValueError:
                # all words that were given must be present
                continue

        if not candidates:
            log.error("No choice with name containing words in order: [%s]" % words)
            log.error("selectionMap: [%s]" % self.selectionMap)
            pushEvent(MicrophoneEvent("failure"))
            return

        # sort by total hole size
        #candidates.sort(key=lambda x: (x[0], x[1]))
        candidates.sort()
        log.info("candidates: [%s]" % candidates)

        # remove hole sizes leaving just the windows
        candidates = [c[-1] for c in candidates]

        log.info("selectionMap: [%s]" % self.selectionMap)

        # check if the current window is a candidate. if so
        # just cycle through to the next best scoring candidate.
        currentSelection = self._currentChoice()
        try:
            idx = candidates.index(currentSelection)
            self._select(candidates[(idx+1) % len(candidates)])
            return
        except ValueError:
            pass

        # otherwise go with the best
        self._select(extras[self._actionRuleRefName], candidates[0])

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

    def _select(self, cmd, choice):
        assert False

    def _noChoice(self):
        return None


