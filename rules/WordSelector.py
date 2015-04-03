#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mdlog
log = mdlog.getLogger(__name__)

from rules.ContextualRule import makeContextualRule
from wordUtils import extractWords
from EventLoop import getLoop, pushEvent
from EventList import RuleRegisterEvent, WordListEvent, ConnectedEvent, RecognitionStateEvent
from protocol import makeHashedRule, RuleType, ListRef, Repetition, RuleRef
from Actions import runCmd
from functools import partial
from Context import Context
from copy import copy
from util import enum

class WordSelector(object):
    MAX_SUBWORDS = 6

    def __init__(self, name, cmdWords,
                 allowNoChoice=True,
                 ruleType=RuleType.TERMINAL):
        if type(cmdWords) in (unicode, str):
            self.cmdWords = [cmdWords]
        else:
            self.cmdWords = cmdWords

        self.name = name
        self.ruleType = ruleType
        self.allowNoChoice = allowNoChoice
        getLoop().subscribeEvent(ConnectedEvent, self._sendWords)
        self.actionRule = None
        self.wordRule = None
        self.rule = None
        # self.words is a list of lists of sets where:
        # self.words[phraseLength][wordIndex] = set() of words
        # So if "foo bar" is a valid phrase, then it contains:
        # self.words[2][0] == "foo"
        # self.words[2][1] == "bar"
        self.words = []
        # self.selectionMap is a list of tuples where the first element
        # is a list of words in order that map to the second element in
        # the tuple.
        self.selectionMap = []
        self.activated = False
        self.context = Context(set([self]))

        self._buildRule()

    def _extractWords(self, n):
        x = extractWords(n)
         #log.info("[%s] extracted words [%s] [%s]" % (type(self).__name__, n, x))
        return x

    def _update(self, choices):
        self.words = []
        self.selectionMap = []
        allWords = set()
        for n in choices:
            choiceWords = self._extractWords(n)

            # If a phrase is too long, we split up the parts we can
            # and dump the rest in the last list ref. So if the limit
            # is 3 and "FooBarBuzzKillTon" comes in, we get the word
            # list -> ["Foo", "Bar", "Buzz Kill Ton"]
            if len(choiceWords) > self.MAX_SUBWORDS:
                individualWords = choiceWords[:self.MAX_SUBWORDS-1]
                rest = " ".join(choiceWords[self.MAX_SUBWORDS-1:])
                choiceWords = individualWords + [rest]
            
            allWords.update(set(choiceWords))
            while len(choiceWords) > len(self.words):
                self.words.append([])
            while len(choiceWords) > len(self.words[len(choiceWords)-1]):
                self.words[len(choiceWords)-1].append(set())

            for i, w in enumerate(choiceWords):
                self.words[len(choiceWords)-1][i].add(w)
            self.selectionMap.append((choiceWords, n))

        for i in range(len(self.words)):
            for j in range(len(self.words[i])):
                if self.words[i][j]:
                    log.info("%s New words %dx%d len: [%d] set: [%s]" % (self.name, i, j, len(self.words[i][j]), self.words[i][j]))
        log.info("%s word count: [%d]" % (self.name, len(allWords)))
        # log.info("New words [%d] for %s :: %s: [%s]" % (count, self.name, type(self), self.words))
        self._sendWords()

    def _wordListRefName(self, x, y):
        return self.name + "Word" + ("%dx%d" % (x, y))

    def _wordListName(self, x, y):
        return "Master" + self.name + "List" + ("%dx%d" % (x, y))

    @property
    def _wordRuleName(self):
        return self.name + "WordRule"

    @property
    def _wordRuleRefName(self):
        return self.name + "WordRuleRef"

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
    
    def _getWords(self, extras):
        words = []
        if self._wordRuleRefName in extras:
            return extras[self._wordRuleRefName]["words"]
        else:
            return None
    
    def _buildRule(self):                
        mapping = {}
        for command in self.cmdWords:
            mapping[command] = None
        self.actionRule = makeHashedRule(self._actionRuleName, mapping, ruleType=RuleType.INDEPENDENT)
        pushEvent(RuleRegisterEvent(self.actionRule))

        mapping = {}
        extras = []
        for i in range(self.MAX_SUBWORDS):
            for j in range(i+1):
                phrase = []
                for k in range(j, self.MAX_SUBWORDS):
                    optional = (k != j)
                    refString = "<" + self._wordListRefName(i, k) + ">"
                    if optional:
                        refString = "[%s]" % refString
                    phrase.append(refString)
                    extras.append(ListRef(self._wordListName(i, k), self._wordListRefName(i, k), []))
                phrase = " ".join(phrase)
                mapping[phrase] = None                
        
        self.wordRule = makeHashedRule(self._wordRuleName, mapping, extras, ruleType=RuleType.INDEPENDENT)
        pushEvent(RuleRegisterEvent(self.wordRule))

        wordRulePart = "<%s>" % self._wordRuleRefName
        if self.allowNoChoice:
           wordRulePart = "[%s]" % wordRulePart 

        phrase = ("<%s>" % self._actionRuleRefName) + " " + wordRulePart
        mapping = {
            phrase : self._onSelection
        }
        extras = [
            RuleRef(self.actionRule, self._actionRuleRefName),
            RuleRef(self.wordRule, self._wordRuleRefName),
        ]

        self.rule = makeContextualRule(self._ruleName, mapping, extras, ruleType=self.ruleType)

        log.info("new crazy rule: [%s]" % self.rule.rule.rule.mapping)
        log.info("new crazy rule extras: [%s]" % self.rule.rule.rule.extras)
    
    def _onSelection(self, extras={}):
        words = self._getWords(extras)
        if not words:
            if self.allowNoChoice:
                self._noChoice()
            else:
                log.error("Command [%s] must be used with a choice." % extras[self._actionRuleRefName]["words"])
                pushEvent(RecognitionStateEvent("failure"))
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
                    if lastIdx:
                        # start at index we last left off at so we can
                        # enforce word order
                        idx = revWinWords.index(w, lastIdx)
                    else:
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
            pushEvent(RecognitionStateEvent("failure"))
            return

        # sort by total hole size
        #candidates.sort(key=lambda x: (x[0], x[1]))
        candidates.sort()
        log.debug("candidates: [%s]" % candidates)

        # remove hole sizes leaving just the windows
        candidates = [c[-1] for c in candidates]

        log.debug("selectionMap: [%s]" % self.selectionMap)

        # check if the current window is a candidate. if so
        # just cycle through to the next best scoring candidate.
        currentSelection = self._currentChoice()
        try:
            idx = candidates.index(currentSelection)
            self._select(extras[self._actionRuleRefName], candidates[(idx+1) % len(candidates)])
            return
        except ValueError:
            pass

        # otherwise go with the best
        self._select(extras[self._actionRuleRefName], candidates[0])

    def _sendWords(self, ev=None):
        # We want to iterate the whole possible space, not just what's present,
        # because we want to initialize empty word lists to empty in case there
        # are left over values from previous runs of the server.
        for i in range(self.MAX_SUBWORDS):
            for j in range(i+1):
                try:
                    words = self.words[i][j]
                except IndexError:
                    words = []
                pushEvent(WordListEvent(self._wordListName(i, j), words))

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



