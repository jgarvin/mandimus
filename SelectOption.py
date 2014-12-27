import mdlog
log = mdlog.getLogger(__name__)
log.setLevel(20)

import Actions
import wordUtils
import util
from rules import Rule
from rules.MappingRule import MappingRule 
import rules.ruleUtil as ruleUtil
import EventLoop
import EventList
import Window
from copy import copy

# should handle multiple rules, understanding
# what to index cache by. this way enabling/disabling
# can be done instead of constant regenerating

class SelectOption(Actions.Action):
    leadingTerm = ""
    interval = 1
    eventType = None
    priority = 1
    classLog = False
    
    def __init__(self):
        Actions.Action.__init__(self)
        self.ruleClass = None
        self.choices = []
        self.spokenForms = {}
        self.history = []
        self._registerForUpdates()

    def _registerForUpdates(self):
        if self.eventType:
            EventLoop.getLoop().subscribeEvent(self.eventType, self.updateChoices, priority=self.priority)

    def updateChoices(self, ev):
        window = Window.getFocusedWindow()
        if not self._contextMatch(window):
            return

        newChoices = self._filterChoices(ev.choices)
        if newChoices == self.choices:
            log.debug("No new choices %s" % type(self).__name__)
            return
        
        self.choices = newChoices
        self.__updateRuleClass()

    def _filterChoices(self, choices):
        return choices
        
    def __updateRuleClass(self):
        newSpokenForms = {}
        for c in self.choices:
            newSpokenForms[c] = [self._extractWords(c)]
        if newSpokenForms == self.spokenForms:
            log.debug("Spoken form change check failed.")
            return
        self.spokenForms = newSpokenForms
        omapping = self._buildSelectMapping()

        newRuleClass = ruleUtil.buildRuleClass(type(self).__name__ + "Grammar", self._contextMatch,
                                               omapping)

        log.debug(newRuleClass.mapping)
        
        if self.ruleClass == newRuleClass:
            log.debug("Rule change check failed.")
            return
        #log.info("%s" % newRuleClass())
        self.ruleClass = newRuleClass
        log.debug("Registering rule")
        Rule.registerRule(self.ruleClass)
        
    def _buildSelectMapping(self):
        omapping = {}
        for w, forms in self.spokenForms.items():
            if util.deepEmpty(forms):
                continue
            ogrammar = [self.leadingTerm]
            first = True
            ogrammar += ["("]
            for form in forms:
                if not first:
                    ogrammar += ["|"]
                for word in form:
                    ogrammar.append("[%s]" % word)
                first = False
            ogrammar += [")"]
            ogrammar = ' '.join(ogrammar)
            omapping[ogrammar] = self 
        return omapping

    def _extractWords(self, choice):
        return wordUtils.extractWords(choice)

    def __call__(self, extras={}):
        words = extras["words"].split()

        if len(words) == 1 and words[0] == self.leadingTerm:
            self._noChoice()
            return

        words = words[1:]

        scores = {}
        for k, v in self.spokenForms.items():
            scores[k] = -1
            for form in v:
                if self.classLog:
                    log.info(form)
                # We need a copy because we're going to remove
                # words as we find them, so that we can match
                # better when the same word occurs twice, e.g.
                # foo/foo.py should get matched if you say
                # "foo foo py" instead of rules/foo.py
                f = copy(form)
                matches = 0
                for word in words:
                    if self.classLog:
                        log.info("matches: %f" % matches)
                    # this still ignores relative ordering
                    # of different words
                    if word in f:
                        if self.classLog:
                            log.info("match %s in %s" % (word, f))
                        if matches:
                            matches *= 2.5
                        else:
                            matches = 1
                        f.remove(word)
                score = float(matches) / max(len(form), len(words))
                scores[k] = max(score, scores[k])
                if self.classLog:
                    log.info("Score for [%s] calc: [%f / max(%d, %d)]: %f" % (form, matches, len(form), len(words), score))

        counter = scores.items()
        counter.sort(key=lambda x: x[1], reverse=True)
        log.info(counter)

        first = counter[0]
        ties = []
        for c in counter:
            # not really ties anymore, just everything with at least 1 match
            if c[1] > 0:
                if self.classLog:
                    log.info("adding tie %s" % str(c))
                ties.append(c)
            else:
                break

        bestpick = None
        
        # if there are mulitple equally suitable choices,
        # and one of them is already chosen,
        # pick the one with the next highest ID
        # modulo the number of ties, effectively
        # cycling the choices
        ties.sort(key=self._tieSorter())
        currentChoice = self._currentChoice()
        if self.classLog:
            log.info("sorted ties: %s" % ties)
            log.info("current choice: %s" % currentChoice)
        for i, t in enumerate(ties):
            if self.classLog:
                log.info("comparing %s %s: %s" % (t[0], currentChoice, t[0] == currentChoice))
            if t[0] == currentChoice:
                bestpick = ties[(i+1) % len(ties)][0]
                if self.classLog:
                    log.info("Matched, cycling to %s" % bestpick)
                break

        # if none is selected, then rely on history
        if bestpick is None:
            for h in reversed(self.history):
                for t in ties:
                    #log.info(h,t[0])
                    if h == t[0]:
                        bestpick = h
                        break
                if bestpick is not None:
                    break

        # if all else fails then just pick the first
        if bestpick is None:
            bestpick = ties[0][0]

        self.history.append(bestpick)
        self._select(bestpick)

    def _tieSorter(self):
        return lambda x: x[0]   

    def _currentChoice(self):
        return None

    def _select(self, choice):
        pass

    def _noChoice(self):
        search_max = 10
        curChoice = self._currentChoice()
        for h in list(reversed(self.history))[:search_max]:
            if h != curChoice:
                self._select(h)
                break

    def _contextMatch(self, window):
        return False
