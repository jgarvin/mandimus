from rules.ContextualRule import makeContextualRule
from wordUtils import extractWords
from EventLoop import getLoop, pushEvent
from EventList import WindowListEvent, RuleRegisterEvent, WordListEvent
from protocol import makeHashedRule, RuleType, ListRef, Repetition, RuleRef

class WindowNameManager(object):
    def __init__(self):
        getLoop().subscribeEvent(WindowListEvent, self.onWindowList)
        self.lastWordSet = set()
        self.rule = self.buildRule()

    def buildRule(self):
        mapping = {
            "<window_word>" : None
        }
        extras = [
            ListRef("MasterWindowWordList", "window_word", [])
        ]
        WindowWordRule = makeHashedRule("WindowWordRule", mapping, extras, ruleType=RuleType.INDEPENDENT)
        pushEvent(RuleRegisterEvent(WindowWordRule))

        mapping = {
            "win <words>" : self.onSelection
        }

        WindowWordRuleRef = RuleRef(WindowWordRule, "wordruleref")

        # problem here is I thought dragonfly wanted a rule when it wanted a rule
        # reference... so repetition needs a rule ref, so I have to serialize the
        # name of the extra? nothing else serializes this way!!!
        
        extras = [
            WindowWordRuleRef,
            Repetition(WindowWordRuleRef, 1, 8, "words"),
        ]
        WinRule = makeContextualRule("Win", mapping, extras, ruleType=RuleType.INDEPENDENT)
        WinRule.activate()
        
    def onWindowList(self, ev):
        words = set()
        for w in ev.windows:
            words.update(extractWords(w.name))
        if self.lastWordSet == words:
            return
        lastWordSet = words
        pushEvent(WordListEvent("MasterWindowWordList", words))

    def onSelection(self, extras={}):
        log.info("Got a match! [%s]" % extras)

_mgr = WindowNameManager()
