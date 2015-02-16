from rules.ContextualRule import makeContextualRule
from wordUtils import extractWords
from EventLoop import getLoop, pushEvent
from EventList import WindowListEvent, RuleRegisterEvent, WordListEvent, ConnectedEvent
from protocol import makeHashedRule, RuleType, ListRef, Repetition, RuleRef

class WindowNameManager(object):
    def __init__(self):
        getLoop().subscribeEvent(WindowListEvent, self.onWindowList)
        getLoop().subscribeEvent(ConnectedEvent, self.onWindowList)
        self.rule = self.buildRule()
        self.words = set()

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
            for w in ev.windows:
                self.words.update(extractWords(w.name))
        pushEvent(WordListEvent("MasterWindowWordList", self.words))

    def onSelection(self, extras={}):
        log.info("Got a match! [%s]" % extras)

_mgr = WindowNameManager()
