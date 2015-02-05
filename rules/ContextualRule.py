from protocol import makeHashedRule
from Context import Context
from EventLoop import getLoop
from EventList import RuleActivateEvent, RuleDeactivateEvent

class ContexualRule(object):
    def __init__(self, *args, **kwargs):
        self.rule = makeHashedRule(*args, **kwargs)
        self._context = Context(self.rule)

    @property
    def context(self):
        return self._context

    def activate(self):
        getLoop().put(RuleActivateEvent(self.rule))

    def deactivate(self):
        getLoop().put(RuleDeactivateEvent(self.rule))

def makeContexualRule(name, mapping, extras, defaults, ruleType=RuleType.SERIES,
                      seriesMergeGroup=0):
    x = makeHashedRule(ruleType, seriesMergeGroup, name, mapping, extras, defaults)
    x = ContextualRule(x)
    return x
