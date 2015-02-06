from protocol import makeHashedRule, RuleType
from Context import Context
from EventLoop import getLoop
from EventList import RuleActivateEvent, RuleDeactivateEvent

class ContextualRule(object):
    def __init__(self, rule, context=None):
        self.rule = rule
        if context is None:
            self._context = Context(set([self]))
        else:
            self._context = context
            context.addTarget(self)

    @property
    def context(self):
        return self._context

    def activate(self):
        getLoop().put(RuleActivateEvent(self.rule))

    def deactivate(self):
        getLoop().put(RuleDeactivateEvent(self.rule))

def makeContextualRule(name, mapping, extras=[], defaults={}, ruleType=RuleType.SERIES,
                       seriesMergeGroup=0):
    x = makeHashedRule(name, mapping, extras, defaults, ruleType, seriesMergeGroup)
    return ContextualRule(x)
