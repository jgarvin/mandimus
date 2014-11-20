import traceback
from util import EqualityMixin

class Integer(EqualityMixin):
    def __init__(self, name, lower_bound, upper_bound):
        self.name = name
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound

    def __str__(self):
        return "INTEGER %s %d %d" % (self.name, self.lower_bound, self.upper_bound)

class Dictation(EqualityMixin):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return "DICTATION %s" % (self.name,)

class Repetition(EqualityMixin):
    def __init__(self, child, min=1, max=None, name=None):
        self.child = child
        self.min = min
        self.max = max
        self.name = name
        
    def __str__(self):
        return "REPETITION %s %s %s %s" % (self.child.name, self.min, self.max, self.name)

class RuleRef(EqualityMixin):
    def __init__(self, rule, name):
        self.rule = rule
        self.name = name
        
    def __str__(self):
        return "RULEREF %s %s" % (self.rule.__name__, self.name)

