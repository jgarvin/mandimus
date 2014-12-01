import mdlog
log = mdlog.getLogger(__name__)

from copy import copy
import EventLoop
import EventList

registered_rules = {}

def getName(f):
    return f.__name__

def registeredRules():
    global registered_rules
    return registered_rules

def registerRule(f):
    global registered_rules
    newName = getName(f) 
    
    remove = set()
    for ruleName in registered_rules:
        if newName == ruleName and type(registered_rules[ruleName]) == type(f):
            # you're reregistering the exact same rule
            return

    newRule = f()
    registered_rules[newName] = newRule
    loop = EventLoop.getLoop()
    if loop:
        loop.put(EventList.RuleChangeEvent(newRule))
    return f

def commandTally():
    global registered_rules
    tally = 0
    for s in registered_rules.values():
        tally += len(s.mapping)
    log.info(tally)
    return tally
