import mdlog
log = mdlog.getLogger(__name__)

from copy import copy

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
        if newName == ruleName:
            # log.info('removing old ' + getName(f))
            remove.add(ruleName)
    for r in remove:
        del registered_rules[r]

    registered_rules[newName] = f()
    return f

def commandTally():
    global registered_rules
    tally = 0
    for s in registered_rules.values():
        tally += len(s.mapping)
    log.info(tally)
    return tally
