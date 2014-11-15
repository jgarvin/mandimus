from copy import copy

registered_rules = set()

def getName(f):
    return f.__name__

def registeredRules():
    global registered_rules
    return registered_rules

def registerRule(f):
    global registered_rules
    remove = set()
    for rule in registered_rules:
        if getName(f) == getName(rule):
            remove.add(rule)
    registered_rules -= remove
    registered_rules.add(f)
    return f

def commandTally():
    global registered_rules
    tally = 0
    for s in registered_rules:
        tally += len(s.mapping)
    print tally
    return tally
