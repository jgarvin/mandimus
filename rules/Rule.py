registered_rules = set()

def registeredRules():
    global registered_rules
    return registered_rules

def registerRule(f):
    global registered_rules
    registered_rules.add(f)
    return f

def commandTally():
    global registered_rules
    tally = 0
    for s in registered_rules:
        tally += len(s.mapping)
    print tally
    return tally
