class Integer(object):
    def __init__(self, var, lower_bound, upper_bound):
        self.var = var
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound

    def __str__(self):
        return "INTEGER %s %d %d" % (self.var, self.lower_bound, self.upper_bound)

class Dictation(object):
    def __init__(self, var):
        self.var = var

    def __str__(self):
        return "DICTATION %s" % (self.var,)
