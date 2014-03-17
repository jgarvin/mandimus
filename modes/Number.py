from Actions import pressKey
from Mode import Mode

class NumberMode(Mode):
    def __init__(self):
        Mode.__init__(self)

    @property
    def commands(self):
        # Based on the NATO phonetic alphabet
        # http://en.wikipedia.org/wiki/NATO_phonetic_alphabet
        c = {
            "zero" : (lambda: self.onDigit(0)),
            "one" : (lambda: self.onDigit(1)),
            "two" : (lambda: self.onDigit(2)),
            "tree": (lambda: self.onDigit(3)),
            "fower" : (lambda: self.onDigit(4)),
            "fife" : (lambda: self.onDigit(5)),
            "six" : (lambda: self.onDigit(6)),
            "seven" : (lambda: self.onDigit(7)),
            "eight" : (lambda: self.onDigit(8)),
            "niner" : (lambda: self.onDigit(9)),
            }
        return c

    # want subclasses to have a hook to override
    def onDigit(self, d):
        pressKey(str(d))
        
