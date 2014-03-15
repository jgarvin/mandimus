from Actions import pressKey
from Number import NumberMode

class SpellMode(NumberMode):
    def __init__(self):
        NumberMode.__init__(self)

    @classmethod
    def activationPhrase(cls):
        return "spell mode"

    @property
    def commands(self):
        # Based on the NATO phonetic alphabet
        # http://en.wikipedia.org/wiki/NATO_phonetic_alphabet
        c = {}
        c.update(NumberMode.commands.fget(self))
        c.update({
            "alpha" : (lambda: self.onLetter("a")),
            "bravo" : (lambda: self.onLetter("b")),
            "charlie" : (lambda: self.onLetter("c")),
            "delta" : (lambda: self.onLetter("d")),
            "echo" : (lambda: self.onLetter("e")),
            "fokstrot" : (lambda: self.onLetter("f")),
            "golf": (lambda: self.onLetter("g")),
            "hotel" : (lambda: self.onLetter("h")),
            "india" : (lambda: self.onLetter("i")),
            "juliet" : (lambda: self.onLetter("j")),
            "keyloh" : (lambda: self.onLetter("k")),
            "leemah" : (lambda: self.onLetter("l")),
            "mike" : (lambda: self.onLetter("m")),
            "november" : (lambda: self.onLetter("n")),
            "oscar" : (lambda: self.onLetter("o")),
            "papa" : (lambda: self.onLetter("p")),
            "kehbeck" : (lambda: self.onLetter("q")),
            "rowmeoh" : (lambda: self.onLetter("r")),
            "sierra" : (lambda: self.onLetter("s")),
            "tanggo" : (lambda: self.onLetter("t")),
            "uniform" : (lambda: self.onLetter("u")),
            "viktah" : (lambda: self.onLetter("v")),
            "wisskey" : (lambda: self.onLetter("w")),
            "ecksray" : (lambda: self.onLetter("x")),
            "yangkey" : (lambda: self.onLetter("y")),
            "zooloo" : (lambda: self.onLetter("z")),
            })
        return c

    def onLetter(self, l):
        pressKey(l)

    
