from Actions import keys
from Mode import Mode

class XMonadMode(Mode):
    def __init__(self):
        Mode.__init__(self)

    @classmethod
    def activationPhrase(cls):
        return "win mode"

    @property
    def commands(self):
        c = {
            "left" : keys("ctrl+alt+s"),
            "right" : keys("ctrl+alt+h"),
            "move left" : keys("ctrl+alt+a"),
            "move right" : keys("ctrl+alt+t"),
            "next" : keys("ctrl+alt+e"),
            "previous" : keys("ctrl+alt+o"),
            "move next" : keys("ctrl+alt+shift+e"),
            "move previous" : keys("ctrl+alt+shift+o"),
            "expand" : keys("ctrl+alt+i"),
            "shrink" : keys("ctrl+alt+n"),
            "cycle" : keys("ctrl+alt+backslash"),
            "close window" : keys("ctrl+alt+x"),
            "make master" : keys("ctrl+alt+Return"),
            "editor" : keys("ctrl+alt+w"),
            "browser" : keys("ctrl+alt+b")
            }
        return c
        
