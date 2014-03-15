from Actions import keys, keydown, keyup
from Mode import Mode

class CUAMode(Mode):
    def __init__(self):
        Mode.__init__(self)

    @classmethod
    def activationPhrase(cls):
        return None

    @property
    def commands(self):
        c = {
            "up" : keys("Up"),
            "down" : keys("Down"),
            "left" : keys("Left"),
            "right" : keys("Right"),
            "select all" : keys("ctrl+a"),
            "copy" : keys("ctrl+c"),
            "cut" : keys("ctrl+x"),
            "paste" : keys("ctrl+v"),
            "hold shift" : keydown("shift"),
            "release shift" : keyup("shift"),
            "pop" : keys("alt+BackSpace"),
            "backspace" : keys("BackSpace"),
            "delete" : keys("Delete"),
            "enter" : keys("Return"),
            "home" : keys("Home"),
            "end" : keys("End"),
            "escape" : keys("Escape"),
            "space" : keys("space"),
            "page up" : keys("Prior"),
            "page down" : keys("Next"),
            "undo" : keys("ctrl+z"),
            "redo" : keys("ctrl+y")
            }
        return c
