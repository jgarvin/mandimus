from Actions import keys
from Mode import Mode

class ChromeMode(Mode):
    def __init__(self):
        Mode.__init__(self)

    @classmethod
    def activationPhrase(cls):
        return "chrome mode"

    @property
    def commands(self):
        c = {
            "new tab" : keys("ctrl+t"),
            "close tab" : keys("ctrl+w"),
            "address" : keys("ctrl+l"),
            "next tab" : keys("ctrl+Tab"),
            "previous tab" : keys("ctrl+shift+Tab"),
            "back" : keys("alt+Left"),
            "forward" : keys("alt+Right"),
            "escape" : keys("Escape")
            }
        return c
