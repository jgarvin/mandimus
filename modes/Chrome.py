from Actions import keys
from CUA import CUAMode
from Spell import SpellMode
from Rat import RatMode

class ChromeMode(CUAMode, SpellMode, RatMode):
    def __init__(self):
        CUAMode.__init__(self)
        SpellMode.__init__(self)
        RatMode.__init__(self)

    @property
    def commands(self):
        c = {}
        c.update(CUAMode.commands.fget(self))
        c.update(SpellMode.commands.fget(self))
        c.update(RatMode.commands.fget(self))
        c.update({
            "new tab" : keys("ctrl+t"),
            "close tab" : keys("ctrl+w"),
            "address" : keys("ctrl+l"),
            "next" : keys("ctrl+Tab"),
            "previous" : keys("ctrl+shift+Tab"),
            "back" : keys("alt+Left"),
            "forward" : keys("alt+Right"),
            "dot" : keys("period"),
            "refresh" : keys("F5"),
            "reopen tab" : keys("ctrl+shift+t")
            })
        return c
