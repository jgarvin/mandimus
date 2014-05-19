from Actions import keys
from Mode import Mode

class ChromeMode(Mode):
    @property
    def commands(self):
        c = {}
        c.update({
            "new tab" : keys("ctrl+t"),
            "close tab" : keys("ctrl+w"),
            "address" : keys("ctrl+l"),
            "next" : keys("ctrl+Tab"),
            "previous" : keys("ctrl+shift+Tab"),
            "back" : keys("alt+Left"),
            "forward" : keys("alt+Right"),
            "refresh" : keys("F5"),
            "reopen tab" : keys("ctrl+shift+t")
            })
        return c

    def isModeWindow(self, win):
        return "chromium-browser" in win.wmclass.lower()
