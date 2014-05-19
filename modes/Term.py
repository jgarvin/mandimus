from Actions import keys, typekeys
from Mode import Mode

class TermMode(Mode):
    @property
    def commands(self):
        c = {}
        c.update({
            "interrupt" : keys("ctrl+c"),
            "search" : keys("ctrl+r"),
            "up" : keys("Up"),
            "down" : keys("Down"),
            "enter" : keys("Return"),            
            "exit" : keys("ctrl+d")
            })
        return c

    def isModeWindow(self, win):
        termClasses = [ "xfce4-terminal", "gnome-terminal" ] 
        return any(t in win.wmclass.lower() for t in termClasses)
