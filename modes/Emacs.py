from Actions import keys, typekeys
from Mode import Mode

class EmacsMode(Mode):
    @property
    def commands(self):
        c = {}
        c.update({
            "open file" : keys("ctrl+x ctrl+f"),
            "switch" : keys("ctrl+x b"),
            "save" : keys("ctrl+x ctrl+s"),
            "interrupt" : keys("ctrl+g"),
            "search" : keys("ctrl+s"),
            "reverse search" : keys("ctrl+r"),
            "enter" : keys("Return"),
            "unsplit" : keys("ctrl+x 1"),
            "kill" : keys("ctrl+k"),
            "copy" : keys("alt+w"),
            "yank" : keys("ctrl+y"),
            "top" : keys("alt+shift+comma"),
            "bottom" : keys("alt+shift+period"),
            "undo" : keys("ctrl+shift+minus"),
            "redo" : keys("alt+shift+minus")
            })
        return c

    def isModeWindow(self, win):
        termClasses = [ "emacs" ] 
        return any(t in win.wmclass.lower() for t in termClasses)
