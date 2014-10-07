import subprocess
import re

class typekeys:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        cmd = "xdotool type " + self.keyStr
        subprocess.call(cmd, shell=True)    

def parseKeyString(keyStr):
    """Translate dragonfly style key descriptions
    to xdotool's preferred versions"""
    xdo = []
    modifiers = []
    keys = keyStr.split('-')
    if len(keys) > 1:
        modifiers = keys[0]
        del keys[0]
    keys = ''.join(keys)
    for modifier in modifiers:
        if modifier == 'c':
            xdo += ['ctrl']
        elif modifier == 'a':
            xdo += ['alt']
        elif modifier == 's':
            xdo += ['shift']
        else:
            print 'Unknown modifier'

    replacements = {
        "left" : "Left",
        "right" : "Right",
        "up" : "Up",
        "down" : "Down",
        "home" : "Home",
        "end" : "End",
        "pgup" : "Prior",
        "pgdown" : "Next",
        "enter" : "Return",
        "backspace" : "BackSpace",
        "del" : "Delete",
        "delete" : "Delete",
        "insert" : "Insert",
        "backtick" : "grave",
        "caret" : "asciicircum",
        "dot" : "period",
        "dquote" : "quotedbl",
        "escape" : "Escape",
        "exclamation" : "exclam",
        "hash" : "numbersign",
        "hyphen" : "minus",
        "squote" : "apostrophe",
        "tilde" : "asciitilde",
        "langle" : "less",
        "rangle" : "greater",
        "lbrace" : "braceleft",
        "rbrace" : "braceright",
        "lbracket" : "bracketleft",
        "rbracket" : "bracketright",
        "lparen" : "parenleft",
        "rparen" : "parenright",
        "tab" : "Tab",
    }

    for key, val in replacements.items():
        keys = keys.replace(key, val)
    
    keys = re.sub("f([0-9])+", "F\\1", keys)  
    return '+'.join(xdo) + '+' + keys

class keys:
    def __init__(self, keyStr):
        self.keyStr = parseKeyString(keyStr)

    def __call__(self):
        pressKey(self.keyStr)

def pressKey(key):
    # TODO: pay attention to errors, exit status
    cmd = "xdotool key " + key
    subprocess.call(cmd, shell=True)    

class keydown:
    def __init__(self, keyStr):
        self.keyStr = parseKeyString(keyStr)

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool keydown " + self.keyStr
#        print "executing: " + cmd
        subprocess.call(cmd, shell=True)

class keyup:
    def __init__(self, keyStr):
        self.keyStr = parseKeyString(keyStr)

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool keyup " + self.keyStr
#        print "executing: " + cmd
        subprocess.call(cmd, shell=True)
        
class click:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool click " + str(self.keyStr)
#        print "executing: " + cmd
        subprocess.call(cmd, shell=True)

def moveRelativeToWindow(x, y, windowId):
        cmd = "xdotool mousemove --window %s %s %s" % (windowId, x, y)
#        print "executing: " + cmd
        subprocess.call(cmd, shell=True)
