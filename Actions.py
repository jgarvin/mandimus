import subprocess
import re

def runCmd(cmd):
    subprocess.call(cmd, shell=True)

def parseKeyString(keyStr):
    """Translate dragonfly style key descriptions
    to xdotool's preferred versions"""
    
    # dragonfly uses comma sep keys, xdotool uses spaces
    singles = keyStr.split(',')
    keys = [parseSingleKeystring(s) for s in singles]
    return ' '.join(keys)

def parseSingleKeystring(keyStr):
    keyStr = keyStr.strip()
    
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
            raise Exception('Unknown modifier: ' + modifier)

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

class ActionList(object):
    def __init__(self, lst=[]):
        self.lst = []
        
    def __add__(self, other):
        if isinstance(other, ActionList):
            self.lst.extend(other.lst)
        else:
            self.lst.append(other)
        return self

    def __call__(self, extras={}):
        for f in self.lst:
            f(extras)

class Action(object):
    def __init__(self, data):
        self.data = data

    def __add__(self, other):
        return ActionList() + self + other

class Key(Action):
    def __init__(self, keyStr):
        Action.__init__(self, parseKeyString(keyStr)) 

    def __call__(self, extras={}):
        cmd = ("xdotool key " + self.data) % extras
        runCmd(cmd)    

class Text(Action):
    def __init__(self, fmt):
        Action.__init__(self, fmt) 

    def __call__(self, extras={}):
        cmd = ("xdotool type '" + self.data + "'") % extras
        runCmd(cmd)

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
