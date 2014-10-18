import subprocess
import re

def runCmd(cmd):
    subprocess.call(cmd, shell=True)

def parseKeyString(keyStr):
    """Translate dragonfly style key descriptions
    to xdotool's preferred versions"""
    
    # dragonfly uses comma sep keys, xdotool uses spaces
    singles = keyStr.split(',')
    
    keys = []
    for s in singles:
        if ':' in s:
            key, count = s.split(':')
            count = int(count)
        else:
            key, count = s, 1

        key = parseSingleKeystring(key)
        keys.extend([key] * count)
            
    return ' '.join(keys)

def parseSingleKeystring(keyStr):
    xdo = []
    modifiers = []
    keys = keyStr.split('-')
    keys = [k.strip() for k in keys]
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

class Speak(Action):
    def __call__(self, extras={}):
        cmd = "echo '" + (self.data % extras) + "' | festival --tts"
        runCmd(cmd)

class Key(Action):
    def __call__(self, extras={}):
        cmd = "xdotool key " + parseKeyString(self.data % extras)
        runCmd(cmd)    

class Text(Action):
    def typeKeys(self, letters):
        # escape single quotes, we actually close the string
        # add the escape single quote, and reopen the string
        letters = letters.replace("'", "'\\''")
        cmd = ("xdotool type '" + letters + "'")
        runCmd(cmd)

    def __call__(self, extras={}):
        self.typeKeys(self.data % extras)

class Camel(Text):
    def __call__(self, extras={}):
        words = self.data % extras
        words = [w.lower() for w in words.split(' ')]
        words = [words[0]] + [w.capitalize() for w in words[1:]]
        self.typeKeys(''.join(words))

class Underscore(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = unicode.upper if caps else unicode.lower

    def __call__(self, extras={}):
        words = self.data % extras
        words = [self.caps(w) for w in words.split(' ')]
        self.typeKeys('_'.join(words))        

class Hyphen(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = unicode.upper if caps else unicode.lower

    def __call__(self, extras={}):
        words = self.data % extras
        words = [self.caps(w) for w in words.split(' ')]
        self.typeKeys('-'.join(words))        

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
