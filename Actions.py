import subprocess
import re

def runCmd(cmd):
    print 'cmd: [' + cmd + ']'
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
    xdo.append(keys)
    return '+'.join(xdo)

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
    def __init__(self, data, formatting=True):
        Action.__init__(self, data)
        self.formatting = formatting
        
    def typeKeys(self, letters):
        noformatting = {
            u"\cap" : u"cap",
            u"\caps-on" : u"caps on",
            u"\caps-off" : u"caps off",
            u"\all-caps" : u"all caps",
            u"\no-space" : u"no space",
            u".\period" : u"period",
            u",\comma" : u"comma",
            u"(\left-parenthesis" : u"left parenthesis",
            u")\right-parenthesis" : u"right parenthesis",
            u"\dash" : u"dash",
            u"\hyphen" : u"hyphen",
            u".\point" : u"point",
            u".\dot" : u"dot",
            u"\space-bar" : u"space bar",
            u"\new-line" : u"new line",
            u"?\question-mark" : u"question mark",
            u"!\exclamation-mark" : u"exclamation mark",
            u"@\at-sign" : u"at sign",
            u"#\number-sign" : u"number sign",
            u"$\dollar-sign" : u"dollar sign",
            u"%\percent-sign" : u"percent sign",
            u"~\tilde" : u"tilde",
            u"`\backquote" : u"backquote",
            u"+\plus-sign" : u"plus sign",
            u"\x96\minus-sign" : u"minus sign",
            u"-\minus-sign" : u"minus sign",
            u":\colon" : "colon",
            u";\semicolon" : "semicolon",
            }
        
        formatting = {
            u".\period" : u".",
            u",\comma" : u",",
            u"(\left-parenthesis" : u"(",
            u")\right-parenthesis" : u")",
            u"\x96\dash" : u"-",
            u"-\hyphen" : u"-",
            u".\point" : u".",
            u".\dot" : u".",
            u"\space-bar" : u" ",
            u"\new-line" : u"\n", # hit enter?
            u"?\question-mark" : u"?",
            u"!\exclamation-mark" : u"!",
            u"@\at-sign" : u"@",
            u"#\number-sign" : u"#",
            u"$\dollar-sign" : u"$",
            u"%\percent-sign" : u"%",
            u"~\tilde" : u"~",
            u"`\backquote" : u"`",
            u"+\plus-sign" : u"+",
            u"\x96\minus-sign" : u"-",
            u"-\minus-sign" : u"-",
            u":\colon" : ":",
            u";\semicolon" : ";",
            }

        if self.formatting:
            replacements = formatting
        else:
            replacements = noformatting
        
        for key, val in replacements.items():
            letters = letters.replace(key, val)
        
        # escape single quotes, we actually close the string
        # add the escape single quote, and reopen the string
        letters = letters.replace("'", "'\\''")
        cmd = ("xdotool type '" + letters + "'")
        runCmd(cmd)

    def __call__(self, extras={}):
        self.typeKeys(self.data % extras)        

class Camel(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt, formatting=False)
        self.caps = unicode.capitalize if caps else unicode.lower

    def __call__(self, extras={}):
        words = self.data % extras
        words = [w.lower() for w in words.split(' ')]
        words = [self.caps(words[0])] + [w.capitalize() for w in words[1:]]
        self.typeKeys(''.join(words))

class Underscore(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt, formatting=False)
        self.caps = unicode.upper if caps else unicode.lower

    def __call__(self, extras={}):
        words = self.data % extras
        words = [self.caps(w) for w in words.split(' ')]
        self.typeKeys('_'.join(words))        

class Hyphen(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt, formatting=False)
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
