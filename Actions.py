import subprocess
import re
import string
from listHelpers import dictReplace

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

    keys = dictReplace(keys, replacements)
    
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

class SelectWindow(Action):
    def __call__(self, extras={}):
        cmd = "xdotool windowactivate %d" % (self.data.winId)
        runCmd(cmd)

class Speak(Action):
    def __call__(self, extras={}):
        cmd = "echo '" + (self.data % extras) + "' | festival --tts"
        runCmd(cmd)

class Key(Action):
    def __call__(self, extras={}):
        cmd = "xdotool key " + parseKeyString(self.data % extras)
        runCmd(cmd)    

class FormatState(object):
    noformatting = {
        # format state commands
        ur"\cap" : ur"cap",
        ur"\caps-on" : ur"caps on",
        ur"\caps-off" : ur"caps off",
        ur"\all-caps" : ur"all caps",
        ur"\no-space" : ur"no space",
        ur"\numeral" : ur"numeral",

        # punctuation
        ur".\period" : ur"period",
        ur",\comma" : ur"comma",
        ur"(\left-parenthesis" : ur"left parenthesis",
        ur")\right-parenthesis" : ur"right parenthesis",
        u"\dash" : ur"dash",
        ur"\hyphen" : ur"hyphen",
        ur".\point" : ur"point",
        ur".\dot" : ur"dot",
        ur"\space-bar" : ur"space bar",
        ur"\new-line" : ur"new line",
        ur"?\question-mark" : ur"question mark",
        ur"!\exclamation-mark" : ur"exclamation mark",
        ur"@\at-sign" : ur"at sign",
        ur"#\number-sign" : ur"number sign",
        ur"$\dollar-sign" : ur"dollar sign",
        ur"%\percent-sign" : ur"percent sign",
        ur"~\tilde" : ur"tilde",
        ur"`\backquote" : ur"backquote",
        ur"+\plus-sign" : ur"plus sign",
        u"\x96\\minus-sign" : ur"minus sign",
        ur"-\minus-sign" : ur"minus sign",
        ur":\colon" : ur"colon",
        ur";\semicolon" : ur"semicolon",
        ur"*\asterisk" : ur"asterisk",
        u"_\\underscore" : ur"underscore",
        ur"|\vertical-bar" : ur"vertical bar",
        ur"/\slash" : ur"slash",
        ur"\backslash" : ur"backslash",
        }
    
    formatting = {
        ur".\period" : ur".",
        ur",\comma" : ur",",
        ur"(\left-parenthesis" : ur"(",
        ur")\right-parenthesis" : ur")",
        u"\x96\\dash" : ur"-",
        ur"-\hyphen" : ur"-",
        ur".\point" : ur".",
        ur".\dot" : ur".",
        ur"\space-bar" : ur" ",
        ur"\new-line" : u"\n", # hit enter?
        ur"?\question-mark" : ur"?",
        ur"!\exclamation-mark" : ur"!",
        ur"@\at-sign" : ur"@",
        ur"#\number-sign" : ur"#",
        ur"$\dollar-sign" : ur"$",
        ur"%\percent-sign" : ur"%",
        ur"~\tilde" : ur"~",
        ur"`\backquote" : ur"`",
        ur"+\plus-sign" : ur"+",
        u"\x96\\minus-sign" : ur"-",
        ur"-\minus-sign" : ur"-",
        ur":\colon" : ur":",
        ur";\semicolon" : ur";",
        ur"*\asterisk" : ur"*",
        u"_\\underscore" : ur"_",
        ur"|\vertical-bar" : ur"|",
        ur"/\slash" : ur"/",
        ur"\backslash" : u"\\",
        }

    numeralmap = {
        "zero" : "0",
        "one" : "1",
        "two" : "2",
        "to" : "2",
        "too" : "2",
        "three" : "3",
        "four" : "4",
        "five" : "5",
        "six" : "6",
        "seven" : "7",
        "eight" : "8",
        "nine" : "9",
    }

    def __init__(self, formatting=True, spaces=True):
        self.no_space_once = False
        self.cap_once = False
        self.caps = False
        self.do_formatting = formatting
        self.spacesEnabled = spaces
        self.next_numeral = False

    def format(self, s):
        new = []
        first = True
        for word in s:
            print 'word ' + word
            if word == ur"\cap" and self.do_formatting:
                self.cap_once = True
            elif word == ur"\caps-on" and self.do_formatting:
                self.caps = True
            elif word == ur"\caps-off" and self.do_formatting:
                self.caps = False
            elif word == ur"\no-space" and self.do_formatting:
                self.no_space_once = True
            elif word == ur"\numeral" and self.do_formatting:
                self.next_numeral = True
            else:
                isCode = word in self.formatting.keys()
                print 'isCode: ' + str(isCode)
                newWord = word
                if isCode:
                    if self.do_formatting:
                        replacements = self.formatting
                    else:
                        replacements = self.noformatting

                    for key, val in replacements.items():
                        newWord = newWord.replace(key, val)
                    new.append(newWord)
                    print 'newWord: ' + newWord
                    self.no_space_once = True
                else:
                    if self.cap_once:
                        newWord = word.capitalize()
                        
                    if self.caps:
                        newWord = word.upper()
                        
                    if not self.no_space_once:
                        if not first and self.spacesEnabled:
                            new.append(u' ')
                        self.no_space_once = False

                    if self.next_numeral:
                        if newWord not in string.digits:
                            newWord = self.numeralmap[newWord.lower()]
                        self.next_numeral = False
                    
                    new.append(newWord)
                    first = False
        return new

# TODO: multiple formatting options, caps stuff is different than
# punctuation
class Text(Action):
    def __init__(self, data):
        Action.__init__(self, data)
        
    def typeKeys(self, letters):
        # we pass each character as a separate argument to xdotool,
        # this prevents xdotool from interpreting double hyphens and
        # hyphens followed by words as xdotool flags
        arglist = []
        for l in letters:
            newletter = l
            # single quotes have to be passed unquoted and escaped
            # or the shell gets confused
            if l == '\'':
                newletter = "\\'"
            else:
                newletter = "'" + l + "'"
            arglist.append(newletter)

        letters = ' '.join(arglist)
        cmd = ("xdotool type --clearmodifiers " + letters)
        runCmd(cmd)

    def __call__(self, extras={}):
        words = (self.data % extras).lower().split(' ')
        self._execute(words)

    def _execute(self, words):
        words = FormatState().format(words)
        self.typeKeys(''.join(words))

class Camel(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = unicode.capitalize if caps else unicode.lower

    def _execute(self, words):
        words = FormatState(formatting=False, spaces=False).format(words)
        words = [w.lower() for w in words]
        words = [self.caps(words[0])] + [w.capitalize() for w in words[1:]]
        self.typeKeys(''.join(words))

class Underscore(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = unicode.upper if caps else unicode.lower

    def _execute(self, words):
        words = FormatState(formatting=False, spaces=False).format(words)
        words = [self.caps(w) for w in words]
        self.typeKeys('_'.join(words))        

class Hyphen(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = unicode.upper if caps else unicode.lower

    def _execute(self, words):
        words = FormatState(formatting=False, spaces=False).format(words)
        words = [self.caps(w) for w in words]
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
