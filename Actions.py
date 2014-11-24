import mdlog
log = mdlog.getLogger(__name__)

import subprocess
import re
import string
import operator
from listHelpers import dictReplace
from Window import Window, getFocusedWindow
from util import deepEmpty

def runCmd(cmd):
    log.info('cmd: [' + cmd + ']')
    subprocess.call(cmd, shell=True)

def splitKeyString(keyStr):
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
        keys.extend([key] * count)
            
    return keys    

def parseKeyString(keyStr):
    """Translate dragonfly style key descriptions
    to xdotool's preferred versions"""
    
    keys = [parseSingleKeystring(k) for k in splitKeyString(keyStr)]
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
        "left"        : "Left",
        "right"       : "Right",
        "up"          : "Up",
        "down"        : "Down",
        "home"        : "Home",
        "end"         : "End",
        "pgup"        : "Prior",
        "pgdown"      : "Next",
        "enter"       : "Return",
        "backspace"   : "BackSpace",
        "delete"      : "Delete",
        "insert"      : "Insert",
        "backtick"    : "grave",
        "caret"       : "asciicircum",
        "dot"         : "period",
        "dquote"      : "quotedbl",
        "escape"      : "Escape",
        "exclamation" : "exclam",
        "hash"        : "numbersign",
        "hyphen"      : "minus",
        "squote"      : "apostrophe",
        "tilde"       : "asciitilde",
        "langle"      : "less",
        "rangle"      : "greater",
        "lbrace"      : "braceleft",
        "rbrace"      : "braceright",
        "lbracket"    : "bracketleft",
        "rbracket"    : "bracketright",
        "lparen"      : "parenleft",
        "rparen"      : "parenright",
        "tab"         : "Tab",
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
    def __init__(self, data=None):
        self.data = data

    def __add__(self, other):
        return ActionList() + self + other

    def __eq__(self, other):
        return type(self) == type(other) and self.data == other.data

class Repeat(Action):
    # Logic in server
    pass

class Mimic(Action):
    # Logic in server
    pass

class Noop(Action):
    def __call__(self, extras={}):
        pass

class SelectChoice(Action):
    hist = {}
    
    def __init__(self, data, leadingTerm):
        Action.__init__(self, data)
        self.leadingTerm = leadingTerm

    @classmethod
    def history(cls):
        if cls not in cls.hist:
            cls.hist[cls] = []
        return cls.hist[cls]

    def __eq__(self, other):
        return type(self) == type(other) and self.data == other.data
        
    def __call__(self, extras={}):
        words = extras["words"].split()

        if len(words) == 1 and words[0] == self.leadingTerm:
            self._noChoice()
            return

        # TODO: until we get rule references words is going
        # to contain the word for activating the rule, which
        # is not what we want

        # whichever choice matches the most words, choose
        # TODO: would be better if it regarded order
        # TODO: should use percentage matching rather than total
        counter = {}
        for word in words:
            if word not in self.data:
                continue
            for choice in self.data[word]:
                if choice not in counter:
                    counter[choice] = 0    
                counter[choice] += 1

        # get choice that tied on number of words
        counter = counter.items()
        counter.sort(key=lambda x: x[1], reverse=True)
        first = counter[0]
        ties = []
        for c in counter:
            if c[1] == first[1]:
                ties.append(c)
            else:
                break

        bestpick = None
        
        # if there are mulitple equally suitable choices,
        # and one of them is already chosen,
        # pick the one with the next highest ID
        # modulo the number of ties, effectively
        # cycling the choices
        ties.sort(key=self._tieSorter())
        currentChoice = self._currentChoice()
        for i, t in enumerate(ties):
            if t[0] == currentChoice:
                bestpick = ties[(i+1) % len(ties)][0]
                break

        # if none is selected, then rely on history
        if bestpick is None:
            for h in reversed(self.history()):
                for t in ties:
                    #log.info(h,t[0])
                    if h == t[0]:
                        bestpick = h
                        break
                if bestpick is not None:
                    break

        # if all else fails then just pick the first
        if bestpick is None:
            bestpick = ties[0][0]

        self.history().append(bestpick)
        self._select(bestpick)

    def _tieSorter(self):
        return lambda x: x[0]   

    def _currentChoice(self):
        return None

    def _select(self, choice):
        pass

    def _noChoice(self):
        search_max = 10
        curChoice = self._currentChoice()
        for h in list(reversed(self.history()))[:search_max]:
            if h != curChoice:
                self._select(h)
                break

class SelectWindow(SelectChoice):
    def _tieSorter(self):
        return lambda x: x[0].winId

    def _currentChoice(self):
        return getFocusedWindow()

    def _select(self, choice):
        cmd = "xdotool windowactivate %d" % (choice.winId)
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
        ur"<\less-than-sign" : ur"less than sign",
        ur">\greater-than-sign" : ur"greater than sign", 
        ur"=\equals-sign" : ur"equals sign",
        ur"[\left-square-bracket" : ur"left square bracket",
        ur"]\right-square-bracket" : ur"right square bracket",
        ur"{\left-curly-bracket" : ur"left curly bracket",
        ur"}\right-curly-bracket" : ur"right curly bracket",
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
        ur"\backslash" : u"\\\\",
        ur"<\less-than-sign" : ur"<",
        ur">\greater-than-sign" : ur">", 
        ur"=\equals-sign" : ur"=",        
        ur"[\left-square-bracket" : ur"[",
        ur"]\right-square-bracket" : ur"]",
        ur"{\left-curly-bracket" : ur"{",
        ur"}\right-curly-bracket" : ur"}",
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
        self.next_literal = False
        
    def format(self, s):
        new = []
        first = True
        for word in s:
            #log.info('word ' + word)
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
            elif word == ur"literal" and self.do_formatting and not self.next_literal and len(s) > 1:
                self.next_literal = True
            else:
                isCode = word in self.formatting.keys()
                #log.info('isCode: ' + str(isCode))
                newWord = word
                if isCode:
                    if self.do_formatting and not self.next_literal:
                        replacements = self.formatting
                    else:
                        replacements = self.noformatting

                    for key, val in replacements.items():
                        newWord = newWord.replace(key, val)
                    new.append(newWord)
                    #log.info('newWord: ' + newWord)
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

                    # dragonfly doesn't properly filter slashes when
                    # the written and spoken form of a word differ
                    # and the spoken form has spaces in it, e.g.
                    # xdotool -> "ex do tool"
                    # reported: https://github.com/t4ngo/dragonfly/issues/14
                    if newWord.endswith("\\"):
                        newWord = newWord.rstrip("\\")
                        
                    new.append(newWord)
                    first = False
                self.next_literal = False
        return new

def typeKeys(letters):
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
    
def lower(word):
    if isinstance(word, unicode):
        return unicode.lower(word)
    elif isinstance(word, str):
        return str.lower(word)
    raise Exception("Unknown string type.")

def capitalize(word):
    if isinstance(word, unicode):
        return unicode.capitalize(word)
    elif isinstance(word, str):
        return str.capitalize(word)
    raise Exception("Unknown string type.")

# TODO: multiple formatting options, caps stuff is different than
# punctuation
class Text(Action):
    def __init__(self, data, lower=True):
        Action.__init__(self, data)
        self.lower = lower
        
    def __call__(self, extras={}):
        text = self._text(extras)
        log.info(text)
        self._print(text)

    def _print(self, words):
        typeKeys(words)

    def _text(self, extras):
        words = (self.data % extras).split(' ')
        words = FormatState().format(words)
        if self.lower:
            words = [w.lower() for w in words]
        return ''.join(words)
    
class Camel(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = capitalize if caps else lower

    def _text(self, extras):
        words = (self.data % extras).lower().split(' ')
        words = FormatState(formatting=False, spaces=False).format(words)
        words = [w.lower() for w in words]
        words = [self.caps(words[0])] + [w.capitalize() for w in words[1:]]
        return ''.join(words)

class Underscore(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = capitalize if caps else lower

    def _text(self, extras):
        words = (self.data % extras).lower().split(' ')
        words = FormatState(formatting=False, spaces=False).format(words)
        words = [self.caps(w) for w in words]
        return '_'.join(words)        

class Hyphen(Text):
    def __init__(self, fmt, caps=False):
        Text.__init__(self, fmt)
        self.caps = capitalize if caps else lower

    def _text(self, extras):
        words = (self.data % extras).lower().split(' ')
        words = FormatState(formatting=False, spaces=False).format(words)
        words = [self.caps(w) for w in words]
        return '-'.join(words)        

class click:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool click " + str(self.keyStr)
#        log.info("executing: " + cmd)
        subprocess.call(cmd, shell=True)

def moveRelativeToWindow(x, y, windowId):
        cmd = "xdotool mousemove --window %s %s %s" % (windowId, x, y)
#        log.info("executing: " + cmd)
        subprocess.call(cmd, shell=True)
