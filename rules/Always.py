from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation

import string

# taken from https://github.com/schickm/dragonfly-modules/blob/master/chrome.py
alphamapping = {
    "alpha": "a",
    "bravo": "b",
    "charlie": "c",
    "delta": "d",
    "echo": "e",
    "foxtrot": "f",
    "golf": "g",
    "hotel": "h",
    "india": "i",
    "juliet": "j",
    "kilo": "k",
    "lima": "l",
    "mike": "m",
    "november": "n",
    "oscar": "o",
    "papa": "p",
    "quebec": "q",
    "romeo": "r",
    "sierra": "s",
    "tango": "t",
    "uniform": "u",
    "victor": "v",
    "key": "w",
    "xray": "x",
    "yankee": "y",
    "zulu": "z",
    }

digitmapping = {
    "zero": "0",
    "one": "1",
    "two": "2",
    "tree": "3",
    "four": "4",
    "fife": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "niner": "9",
    }

punctuationMapping = {
    "backslash" : "\\",
    "slash" : "/",
    "forward slash" : "/",
    "exclamation point" : "!",
    "at sign" : "@",
    "pound" : "#",
    "dollar" : "$",
    "cash" : "$",
    "percent" : "%",
    "caret" : "^",
    "ampersand" : "&",
    "asterisk" : "*",
    "colon" : ":",
    "semicolon" : ";",
    "period" : ".",
    "dot" : ".",
    "comma" : ",",
    "tilde" : "~",
    }

directions = ['left', 'right', 'up', 'down']

# TODO: grammar could be much better, 3 controls in a row doesn't make sense
modifierRule = "[(control|alt|shift)] [(control|alt|shift)] [(control|alt|shift)] "
possibleLetters = modifierRule + '(' + '|'.join(alphamapping.keys()) + ')'
possibleDigits = modifierRule + '(' + '|'.join(digitmapping.keys()) + ')'
possibleDirections = modifierRule + '(' + '|'.join(directions) + ')'

#print possibleKeyPresses

class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift
    
    def __call__(self, extras):
        words = extras['words']
        words = words.split()
        modifiers = words[:-1]
        if self.force_shift and "shift" not in modifiers:
            modifiers.append("shift")
        keystring = []
        for word in modifiers:
            if word == "control":
                keystring.append('c')
            elif word == "alt":
                keystring.append('a')
            elif word == "shift":
                keystring.append('s')
        if len(modifiers):
            keystring.append('-')
                      
        finalkey = words[-1]
        for key, val in alphamapping.items() + digitmapping.items():
            finalkey = finalkey.replace(key, val)
        keystring.append(finalkey)
        
        Key(''.join(keystring))()

@registerRule
class AlwaysRule(SeriesMappingRule):
    mapping = {
        "camel <text>" : Camel("%(text)s"),
        "cap camel <text>" : Camel("%(text)s", True),
        "hype <text>" : Hyphen("%(text)s"),
        "cap hype <text>" : Hyphen("%(text)s", True),
        "underscore <text>" : Underscore("%(text)s"),
        "cap underscore <text>" : Underscore("%(text)s", True),
        "type <text>" : Text("%(text)s"),
        "command tally" : (lambda x: Speak(str(commandTally()))()),
        "left [<n>]" : Key("left:%(n)d"),
        "right [<n>]" : Key("right:%(n)d"),
        "up [<n>]" : Key("up:%(n)d"),
        "down [<n>]" : Key("down:%(n)d"),
        "space" : Key("space"),
        "backspace" : Key("backspace"),
        "delete" : Key("delete"),
        'let ' + possibleLetters : PressKey(),
        'cap let ' + possibleLetters : PressKey(force_shift=True),
        'num ' + possibleDigits : PressKey(),
        'dir ' + possibleDirections : PressKey(),
        }

    extras = [
        Integer("n", 1, 20),
        Dictation("text")
        ]
    
    defaults = {
        "n": 1,
        }    

    @classmethod
    def activeForWindow(cls, window):
        return True
