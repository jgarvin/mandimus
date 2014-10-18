from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation

import string

# many missing, good enough for now
possibleKeys = list(set([l.lower() for l in string.letters]))
possibleKeys += list(string.digits)

# taken from https://github.com/schickm/dragonfly-modules/blob/master/chrome.py
keymapping = {
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

possibleKeys = '(' + '|'.join(keymapping.keys()) + ')'
possibleKeyPresses = "[(control|alt|shift)] [(control|alt|shift)] [(control|alt|shift)] " + possibleKeys
print possibleKeyPresses

class PressKey(object):
    def __call__(self, extras):
        words = extras['words']
        words = words.split()
        modifiers = words[:-1]
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
        for key, val in keymapping.items():
            finalkey = finalkey.replace(key, val)
        keystring.append(finalkey)
        
        Key(''.join(keystring))()

@registerRule
class AlwaysRule(SeriesMappingRule):
    mapping = {
        "camel <text>" : Camel("%(text)s"),
        "hyphen <text>" : Hyphen("%(text)s"),
        "underscore <text>" : Underscore("%(text)s"),
        "caps hyphen <text>" : Hyphen("%(text)s", True),
        "caps underscore <text>" : Underscore("%(text)s", True),
        "type <text>" : Text("%(text)s"),
        "command tally" : (lambda x: Speak(str(commandTally()))()),
        "left [<n>]" : Key("left:%(n)d"),
        "right [<n>]" : Key("right:%(n)d"),
        "up [<n>]" : Key("up:%(n)d"),
        "down [<n>]" : Key("down:%(n)d"),
        "space" : Key("space"),
        'press ' + possibleKeyPresses : PressKey(),
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
