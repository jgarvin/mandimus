from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, Repeat
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Elements import Integer, Dictation
from collections import OrderedDict
from listHelpers import dictReplace
from rules.emacs.Emacs import Emacs

import string

# taken from https://github.com/schickm/dragonfly-modules/blob/master/chrome.py
alphamapping = OrderedDict([
    ("alpha", "a"),
    ("bravo", "b"),
    ("charlie", "c"),
    ("delta", "d"),
    ("echo", "e"),
    ("foxtrot", "f"),
    ("golf", "g"),
    ("hotel", "h"),
    ("india", "i"),
    ("juliet", "j"),
    ("kilo", "k"),
    ("lima", "l"),
    ("mike", "m"),
    ("november", "n"),
    ("oscar", "o"),
    ("papa", "p"),
    ("quebec", "q"),
    ("romeo", "r"),
    ("sierra", "s"),
    ("tango", "t"),
    ("uniform", "u"),
    ("victor", "v"),
    ("whiskey", "w"),
    ("xray", "x"),
    ("yankee", "y"),
    ("zulu", "z"),
    ])

digitmapping = OrderedDict([
    ("zero", "0"),
    ("one", "1"),
    ("two", "2"),
    ("tree", "3"),
    ("four", "4"),
    ("fife", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("niner", "9"),
    ])

punctuationMapping = OrderedDict([
    ("backslash" , "backslash"),
    ("slash" , "slash"),
    ("exclamation" , "exclamation"),
    ("at" , "at"),
    ("pound" , "hash"),
    ("dollar" , "dollar"),
    ("cash" , "dollar"),
    ("percent" , "percent"),
    ("caret" , "caret"),
    ("ampersand" , "ampersand"),
    ("asterisk" , "asterisk"),
    ("colon" , "colon"),
    ("semicolon" , "semicolon"),
    ("period" , "period"),
    ("dot" , "period"),
    ("swirl" , "comma"), # wtf dragon? can't recognize 'comma'
    ("backtick" , "backtick"),
    ("tilde" , "tilde"),
    ("single quote" , "squote"),
    ("quote" , "dquote"),
    ("less" , "langle"),
    ("greater" , "rangle"),
    ("left angle" , "langle"),
    ("right angle" , "rangle"),
    ("open angle" , "langle"),
    ("close angle" , "rangle"),
    ("left brace" , "lbrace"),
    ("right brace" , "rbrace"),
    ("open brace" , "lbrace"),
    ("close brace" , "rbrace"),
    ("left bracket" , "lbracket"),
    ("right bracket" , "rbracket"),
    ("open bracket" , "lbracket"),
    ("close bracket" , "rbracket"),
    ("left paren" , "lparen"),
    ("right paren" , "rparen"),
    ("open paren" , "lparen"),
    ("close paren" , "rparen"),
    ("hyphen" , "hyphen"),
    ("minus" , "hyphen"),
    ("dash" , "hyphen"),
    ])

directions = ['left', 'right', 'up', 'down']

# TODO: grammar could be much better, 3 controls in a row doesn't make sense
modifierRule = "[control] [alt] [shift]"
possibleLetters = modifierRule + '(' + '|'.join(alphamapping.keys()) + ')'
possibleDigits = modifierRule + '(' + '|'.join(digitmapping.keys()) + ')'
possibleDirections = modifierRule + '(' + '|'.join(directions) + ')'
possiblePunctuation = modifierRule + '(' + '|'.join(punctuationMapping.keys()) + ')'

#print possibleKeyPresses

        
class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift
    
    def __call__(self, extras):
        words = extras['words']
        print 'w: ' + str(words)
        words = words.split()
        words = words[1:] # cut off num/dir/sym
        keystring = []
        foundModifier = True

        if self.force_shift and "shift" not in words:
            words = ["shift"] + words
        
        for i, word in enumerate(words):
            if word in ["control", "alt", "shift"]:
                keystring.append(word[0])
                foundModifier = True
            else:
                break

        if foundModifier:
            keystring.append('-')
                                  
        finalkey = ' '.join(words[i:]) # everything not a modifier
        finalkey = dictReplace(finalkey, dict(alphamapping.items() + digitmapping.items() + punctuationMapping.items()))
        keystring.append(finalkey)
        print keystring
        Key(''.join(keystring))()

# TODO: 'blend' all the active grammars including this one together
# so we always have one nice big series mapping rule and commands
# can chain across grammars
@registerRule
class AlwaysRule(SeriesMappingRule):
    mapping = {
        "command tally"              : (lambda x: Speak(str(commandTally()))()),
        'let ' + possibleLetters     : PressKey(),
        'caplet ' + possibleLetters  : PressKey(force_shift=True),
        'num key ' + possibleDigits  : PressKey(),
        'dir ' + possibleDirections  : PressKey(),
        'sym ' + possiblePunctuation : PressKey(),
        'rep [<n>]'                  : Repeat(),
        'tab'                        : Key("tab"),
        "num <big>"                  : Text("%(big)d"),
    }

    extras = [
        Integer("n", 2, 20),
        Integer("big", 0, 2**14),
        Dictation("text")
        ]
    
    defaults = {
        "n": 1,
        }    

    @classmethod
    def activeForWindow(cls, window):
        return True

class TypingBase(MappingRule):
    extras = [
        Integer("n", 1, 20),
        Dictation("text")
        ]
    
    defaults = {
        "n": 1,
        }    

    @classmethod
    def activeForWindow(cls, window):
        return not Emacs.activeForWindow(window)
    
@registerRule
class TypingRule(TypingBase):
    mapping = {
        "type <text>" : Text("%(text)s", False),
    }

@registerRule
class CamelRule(TypingBase):
    mapping = {
        "camel <text>" : Camel("%(text)s"),
    }

@registerRule
class StudRule(TypingBase):
    mapping = {
        "stud <text>" : Camel("%(text)s", True),
    }

@registerRule
class HyphenRule(TypingBase):
    mapping = {
        "fen <text>"     : Hyphen("%(text)s"),
        "cap fen <text>" : Hyphen("%(text)s", True),
    }    

@registerRule
class UnderscoreRule(TypingBase):
    mapping = {
        "score <text>"     : Underscore("%(text)s"),
        "cap score <text>" : Underscore("%(text)s", True),
    }
    

