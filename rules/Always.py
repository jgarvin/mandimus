import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, RepeatPreviousAction
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Elements import Integer, Dictation, RuleRef, Repetition
from collections import OrderedDict
from listHelpers import dictReplace
from rules.emacs.Emacs import Emacs
from rules.BaseRules import AlphaRule, DigitRule, SymRule, CharRule

import string

class PressKey(object):
    def __init__(self, force_shift=False):
        self.force_shift = force_shift
    
    def __call__(self, extras):
        words = extras['words']
        log.info('w: ' + str(words))
        words = words.split()
        keystring = []
        foundModifier = True

        if self.force_shift and "cap" not in words:
            words = ["cap"] + words
        
        numIndex = None
        repetitions = extras['n']
        for i, word in enumerate(words):
            if word in ["control", "alt", "cap"]:
                keystring.append(word[0] if word != "cap" else "s") 
                foundModifier = True
            elif word == "num":
                continue
            else:
                break

        if foundModifier:
            keystring.append('-')
                                  
        finalkey = words[i]
        #log.info('finalkey1: %s' % finalkey)
        finalkey = dictReplace(finalkey, dict(AlphaRule.mapping.items() + DigitRule.mapping.items() + SymRule.mapping.items()))
        #log.info('finalkey2: %s' % finalkey)
        keystring.append(finalkey)
        #log.info("keystring: %s" % keystring)
        for r in range(repetitions):
            Key(''.join(keystring))()

mapping = {
    "command tally"                            : (lambda x: Speak(str(commandTally()))()),
    'rep [<n>]'                                : RepeatPreviousAction(),
    "[control] [alt] [cap] <charrule> [<n>]"   : PressKey(),
    'scoot [<n>]'                              : Key("tab:%(n)d"),
    'cap scoot [<n>]'                          : Key("s-tab:%(n)d"),
}

charref = RuleRef(CharRule, "charrule")

extras = [
    Integer("n", 2, 20),
    Integer("digit", 0, 10),
    Dictation("text"),
    charref,
]

defaults = {
    "n": 1,
}

AlwaysRule = makeHashedRule(RuleType.SERIES, 0, "Always", mapping, extras, defaults)
activateRule(AlwaysRule)

# Need a class that watches what window has focus, and activates/deactivates
# rules based on that.
# Almost all classes need this kind of filtering. How do we do this in the watcher API
# style without a lot of redundancy?

# Need to be able to combine context requirements.

class ContexualRule(object):
    def __init__(self, *args, **kwargs):
        self.rule = makeHashedRule(*args, **kwargs)
        self.context = Context(self.rule)
    def activate(self):
        activateRule(self.rule)
    def deactivate(self):
        deactivateRule(self.rule)

class Context(object):
    def __init__(self, target):
        self.requirements = set()
        self.met = set()
        self.target = target
    
    def addRequirement(self, req):
        self.requirements.add(req)
        req.setContext(self)
        self._maybeFire()

    def met(req):
        assert req in self.requirements
        self.met.add(req)
        self._maybeFire()

    def unmet(req):
        assert req in self.requirements
        self.met.remove(req)
        self._maybeFire()

    def _maybeFire(self):
        if not (self.requirements - self.met):
            self.target.activate()
        else:
            self.target.deactivate()

class Requirement(object):
    def setContext(self, ctx):
        self.context = ctx

class WindowReq(object):
    def __init__(self, wmclass=None, negate=False):
        self.wmclass = wmclass
        self.negate = negate
        getLoop().subscribeEvent(FocusChangeEvent, self.onFocusChange)

    def onFocusChange(self, ev):
        if type(self.wmclass) in (str, unicode):
            self.wmclass = [self.wmclass]
        for c in self.wmclass:
            if c in ev.window.wmclass ^ negate:
                self.context.met(self)

class TypingBase(MappingRule):
    extras = [
        Dictation("text")
        ]
    
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
        "hyphen <text>"     : Hyphen("%(text)s"),
        "cap hyphen <text>" : Hyphen("%(text)s", True),
    }    

@registerRule
class UnderscoreRule(TypingBase):
    mapping = {
        "score <text>"     : Underscore("%(text)s"),
        "cap score <text>" : Underscore("%(text)s", True),
    }
    

