import mdlog, os
log = mdlog.getLogger(__name__)

from rules.Elements import Dictation, Integer, RuleRef
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from rules.MappingRule import MappingRule
import rules.BaseRules as BaseRules
from EventLoop import getLoop
from EventList import FocusChangeEvent, EmacsConnectedEvent
from rules.emacs.Text import EmacsText
import rules.ruleUtil as ruleUtil

def getEmacsList(x):
    x = x.strip("()")
    return x.split()

_majorMode = None
def updateMajorMode(ev):
    global _majorMode
    _majorMode = runEmacsCmd("(cons major-mode (loop as m = major-mode then p while m as p = (get m 'derived-mode-parent) collect p))")
    _majorMode = getEmacsList(_majorMode)
    
def getMajorMode():
    return _majorMode

getLoop().subscribeEvent(FocusChangeEvent, updateMajorMode, priority=0)

class KeywordCmd(Cmd):
    def __init__(self, keywords, log=False):
        self.writtenForms = {}
        for i in keywords:
            self.writtenForms[i[1]] = i[0]
        Cmd.__init__(self, None, log)

    def _lisp(self, extras={}):
        words = extras['words'].split()
        command = words[0]
        # everything after command is part of key 
        rest = words[1:]
        if extras['n'] != 1:
            # unless there's a number spoken at the end
            rest = rest[:-1]
        rest = self.writtenForms[" ".join(rest)]
        if command == "key":
            EmacsText("%s" % rest, lower=False)()
        elif command == "new":
            return "(md-insert-snippet \"%s\")" % rest
        elif command == "prior":
            return "(md-go-to-previous \"%s\")" % rest
        elif command == "future":
            return "(md-go-to-next \"%s\")" % rest
        else:
            assert False

@registerRule
class ModeVerbRule(MappingRule):
    refOnly = True
    mapping = {
        "key" : None,
        "new" : None,
        "prior" : None,
        "future" : None,
    }

class EmacsBase(SeriesMappingRule):
    majorMode = None
    keywords = []

    extras = [
        Integer("n", 2, 20),
        Dictation("text"),
        Dictation("match"),
        Dictation("replace"),
        Integer("big", 0, 2**14),
        RuleRef(BaseRules.CharRule, "charrule"),
        RuleRef(BaseRules.AlphaRule, "alpharule"),
        RuleRef(ModeVerbRule, "mode_verb_rule"),
        Integer("i", 0, 10),
    ]

    defaults = {
        "n"    : 1,
        "text" : "",
        }

    def _alwaysOff(self, w):
        return False

    def __init__(self):
        if self.majorMode and self.keywords:
            getLoop().subscribeEvent(EmacsConnectedEvent, self.sendKeywords)

        defaultOperators = [
            ["+", "plus"],
            ["-", "minus"],
            ["*", "times"],
            ["/", "divide"],
            ["%", "mod"],
        ]

        self.keywords.extend([x for x in defaultOperators if x not in self.keywords]) 
        
        if self.keywords:
            self.keywordRule = ruleUtil.buildRuleClass(
                type(self).__name__ + "Keywords", self._alwaysOff, self.keywordDict, orefOnly=True)

            ruleref_name = type(self).__name__.lower()
            self.extras.append(RuleRef(self.keywordRule, ruleref_name))
            self.mapping.update({"<mode_verb_rule> <%s> [<n>]" % ruleref_name : KeywordCmd(self.keywordList)})
            
            registerRule(self.keywordRule)

    def sendKeywords(self, ev):
        if not self.keywords:
            return
        keywordString = "'(" + " ".join([("\"%s\"" % x[0]) for x in self.keywordList]) + ")"
        # let emacs know what can already be written through voice commands
        # so they get filtered from the belts and token lists
        for m in self.modes():
            runEmacsCmd("(md-register-mode-keywords '%s %s)" % (m, keywordString))

    @classmethod
    def modes(cls):
        # a rule can be active in multiple modes by setting majorMode
        # to a list, tuple, etc.
        if type(cls.majorMode) == str or type(cls.majorMode) == unicode:
            return [cls.majorMode]
        return cls.majorMode

    @property
    def keywordDict(self):
        # spoken to written
        return {i[1] : i[0] for i in self.keywordList}

    @property
    def keywordList(self):
        return [x if type(x) != str else (x, x) for x in self.keywords]
    
    @classmethod
    def activeForWindow(cls, window):
        isemacs = "emacs" in window.wmclass or "Emacs" in window.wmclass
        if not isemacs:
            return False
        if cls.majorMode is None or not getMajorMode():
            return True
        mode = getMajorMode()
        for i in cls.modes():
            if i in mode:
                return True
            
            


