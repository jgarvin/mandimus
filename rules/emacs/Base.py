import mdlog, os
log = mdlog.getLogger(__name__)

from rules.Elements import Dictation, Integer, RuleRef
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Cmd import runEmacsCmd, Cmd
import rules.BaseRules as BaseRules
from EventLoop import getLoop
from EventList import FocusChangeEvent, EmacsConnectedEvent
from rules.emacs.Text import EmacsText

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
    ]

    defaults = {
        "n"    : 1,
        "text" : "",
        }

    def __init__(self):
        if self.majorMode and self.keywords:
            getLoop().subscribeEvent(EmacsConnectedEvent, self.sendKeywords)

        if self.keywords:
            self.mapping.update({"key " + i[1] : EmacsText("%s" % i[0], lower=False) for i in self.keywordList})
            self.mapping.update({"new " + i[1] : Cmd("(md-insert-snippet \"%s\")" % i[0]) for i in self.keywordList})
            self.mapping.update({"prior " + i[1] : Cmd("(md-go-to-previous \"%s\")" % i[0]) for i in self.keywordList})
            self.mapping.update({"future " + i[1] : Cmd("(md-go-to-next \"%s\")" % i[0]) for i in self.keywordList})

    def sendKeywords(self, ev):
        if not self.keywords:
            return
        keywordString = "'(" + " ".join([("\"%s\"" % x[0]) for x in self.keywordList]) + ")"
        # let emacs know what can already be written through voice commands
        # so they get filtered from the belts and token lists
        runEmacsCmd("(md-register-mode-keywords '%s %s)" % (self.majorMode, keywordString))

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
        if type(cls.majorMode) == str or type(cls.majorMode) == unicode:
            return cls.majorMode in getMajorMode()
        else:
            # a rule can be active in multiple modes by setting majorMode
            # to a list, tuple, etc.
            for i in cls.majorMode:
                if i in mode:
                    return True
            
            


