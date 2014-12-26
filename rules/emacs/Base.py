import mdlog, os
log = mdlog.getLogger(__name__)

from rules.Elements import Dictation, Integer, RuleRef
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Cmd import runEmacsCmd
import rules.BaseRules as BaseRules
from EventLoop import getLoop
from EventList import FocusChangeEvent

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

    extras = [
        Integer("n", 2, 20),
        Dictation("text"),
        Dictation("match"),
        Dictation("replace"),
        Integer("big", 0, 2**14),
        RuleRef(BaseRules.CharRule, "charrule"),
    ]

    defaults = {
        "n"    : 1,
        "text" : "",
        }    

    @classmethod
    def activeForWindow(cls, window):
        isemacs = "emacs" in window.wmclass or "Emacs" in window.wmclass
        if not isemacs:
            return False
        if cls.majorMode is None or not getMajorMode():
            return True
        return cls.majorMode in getMajorMode()
