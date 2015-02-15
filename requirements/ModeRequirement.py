from EventList import FocusChangeEvent, MajorModeEvent
from EventLoop import getLoop, pushEvent
from rules.emacs.Cmd import runEmacsCmd, Cmd
from requirements.Requirement import Requirement

def _getEmacsList(x):
    x = x.strip("()")
    return x.split()

_majorMode = None
_oldMajorMode = None
def _updateMajorMode(ev):
    global _majorMode, _oldMajorMode
    _majorMode = runEmacsCmd("(cons major-mode (loop as m = major-mode then p while m as p = (get m 'derived-mode-parent) collect p))")
    _majorMode = _getEmacsList(_majorMode)
    if _majorMode != _oldMajorMode:
        pushEvent(MajorModeEvent(_majorMode))
    _oldMajorMode = _majorMode
    
def _getMajorMode():
    return _majorMode

getLoop().subscribeEvent(FocusChangeEvent, _updateMajorMode, priority=0)

class ModeRequirement(Requirement):
    def __init__(self, modes, contexts=None):
        self.modes = modes
        if type(modes) == str or type(modes) == unicode:
            self.modes = [self.modes]
        Requirement.__init__(self, contexts)
        getLoop().subscribeEvent(MajorModeEvent, self.onModeChange)

    def onModeChange(self, ev):
        matched = False
        for m in ev.modeList:
            if m in self.modes:
                matched = True
                break
        self._met(matched)
