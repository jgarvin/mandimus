import mdlog
log = mdlog.getLogger(__name__)
log.setLevel(20)

from EventList import FocusChangeEvent, MajorModeEvent, EmacsConnectedEvent
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
    _majorMode = runEmacsCmd("(md-get-all-modes)")
    _majorMode = _getEmacsList(_majorMode)
    if _majorMode != _oldMajorMode:
        log.info("Switching major mode: {}".format(_majorMode))
        pushEvent(MajorModeEvent(_majorMode))
    _oldMajorMode = _majorMode
    
def _getMajorMode():
    global _majorMode
    return _majorMode

def _clearMajorMode(ev):
    global _oldMajorMode
    _oldMajorMode = None

getLoop().subscribeEvent(EmacsConnectedEvent, _clearMajorMode, priority=0)
getLoop().subscribeEvent(FocusChangeEvent, _updateMajorMode, priority=0)

class ModeRequirement(Requirement):
    def __init__(self, modes, contexts=None):
        self.modes = modes
        if type(modes) == str or type(modes) == unicode:
            self.modes = [self.modes]
        Requirement.__init__(self, contexts)
        getLoop().subscribeEvent(MajorModeEvent, self.onModeChange)

    def onModeChange(self, ev):
        # if you have no mode requirement, you trivially match
        if not self.modes:
            self._met(True)
            return

        matched = False
        for m in ev.modeList:
            if m in self.modes:
                matched = True
                break
        self._met(matched)
