import mdlog
log = mdlog.getLogger(__name__)

import grammar
from rules.emacs.Base import EmacsBase
import EventLoop  
import EventList
from EventList import FocusChangeEvent
from rules.emacs.Cmd import runEmacsCmd 
from Window import getFocusedWindow

class EmacsCommandWatcher(object):
    cmd = None
    allowError = False
    inFrame = True
    eventType = None
    interval = 1
    onTimer = True
    onFocus = True
    
    def __init__(self):
        self.output = None
        if self.onTimer:
            EventLoop.getLoop().subscribeTimer(self.interval, self.update, priority=0)
        if self.onFocus:
            EventLoop.getLoop().subscribeEvent(FocusChangeEvent, self.update, priority=0)

    def _postProcess(self, output):
        lst = grammar.getStringList(output)
        lst.sort()
        return lst

    def update(self, ev=None):
        window = ev.window if ev else getFocusedWindow()
        if self._contextMatch(window):
            log.debug(self.cmd)
            newOutput = runEmacsCmd(self.cmd, inFrame=self.inFrame, allowError=self.allowError)
            newOutput = self._postProcess(newOutput)
        else:
            newOutput = "nil"

        if newOutput == self.output:
            return
        self.output = newOutput
        EventLoop.getLoop().put(self.eventType(newOutput))

    def _contextMatch(self, window):
        return window and EmacsBase.activeForWindow(window)
