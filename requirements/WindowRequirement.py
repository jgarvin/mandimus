from EventLoop import getLoop
from EventList import FocusChangeEvent
from requirements.Requirement import Requirement

class WindowRequirement(Requirement):
    def __init__(self, contexts=None, wmclass=None, negate=False):
        self.wmclass = wmclass
        self.negate = negate
        Requirement.__init__(self, contexts)
        getLoop().subscribeEvent(FocusChangeEvent, self.onFocusChange)

    def onFocusChange(self, ev):
        if type(self.wmclass) in (str, unicode):
            self.wmclass = [self.wmclass]
        for c in self.wmclass:
            if (c in ev.window.wmclass) ^ self.negate:
                for ctx in self.contexts:
                    ctx.met(self)
