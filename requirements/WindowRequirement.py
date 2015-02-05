from EventLoop import getLoop
from EventList import FocusChangeEvent

class WindowRequirement(object):
    def __init__(self, contexts=None, wmclass=None, negate=False):
        self.wmclass = wmclass
        self.negate = negate
        if contexts is None:
            self.contexts = set()
        getLoop().subscribeEvent(FocusChangeEvent, self.onFocusChange)

    def onFocusChange(self, ev):
        if type(self.wmclass) in (str, unicode):
            self.wmclass = [self.wmclass]
        for c in self.wmclass:
            if c in ev.window.wmclass ^ negate:
                for ctx in self.contexts:
                    ctx.met(self)
