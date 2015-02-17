import mdlog
log = mdlog.getLogger(__name__)
from rules.WordSelector import WordSelector
from EventLoop import getLoop
from EventList import FocusChangeEvent
from requirements.Emacs import IsEmacs
from requirements.Toggle import Toggle
from Window import getFocusedWindow
from rules.ContextualRule import makeContextualRule
from rules.emacs.Cmd import runEmacsCmd
import rules.emacs.grammar as grammar

class EmacsSelector(WordSelector):
    clsToggle = Toggle()
    clsLogging = False

    def __init__(self, name, cmdWord, cmd, interval=1):
        WordSelector.__init__(self, name, cmdWord)
        self.cmd = cmd
        self.interval = 1
        self.inFrame = True
        self.allowError = False
        self.logging = False
        self.lastOutput = None
        
        self.toggle = Toggle()
        self.toggle.enable()

        for c in (self.context, self.rule.context):
            c.addRequirement(IsEmacs)
            c.addRequirement(self.toggle)
            c.addRequirement(EmacsSelector.clsToggle)

        _mapping = {
            "toggle " + name + " selector" : self.toggleEnabled,
            "toggle " + name + " logging" : self.toggleLogging,
        }

        self.toggleRule = makeContextualRule(name + "ToggleRule", _mapping)
        self.toggleRule.activate() # always on

    def _activate(self):
        self.subHandles = []
        self.subHandles.append(getLoop().subscribeTimer(self.interval, self.update, priority=0))
        self.subHandles.append(getLoop().subscribeEvent(FocusChangeEvent, self.update, priority=0))

    def _deactivate(self):
        for h in self.subHandles:
            h.unsubscribe()

    def update(self, ev=None):
        window = ev.window if ev else getFocusedWindow()
        log.info("Selector going to run: [%s]" % self.cmd)
        newOutput = runEmacsCmd(self.cmd, inFrame=self.inFrame,
                                allowError=self.allowError,
                                dolog=(self.logging or self.clsLogging))
        newOutput = self._postProcess(newOutput)

        if newOutput == self.lastOutput:
            return
        self.lastOutput = newOutput
        self._update(newOutput)
        self._sendWords()

    def _postProcess(self, output):
        lst = grammar.getStringList(output)
        lst.sort()
        return lst
        
    def toggleLogging(self, ev=None):
        self.logging = not self.logging
        log.info("Setting %s watcher logging to: %s" % (type(self), self.logging))

    def toggleEnabled(self, ev=None):
        self.toggle.flip()
        log.info("Setting %s watcher to: %s" % (type(self), self.toggle.satisfied))

    @classmethod
    def toggleAllSelectors(cls, ev=None):
        cls.clsToggle.flip()
        log.info("Setting all selectors to: %s" % cls.clsToggle.satisfied)

    @classmethod
    def toggleAllSelectorsLogging(cls, ev=None):
        cls.clsLogging = not cls.clsLogging
        log.info("Setting all selectors logging to: %s" % cls.clsLogging)

# on by default
EmacsSelector.clsToggle.enable()

_mapping = {
    "toggle emacs selectors" : (lambda x: EmacsSelector.toggleAllSelectors()),
    "toggle logging emacs selectors" : (lambda x: EmacsSelector.toggleAllSelectorsLogging()),
}

MasterSelectorToggleRule = makeContextualRule("MasterSelectorToggleRule", _mapping)
MasterSelectorToggleRule.activate() # always on

