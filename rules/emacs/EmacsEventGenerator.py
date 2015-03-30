import mdlog
log = mdlog.getLogger(__name__)
from rules.WordSelector import WordSelector
from EventLoop import getLoop, pushEvent
from EventList import FocusChangeEvent, EmacsConnectedEvent
from requirements.Emacs import IsEmacs
from requirements.Toggle import Toggle
from Window import getFocusedWindow
from rules.ContextualRule import makeContextualRule
from rules.emacs.Cmd import runEmacsCmd
import rules.emacs.grammar as grammar
from Context import Context

class EmacsEventGenerator(object):
    clsToggle = Toggle()
    clsLogging = False

    def __init__(self, name, cmd, eventType, interval=1):
        self.cmd = cmd
        self.interval = 1
        self.inFrame = True
        self.allowError = False
        self.logging = False
        self.lastOutput = None
        self.eventType = eventType
        
        self.toggle = Toggle()
        self.toggle.enable()

        self.context = Context(set([self]))
        self.context.addRequirement(IsEmacs)
        self.context.addRequirement(self.toggle)
        self.context.addRequirement(EmacsEventGenerator.clsToggle)

        _mapping = {
            "toggle " + name + " generator" : self.toggleEnabled,
            "toggle " + name + " logging" : self.toggleLogging,
        }

        self.toggleRule = makeContextualRule(name + "ToggleRule", _mapping)
        self.toggleRule.activate() # always on

    def activate(self):
        self.subHandles = []
        self.subHandles.append(getLoop().subscribeTimer(self.interval, self.update, priority=0))
        self.subHandles.append(getLoop().subscribeEvent(FocusChangeEvent, self.update, priority=0))
        self.subHandles.append(getLoop().subscribeEvent(EmacsConnectedEvent, self.update, priority=0))

    def deactivate(self):
        for h in self.subHandles:
            h.unsubscribe()

    def update(self, ev=None):
        window = ev.window if ev else getFocusedWindow()
        newOutput = runEmacsCmd(self.cmd, inFrame=self.inFrame,
                                allowError=self.allowError,
                                dolog=(self.logging or self.clsLogging))
        newOutput = self._postProcess(newOutput)

        if newOutput == self.lastOutput:
            return
        self.lastOutput = newOutput
        #log.info("New output!")
        pushEvent(self._makeEvent(newOutput))

    def _makeEvent(self, newOutput):
        return self.eventType(newOutput)

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
    def toggleAllGenerators(cls, ev=None):
        cls.clsToggle.flip()
        log.info("Setting all generators to: %s" % cls.clsToggle.satisfied)

    @classmethod
    def toggleAllGeneratorsLogging(cls, ev=None):
        cls.clsLogging = not cls.clsLogging
        log.info("Setting all generators logging to: %s" % cls.clsLogging)

# on by default
EmacsEventGenerator.clsToggle.enable()

_mapping = {
    "toggle emacs generators" : (lambda x: EmacsEventGenerator.toggleAllGenerators()),
    "toggle logging emacs generators" : (lambda x: EmacsEventGenerator.toggleAllGeneratorsLogging()),
}

MasterGeneratorToggleRule = makeContextualRule("MasterGeneratorToggleRule", _mapping)
MasterGeneratorToggleRule.activate() # always on

