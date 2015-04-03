import mdlog
log = mdlog.getLogger(__name__)
log.setLevel(10)

from EventLoop import getLoop
from rules.emacs.Cmd import runEmacsCmd
from EventList import (MicrophoneEvent, ConnectedEvent, DisconnectedEvent, LoadingRulesEvent, EmacsConnectedEvent,
                       RecognitionStateEvent)
import sys

# TODO: should create/tear down in response to connection, more robust than
# resetting internal state
class MicrophoneState(object):
    def __init__(self):
        self.setupState()

    def setupState(self):
        self.micState = "sleeping"
        self.recognitionState = "success"
        self.connected = False
        self.loading = False

    def onMicState(self, ev):
        log.info(sys._getframe(0).f_code.co_name)
        self.micState = ev.state
        self.sendState()

    def onRecognitionState(self, ev):
        log.info(sys._getframe(0).f_code.co_name)
        self.recognitionState = ev.state
        self.sendState()

    def loadingRulesChange(self, ev):
        log.info(sys._getframe(0).f_code.co_name)
        log.info("Mic received state: [%s]" % ev.state)
        self.loading = (ev.state != "done")
        self.sendState()

    def onDisconnect(self, ev):
        log.info(sys._getframe(0).f_code.co_name)
        self.setupState()
        self.sendState()

    def onConnect(self, ev):
        log.info(sys._getframe(0).f_code.co_name)
        self.connected = True
        self.sendState()

    def sendState(self, ev=None):
        log.info(sys._getframe(0).f_code.co_name)
        log.info("connected [%s] loading [%s] state [%s] recog [%s]" % (self.connected, self.loading, self.micState,
                                                                         self.recognitionState))
        
        finalState = None
        if not self.connected or self.loading:
            finalState = "disconnected"
        else:
            if self.micState == "on":
                finalState = self.recognitionState
            else:
                finalState = self.micState
        
        self.tellEmacs(finalState)
        
    def tellEmacs(self, state):
        log.info("mic state: %s" % state)
        runEmacsCmd("(md-new-mic-state \"%s\")" % state)

_state = MicrophoneState()

getLoop().subscribeEvent(MicrophoneEvent, _state.onMicState)
getLoop().subscribeEvent(RecognitionStateEvent, _state.onRecognitionState)
getLoop().subscribeEvent(LoadingRulesEvent, _state.loadingRulesChange)
getLoop().subscribeEvent(ConnectedEvent, _state.onConnect)
getLoop().subscribeEvent(DisconnectedEvent, _state.onDisconnect)
getLoop().subscribeEvent(EmacsConnectedEvent, _state.sendState)
