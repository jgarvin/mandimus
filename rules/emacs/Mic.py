import mdlog
log = mdlog.getLogger(__name__)

from EventLoop import getLoop
from rules.emacs.Cmd import runEmacsCmd
from EventList import MicrophoneEvent, ConnectedEvent, DisconnectedEvent, LoadingRulesEvent, EmacsConnectedEvent
import sys

class MicrophoneState(object):
    def __init__(self):
        self.state = "disconnected"
        self.connected = False

    def updateState(self, ev):
        self.state = ev.state
        self.sendState()

    def loadingRulesChange(self, ev):
        self.loading = ev.state
        self.sendState()

    def disconnected(self, ev):
        self.connected = False
        self.sendState()

    def sendState(self, ev=None):
        self.tellEmacs(self.state if (self.connected and not self.loading) else "disconnected")
        
    def tellEmacs(self, state):
        log.debug("mic state: %s" % state)
        runEmacsCmd("(md-new-mic-state \"%s\")" % state)

_state = MicrophoneState()

getLoop().subscribeEvent(MicrophoneEvent, _state.updateState)
getLoop().subscribeEvent(LoadingRulesEvent, _state.loadingRulesChange)
getLoop().subscribeEvent(DisconnectedEvent, _state.disconnected)
getLoop().subscribeEvent(EmacsConnectedEvent, _state.sendState)
