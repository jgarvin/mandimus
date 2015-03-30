import mdlog
log = mdlog.getLogger(__name__)
log.setLevel(10)

from EventLoop import getLoop
from rules.emacs.Cmd import runEmacsCmd
from EventList import MicrophoneEvent, ConnectedEvent, DisconnectedEvent, LoadingRulesEvent, EmacsConnectedEvent
import sys

class MicrophoneState(object):
    def __init__(self):
        self.state = "disconnected"
        self.connected = False
        self.loading = False

    def updateState(self, ev):
        log.debug(sys._getframe(0).f_code.co_name)
        self.state = ev.state
        self.sendState()

    def loadingRulesChange(self, ev):
        log.debug(sys._getframe(0).f_code.co_name)
        self.loading = False if ev.state == "done" else True
        self.sendState()

    def onDisconnect(self, ev):
        log.debug(sys._getframe(0).f_code.co_name)
        self.connected = False
        self.sendState()

    def onConnect(self, ev):
        log.debug(sys._getframe(0).f_code.co_name)
        self.connected = True
        self.sendState()

    def sendState(self, ev=None):
        log.debug(sys._getframe(0).f_code.co_name)
        log.debug("connected [%s] loading [%s] state [%s]" % (self.connected, self.loading, self.state))
        self.tellEmacs(self.state if (self.connected and not self.loading) else "disconnected")
        
    def tellEmacs(self, state):
        log.info("mic state: %s" % state)
        runEmacsCmd("(md-new-mic-state \"%s\")" % state)

_state = MicrophoneState()

getLoop().subscribeEvent(MicrophoneEvent, _state.updateState)
getLoop().subscribeEvent(LoadingRulesEvent, _state.loadingRulesChange)
getLoop().subscribeEvent(ConnectedEvent, _state.onConnect)
getLoop().subscribeEvent(DisconnectedEvent, _state.onDisconnect)
getLoop().subscribeEvent(EmacsConnectedEvent, _state.sendState)
