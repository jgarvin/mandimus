import mdlog
log = mdlog.getLogger(__name__)

from EventLoop import getLoop
from rules.emacs.Cmd import runEmacsCmd
from EventList import MicrophoneEvent, ConnectedEvent, DisconnectedEvent, StartupCompleteEvent, EmacsConnectedEvent
import sys

class MicrophoneState(object):
    def __init__(self):
        self.state = "disconnected"
        self.connected = False

    def updateState(self, ev):
        self.state = ev.state
        self.tellEmacs(self.state if self.connected else "disconnected")
        if self.state == "sleeping":
            # waking from sleep use to always reload the client, but that got
            # to be annoying and slow
            #self.connected = False
            pass

    def connectionChange(self, ev):
        self.connected = True if isinstance(ev, StartupCompleteEvent) else False
        self.tellEmacs(self.state if self.connected else "disconnected")

    def resendState(self, ev):
        self.tellEmacs(self.state)
        
    def tellEmacs(self, state):
        log.debug("mic state: %s" % state)
        runEmacsCmd("(md-new-mic-state \"%s\")" % state)

_state = MicrophoneState()

getLoop().subscribeEvent(MicrophoneEvent, _state.updateState)
getLoop().subscribeEvent(StartupCompleteEvent, _state.connectionChange, priority=sys.maxint)
getLoop().subscribeEvent(DisconnectedEvent, _state.connectionChange)
getLoop().subscribeEvent(EmacsConnectedEvent, _state.resendState)
