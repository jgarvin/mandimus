from EventLoop import getLoop
from rules.emacs.Cmd import runEmacsCmd
from EventList import MicrophoneEvent

def tellEmacs(event):
    print "mic state: %s" % event.state
    runEmacsCmd("(md-new-mic-state \"%s\")" % event.state, dolog=True)

getLoop().subscribeEvent(MicrophoneEvent, tellEmacs)
