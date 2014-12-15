from subprocess import call
from EventLoop import getLoop

refreshingEnabled = False

def toggleRefreshClientSources(extras):
    global refreshingEnabled
    refreshingEnabled = not refreshingEnabled

def refreshClientSources(ev=None):
    if refreshingEnabled:
        call("touch ~/dragonshare/NatLink/NatLink/MacroSystem/*.py", shell=True)

getLoop().subscribeTimer(1, refreshClientSources)
