event_loop = None

def getLoop():
    return event_loop

def pushEvent(ev):
    getLoop().put(ev)
