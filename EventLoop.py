event_loop = None

class SubscriptionHandle(object):
    def __init__(self, data):
        self.data = data

    def unsubscribe(self):
        getLoop().unsubscribe(self.data)

def getLoop():
    return event_loop

def pushEvent(ev):
    getLoop().put(ev)
