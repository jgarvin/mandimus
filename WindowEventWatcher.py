import mdlog
log = mdlog.getLogger(__name__)

from EventLoop import getLoop
from Window import Window, getWindowList, getFocusedWindow
from namedtuple import namedtuple
from copy import copy
import time
import re

FocusChangeEvent = namedtuple("FocusChangeEvent", "window") 
WindowListEvent = namedtuple("WindowListEvent", "windows") 

REFRESH_TIME = 0.25

class WindowEventWatcher(object):
    def __init__(self, eventQ, filterFunc=lambda x: False):
        self.filterFunc = filterFunc
        self.pushQ = eventQ

        self.previousWindowId = getFocusedWindow().winId
        self.previousWindowName = getFocusedWindow().name
        self.nextWindowList = getWindowList()
        self.previousWindowList = []

        # this is still too much of a perf hog, need real poll
        getLoop().subscribeTimer(REFRESH_TIME, self)

    def extractList(self, windowList):
        return [(w.winId, w.name) for w in windowList]

    def __call__(self):
        windowList = self.nextWindowList.result # force finishing        
        windowList = filter(self.filterFunc, windowList)

        for window in windowList:
            if time.time() - window.lastXpropTime > REFRESH_TIME:
                window.refreshInfo()        

        newWindowList = self.extractList(windowList)
        if self.previousWindowList != newWindowList:
            self.pushQ.put(WindowListEvent(windowList))
        self.previousWindowList = newWindowList

        newWindow = getFocusedWindow()
        if (newWindow and self.previousWindowId != newWindow.winId) or self.previousWindowName != newWindow.name:
            self.pushQ.put(FocusChangeEvent(newWindow))
        self.previousWindowId = newWindow.winId if newWindow else None
        self.previousWindowName = newWindow.name

        self.nextWindowList = getWindowList() # start async

        
if __name__ == "__main__":
    import sys
    import Queue

    q = Queue.Queue()
    sub = WindowEventWatcher(q, {".*Panel.*"})
    try:
        while True:
            # without a timeout, ctrl-c doesn't work because.. python
            ONEYEAR = 365 * 24 * 60 * 60
            ev = q.get(True, ONEYEAR)

            if isinstance(ev, WindowListEvent):
                print [w.name for w in ev.windows if w.name != '']
            else:
                print str(ev)
    except KeyboardInterrupt:
        sub.stop()
        sys.exit()
    
