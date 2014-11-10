from EventLoop import getLoop
from Window import Window, getWindowList, getFocusedWindow
from namedtuple import namedtuple
from copy import copy
import time
import re

FocusChangeEvent = namedtuple("FocusChangeEvent", "window") 
WindowListEvent = namedtuple("WindowListEvent", "windows") 

class WindowEventWatcher(object):
    def __init__(self, eventQ, filterFunc=lambda x: False):
        self.filterFunc = filterFunc
        self.pushQ = eventQ

        self.previousWindowId = getFocusedWindow().winId
        self.nextWindowList = getWindowList() # start async
        self.previousWindowList = None

        # this is still too much of a perf hog
        # TODO: getWindowList should reuse window objects, or
        # Window should be smart enough to.
        getLoop().subscribeTimer(0.25, self)
        
    def __call__(self):
        newWindow = getFocusedWindow()
        if self.previousWindowId != newWindow.winId:
            self.pushQ.put(FocusChangeEvent(newWindow))
        self.previousWindowId = newWindow.winId

        windowList = self.nextWindowList.result # force finishing
        windowList = filter(self.filterFunc, windowList)
        if self.previousWindowList is None or self.previousWindowList != windowList:
            self.pushQ.put(WindowListEvent(copy(windowList)))
        self.previousWindowList = windowList

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
    
