from Window import Window, getWindowList
from EventThread import EventThread
from namedtuple import namedtuple
from copy import copy
import time
import re

FocusChangeEvent = namedtuple("FocusChangeEvent", "window") 
WindowListEvent = namedtuple("WindowListEvent", "windows") 

class WindowEventWatcher(EventThread):
    def __init__(self, eventQ, filterFunc=lambda x: False):
        self.filterFunc = filterFunc
        EventThread.__init__(self, eventQ)

    def __call__(self):
        previousWindowId = Window(winId=Window.FOCUSED).winId
        nextWindowList = getWindowList() # start async
        previousWindowList = None

        while self.run:
            newWindow = Window(winId=Window.FOCUSED)
            if previousWindowId != newWindow.winId:
                event = FocusChangeEvent(newWindow)
                self.pushQ.put(event)
            previousWindowId = newWindow.winId

            windowList = nextWindowList.result # force finishing
            windowList = filter(self.filterFunc, windowList)
            if previousWindowList is None or previousWindowList != windowList:
                event = WindowListEvent(copy(windowList))
                self.pushQ.put(event)
            previousWindowList = windowList
            
            nextWindowList = getWindowList() # start async
            time.sleep(0.05) # 20fps 

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
    
