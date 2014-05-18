from Window import Window 
from EventThread import EventThread
from namedtuple import namedtuple
import time

FocusChangeEvent = namedtuple("FocusChangeEvent", "window") 


class WindowEventWatcher(EventThread):
    def __call__(self):
        previousWindowId = Window(winId=Window.FOCUSED).winId

        while self.run:
            newWindow = Window(winId=Window.FOCUSED)
            if previousWindowId != newWindow.winId:
                event = FocusChangeEvent(newWindow)
                self.pushQ.put(event)
            previousWindowId = newWindow.winId
            time.sleep(0.05) # 20fps 

if __name__ == "__main__":
    import sys
    import Queue

    q = Queue.Queue()
    sub = WindowEventWatcher(q)
    try:
        while True:
            # without a timeout, ctrl-c doesn't work because.. python
            ONEYEAR = 365 * 24 * 60 * 60
            ev = q.get(True, ONEYEAR)
            print str(ev)
    except KeyboardInterrupt:
        sub.stop()
        sys.exit()
    
