import Queue
import threading

# TODO: use collections.deque instead of queue?
# apparently is lockless

class EventThread(object):
    def __init__(self, pushQ):
        self.pushQ = pushQ

        self.run = True
        self.thread = threading.Thread(target=self)
        self.thread.daemon = True
        self.thread.start()

    def stop(self):
        self.run = False
        self.thread.join()
        
    def __call__(self):
        pass
