import sys
import signal
import threading
import Queue

from namedtuple import namedtuple

def clamp(a, low, high):
    return min(max(a, low), high)

def roundBelowToZero(x, threshold):
    if abs(x) <= threshold:
        return 0
    return x

class XboxPadSubscription(object):
    AXIS_DEAD_ZONE = 0.1
    TRIGGER_ACTIVATION = 0.5

    def __init__(self, callbackQ):
        self.cbQ = callbackQ

        self.run = True
        self.thread = threading.Thread(target=self)
        self.thread.start()

    def stop(self):
        self.run = False
        self.thread.join()

    @classmethod
    def __getAdjustedAxis(cls, pad, axis):
        return roundBelowToZero(clamp(pad.get_axis(axis), -1.0, 1.0), cls.AXIS_DEAD_ZONE)

    @classmethod
    def __getStickState(cls, pad, axisStart):
        return (cls.__getAdjustedAxis(pad, axisStart), cls.__getAdjustedAxis(pad, axisStart+1))

    @classmethod
    def getStickState(cls, left, pad):
        return cls.__getStickState(pad, 0 if left else 2)

    def __call__(self):
        import pygame

        pygame.init()
        pygame.joystick.init()
        clock = pygame.time.Clock()

        # TODO: make configurable
        xboxpads = [pygame.joystick.Joystick(x) for x in range(pygame.joystick.get_count())]
        oldStickState = [] 
        for i, pad in enumerate(xboxpads):
            print "Initializing joystick %d" % (i,)
            pad.init()

            oldStickState.append({ True : self.getStickState(True, pad),
                                   False : self.getStickState(False, pad)})

        buttons = { 0 : "A",
                    1 : "B",
                    2 : "X",
                    3 : "Y",
                    4 : "LB",
                    5 : "RB",
                    6 : "Back",
                    7 : "Start",
                    8 : "Guide",
                    9 : "RStick",
                    10 : "LStick" }

        ButtonEvent = namedtuple("ButtonEvent", "button down")
        AxisEvent = namedtuple("AxisEvent", "left x y")
        DPadEvent = namedtuple("DPadEvent", "x y")

        oldTriggerState = { 4 : False, 5 : False}

        while self.run:
            evlist = pygame.event.get()
            
            for ev in evlist:
                if ev.type == pygame.QUIT:
                    break
                elif ev.type == pygame.JOYBUTTONUP:
                    self.cbQ.put(ButtonEvent(buttons[ev.button], False))
                elif ev.type == pygame.JOYBUTTONDOWN:
                    self.cbQ.put(ButtonEvent(buttons[ev.button], True))
                elif ev.type == pygame.JOYAXISMOTION:
                    joy = pygame.joystick.Joystick(ev.joy)
                    
                    for stick in [False, True]: 
                        # relay movement if different
                        newStickState = self.getStickState(stick, joy)
                        if not newStickState == oldStickState[joy.get_id()][stick]:
                            self.cbQ.put(AxisEvent(stick, newStickState[0], newStickState[1]))
                            oldStickState[joy.get_id()][stick] = newStickState

                    # technically the trigger buttons are axes, but we just care if they're pressed
                    # for now for simplicity
                    triggerNames = { 4 : "RT", 5 : "LT" }
                    for triggerAxis in [4, 5]:
                        if joy.get_axis(triggerAxis) < self.TRIGGER_ACTIVATION and oldTriggerState[triggerAxis]:
                            self.cbQ.put(ButtonEvent(triggerNames[triggerAxis], False))
                            oldTriggerState[triggerAxis] = False
                        elif joy.get_axis(triggerAxis) >= self.TRIGGER_ACTIVATION and not oldTriggerState[triggerAxis]:
                            self.cbQ.put(ButtonEvent(triggerNames[triggerAxis], True))
                            oldTriggerState[triggerAxis] = True

                elif ev.type == pygame.JOYHATMOTION:
                    self.cbQ.put(DPadEvent(*ev.value))
                else:
                    print "Unknown xbox controller event: %s" % (ev,)

            try:
                clock.tick(20)
            except KeyboardInterrupt:
                break

        pygame.quit()

if __name__ == "__main__":
    q = Queue.Queue()
    sub = XboxPadSubscription(q)
    try:
        while True:
            # without a timeout, ctrl-c doesn't work because.. python
            ONEYEAR = 365 * 24 * 60 * 60
            
            print "getting"
            ev = q.get(True, ONEYEAR)
            
            print str(ev)
    except KeyboardInterrupt:
        print "othe"
        sub.stop()
        sys.exit()
