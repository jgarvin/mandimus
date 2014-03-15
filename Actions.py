import subprocess

class keys:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        pressKey(self.keyStr)

def pressKey(key):
    # TODO: pay attention to errors, exit status
    cmd = "xdotool key " + key
    print "executing: " + cmd
    subprocess.call(cmd, shell=True)    

class keydown:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool keydown " + self.keyStr
        print "executing: " + cmd
        subprocess.call(cmd, shell=True)

class keyup:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool keyup " + self.keyStr
        print "executing: " + cmd
        subprocess.call(cmd, shell=True)
        
class click:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool click " + str(self.keyStr)
        print "executing: " + cmd
        subprocess.call(cmd, shell=True)

def moveRelativeToWindow(x, y, windowId):
        cmd = "xdotool mousemove --window %s %s %s" % (windowId, x, y)
        print "executing: " + cmd
        subprocess.call(cmd, shell=True)
