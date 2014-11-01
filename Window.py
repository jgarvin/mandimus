import subprocess
from collections import defaultdict
import string

#################################
###
### the next bit to do is tie this inte the event loop
### so we always know the currently focused window and
### can use that to trigger mode switches...
### oh yeah i wanted each window to have its own mode stack...
### maybe i still want that

## ok now for simplification we'll just have one mode stack.
## and if you change your mind you'll have to move away and
## then back.
## after this is working and i have some working per window
## commands i can play more with making grammars work.

class XpropException(Exception):
    pass

class Window(object):
    FOCUSED = -1

    def __init__(self, winId=None):
        if winId is None:
            winId = self.FOCUSED
        if winId == self.FOCUSED:
            # TODO: pay attention to errors, exit status
            s = subprocess.Popen("xdotool getwindowfocus", shell=True, stdout=subprocess.PIPE)
            (out, err) = s.communicate()
            try:
                self.winId = int(out)
            except ValueError:
                # no window currently selected!
                self.winId = -1
        else:
            self.winId = winId

        self.xpropJob = Xprop(self.winId)
        self.xpropResult = None

    def __eq__(self, other):
        return self.winId == other.winId

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return self.winId
            
    def __getXprop(self, prop):
        if self.xpropResult is None:
            # force job to finish
            self.xpropResult = self.xpropJob.result         

        for line in self.xpropResult:
            x = line.split("=")
            if len(x) < 2:
                continue
            field, value = x[0].strip(), x[1].strip()
            if field == prop:
                return value.strip(',').strip('"')
        return ""

    @property
    def size(self):
        cmd = "xwininfo -id " + str(self.winId)
        s = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
        (out, err) = s.communicate()
        out = out.split()
        width = out[out.index("Width:")+1]
        height = out[out.index("Height:")+1]
        return (int(width), int(height))

    @property
    def name(self):
        n = self.__getXprop("WM_NAME(STRING)")
        if n == "":
            n = self.__getXprop("WM_NAME(COMPOUND_TEXT)")
        # filter unicode crap
        n = ''.join([c for c in n if c in string.printable])
        return n

    @property
    def hasIcon(self):
        return self.__getXprop("_NET_WM_ICON(CARDINAL)") != ''

    @property
    def iconName(self):
        return self.__getXprop("WM_ICON_NAME(STRING)")    

    @property
    def wmclass(self):
        return self.__getXprop("WM_CLASS(STRING)")
    
    @property
    def role(self):
        return self.__getXprop("WM_WINDOW_ROLE(STRING)")

class Job(object):
    def __init__(self, cmd):
        # async launch command
        self.s = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    @property
    def result(self):
        # force command to finish, return result
        (out, err) = self.s.communicate()
        return self._postprocess(out)

    def _postprocess(self, data):
        return data

class Xprop(Job):
    def __init__(self, winId):
        cmd = "xprop -id " + str(winId)
        Job.__init__(self, cmd)

    def _postprocess(self, data):
        return data.split('\n')

class getWindowList(Job):
    def __init__(self):
        cmd = "xdotool search --onlyvisible '.*'"
        Job.__init__(self, cmd)

    def _postprocess(self, data):
        out = data.split()
        return [Window(int(i)) for i in out]
        
if __name__ == "__main__":
    w = Window(winId=Window.FOCUSED)
    print w.size, w.name, w.wmclass, w.iconName
    print [w.name for w in getWindowList().result if w.name != '']
    print [w.wmclass for w in getWindowList().result if w.name != '']
    print [w.hasIcon for w in getWindowList().result if w.name != '']
