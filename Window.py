import subprocess

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

    def __getXprop(self, prop):
        cmd = "xprop -id " + str(self.winId)
        s = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
        (out, err) = s.communicate()
        out = out.split('\n')

        for line in out:
            x = line.split("=")
            if len(x) < 2:
                continue
            field, value = x[0].strip(), x[1].strip()
            if field == prop:
                return value.strip(',').strip('"')

        return "ERROR: NO %s FIELD" % (prop,)        

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
        return self.__getXprop("WM_NAME(STRING)")

    @property
    def iconName(self):
        return self.__getXprop("WM_ICON_NAME(STRING)")    

    @property
    def wmclass(self):
        return self.__getXprop("WM_CLASS(STRING)")    
    
if __name__ == "__main__":
    w = Window(winId=Window.FOCUSED)
    print w.size, w.name, w.wmclass, w.iconName
