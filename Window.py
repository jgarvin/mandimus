import subprocess

class Window:
    FOCUSED = -1

    def __init__(self, winId):
        if winId == self.FOCUSED:
            # TODO: pay attention to errors, exit status
            s = subprocess.Popen("xdotool getwindowfocus", shell=True, stdout=subprocess.PIPE)
            (out, err) = s.communicate()
            self.winId = int(out)    
        else:
            self.winId = winId

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
        cmd = "xprop -id " + str(self.winId)
        s = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
        (out, err) = s.communicate()
        out = out.split('\n')

        for line in out:
            x = line.split("=")
            if len(x) < 2:
                continue
            field, value = x[0].strip(), x[1].strip()
            if field == "WM_NAME(STRING)":
                return value

        return "ERROR: NO WM_NAME FIELD"

if __name__ == "__main__":
    w = Window(winId=Window.FOCUSED)
    print w.size, w.name
