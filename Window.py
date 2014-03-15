import subprocess

def getWindowFocus():
    # TODO: pay attention to errors, exit status
    s = subprocess.Popen("xdotool getwindowfocus", shell=True, stdout=subprocess.PIPE)
    (out, err) = s.communicate()
    return int(out)    

def getWindowDimensions(windowId):
    cmd = "xwininfo -id " + str(windowId)
    s = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
    (out, err) = s.communicate()
    out = out.split()
    width = out[out.index("Width:")+1]
    height = out[out.index("Height:")+1]
    return (int(width), int(height))
