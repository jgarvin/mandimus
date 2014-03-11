import subprocess

class keys:
    def __init__(self, keyStr):
        self.keyStr = keyStr

    def __call__(self):
        # TODO: pay attention to errors, exit status
        cmd = "xdotool key " + self.keyStr
        print "executing: " + cmd
        subprocess.call(cmd, shell=True)
