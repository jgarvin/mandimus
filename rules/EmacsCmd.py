import os
import os.path as op
import subprocess
from Actions import Action

EMACSCLIENT = "timeout 5 emacsclient" # timeout so we don't get stuck blocking
alternative = op.join(os.getenv("HOME"), "opt/bin/emacsclient")
print alternative
if op.exists(alternative):
    EMACSCLIENT = alternative

def runEmacsCmd(command, inFrame=True):
    """Run command optionally in particular frame,
    set True for active frame."""
    args = []
    args += [EMACSCLIENT]
    args += ['-e']
    if inFrame:
        cmd = '(with-current-buffer %s %s)'
        command = cmd % ("(window-buffer (selected-window))", command)
    args += [command]
    # print 'emacs cmd: ' + str(args)
    s = subprocess.Popen(args, shell=False,
                         stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)
    (out, err) = s.communicate()
    if err:
        print "Emacs error!: " + err
    return out

class Cmd(Action):
    def _lisp(self, extras={}):
        fulldata = (self.data % extras)
        print fulldata
        return fulldata    

    def __call__(self, extras={}):
        runEmacsCmd(self._lisp(extras))

