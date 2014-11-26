import mdlog
log = mdlog.getLogger(__name__)

from EventLoop import getLoop
import traceback
import os
import os.path as op
import subprocess
from Actions import Action

EMACSCLIENT = "timeout 5 emacsclient" # timeout so we don't get stuck blocking
alternative = op.join(os.getenv("HOME"), "opt/bin/emacsclient")
log.info(alternative)
if op.exists(alternative):
    EMACSCLIENT = alternative

_majorMode = None
def updateMajorMode():
    global _majorMode
    _majorMode = runEmacsCmd("major-mode").strip()
    
def getMajorMode():
    return _majorMode

getLoop().onDetermineRules(updateMajorMode)

def runEmacsCmd(command, inFrame=True, dolog=False):
    """Run command optionally in particular frame,
    set True for active frame."""
    args = []
    args += [EMACSCLIENT]
    args += ['-e']
    # without this C-g can interrupt the running code
    # with this any cancels are deferred until after
    #wrapper = "(let ((inhibit-quit t)) %s)"
    wrapper = "%s" # inhibit-quit doesn't seem to work
    if inFrame:
        cmd = '(with-current-buffer %s %s)'
        command = cmd % ("(window-buffer (if (window-minibuffer-p) (active-minibuffer-window) (selected-window)))", command)
    args += [wrapper % command]
    if dolog:
        log.info('emacs cmd: ' + str(args))
    s = subprocess.Popen(args, shell=False,
                         stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)
    (out, err) = s.communicate()
    if err:
        log.info("Emacs error!: " + err)
        traceback.print_stack()
    return out

class Cmd(Action):
    def __init__(self, data=None, log=False):
        Action.__init__(self, data)
        self.log = log
        # self.log = True
    
    def _lisp(self, extras={}):
        fulldata = (self.data % extras)
        return fulldata    

    def __call__(self, extras={}):
        repeat = 1
        if 'n' in extras and isinstance(extras['n'], int):
            repeat = extras['n']
            
        code = self._lisp(extras)
        if code is None:
            if self.log:
                log.info("%s no lisp code" % (type(self).__name__))
            return
        if self.log:
            log.info("%s lisp code: [%s]" % (type(self).__name__, code))
            
        for i in range(repeat):
            runEmacsCmd(code)

getCommandsEl = """
(let ((y '()))
  (mapatoms 
   (lambda (x)
     (and (fboundp x)                    ; does x name a function?
	  (commandp (symbol-function x)) ; is it interactive?
	  (setq y (cons (symbol-name x) y))))) y)
"""

# TODO: this ended up being harder than I thought!
# there are over 1000 unique words in the list, so natlink
# complains that the grammar is too complex. I don't know
# where the threshold is but it will be hard to trim, may
# just need to go with a manual list, or some categories
# like anything with 'python' in the name, maybe have
# a separate "python command" vs. "buffer command" etc.
# for pulling out subsets...
def updateCommandGrammar():
    pass
    # commandlist = getStringList(runEmacsCmd(getCommandsEl))
    # all_words = set()
    # for c in commandlist:
    #     all_words.update(extractWords(c))
    # grammar = []
    # for reps in range(1):
    #     grammar += ['[(']
    #     grammar += ['|'.join(all_words)]
    #     grammar += [')]']
    #     grammar += [' ']
    # updateListGrammar(commandlist, 'command', set(),
    #                   SelectCommand, "EmacsCommandMapping")
            

