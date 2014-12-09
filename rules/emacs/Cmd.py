import mdlog
log = mdlog.getLogger(__name__)
log.setLevel(20)

import traceback
import os
import os.path as op
import subprocess

from Actions import Action, Key, Text
import EventLoop
from EventLoop import getLoop
from EventList import FocusChangeEvent
import grammar
import rules.BaseRules as BaseRules

EMACSCLIENT = "timeout 5 emacsclient" # timeout so we don't get stuck blocking
alternative = op.join(os.getenv("HOME"), "opt/bin/emacsclient")
log.info(alternative)
if op.exists(alternative):
    EMACSCLIENT = alternative

logCommands = False
def toggleCommandLogging(*args):
    global logCommands
    logCommands = not logCommands

def runEmacsCmd(command, inFrame=True, dolog=False, allowError=False):
    """Run command optionally in particular frame,
    set True for active frame."""
    args = []
    args += [EMACSCLIENT]
    args += ['-e']

    # have to escape percent signs so python doesn't process them
    command.replace("%", "%%")

    # without this C-g can interrupt the running code
    # with this any cancels are deferred until after
    #wrapper = "(let ((md-inhibit-quit t) (inhibit-quit t)) %s)"
    wrapper = "%s" # inhibit-quit doesn't seem to work
    if allowError:
        wrapper %= '(condition-case err %s (error nil))'
    else:
        wrapper %= '(condition-case err %s (error (message (concat "Mandimus error: " (error-message-string err))) nil))'

    if inFrame:
        cmd = '(with-current-buffer %s %s)'
        command = cmd % ("(window-buffer (if (window-minibuffer-p) (active-minibuffer-window) (selected-window)))", command)

    args += [wrapper % command]
    if dolog or logCommands:
        log.info('emacs cmd: ' + str(args))

    s = subprocess.Popen(args, shell=False,
                         #stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE)
                         #stderr=subprocess.PIPE)
    (out, err) = s.communicate()

    if dolog or logCommands:
        log.info('emacs output: [%s]' % out)
        log.info('emacs error: [%s]' % err)

    if err:
        log.info("Emacs error!: " + err)
        log.error(''.join(traceback.format_stack()))
    return out

class Minibuf(Action):
    def __call__(self, extras={}):
        Key("a-x")()
        Text(self.data % extras)()
        Key("enter")()

class Cmd(Action):
    classLog = False
    
    def __init__(self, data=None, log=False):
        Action.__init__(self, data)
        self.log = log
        # self.log = True
    
    def _lisp(self, extras):
        fulldata = (self.data % extras)
        return fulldata    

    def _repetitions(self, extras={}):
        repeat = 1
        if 'n' in extras and isinstance(extras['n'], int):
            repeat = extras['n']
        return repeat

    def __call__(self, extras={}):
        code = self._lisp(extras)
        if code is None:
            if self.log or self.classLog:
                log.info("%s no lisp code" % (type(self).__name__))
            return
        if self.log or self.classLog:
            log.info("%s lisp code: [%s]" % (type(self).__name__, code))
            
        for i in range(self._repetitions(extras)):
            runEmacsCmd(code)

class CharCmd(Cmd):
    classLog = False
    def _lisp(self, extras={}):
        word = extras['words'].split()[1]
        if word == "num":
            word = extras['words'].split()[2]
        char = BaseRules.CharRule.lookup(word)
        return self.data % char

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
            

class EmacsCommandWatcher(object):
    cmd = None
    allowError = False
    inFrame = True
    eventType = None
    interval = 1
    onTimer = True
    onFocus = True
    
    def __init__(self):
        self.output = None
        if self.onTimer:
            EventLoop.getLoop().subscribeTimer(self.interval, self.update, priority=0)
        if self.onFocus:
            EventLoop.getLoop().subscribeEvent(FocusChangeEvent, self.update, priority=0)

    def _postProcess(self, output):
        lst = grammar.getStringList(output)
        lst.sort()
        return lst

    def update(self, ev=None):
        if self._contextMatch:
            log.debug(self.cmd)
            newOutput = runEmacsCmd(self.cmd, inFrame=self.inFrame, allowError=self.allowError)
            newOutput = self._postProcess(newOutput)
        else:
            newOutput = "nil"

        if newOutput == self.output:
            return
        self.output = newOutput
        EventLoop.getLoop().put(self.eventType(newOutput))

    def _contextMatch(self, window):
        return window and Emacs.activeForWindow(window)
