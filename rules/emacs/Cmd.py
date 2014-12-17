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
from EventList import FocusChangeEvent, EmacsConnectedEvent
import grammar
import rules.BaseRules as BaseRules
import socket
import errno
from Window import getFocusedWindow

EMACSCLIENT = "timeout 5 emacsclient" # timeout so we don't get stuck blocking
alternative = op.join(os.getenv("HOME"), "opt/bin/emacsclient")
log.info(alternative)
if op.exists(alternative):
    EMACSCLIENT = alternative

logCommands = False
def toggleCommandLogging(*args):
    global logCommands
    logCommands = not logCommands

class CommandClient(object):
    def __init__(self):
        self.sock = None
        self.sock = self.makeSocket()

    def makeSocket(self):
        if self.sock:
            self.sock.close()
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
        self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

    def tryConnect(self):
        if not self.sock:
            self.makeSocket()

        self.sock.settimeout(0.05)
        try:
            self.sock.connect(("localhost", 23233))
            log.info("Connected to emacs!")
            getLoop().put(EmacsConnectedEvent())
            return True
        except socket.error as e:
            log.error("Error connecting to emacs: %s" % e)
        except socket.timeout as e:
            log.error("Connection to emacs timed out.")
        return False

    def dumpOther(self):
        self.sock.close()
        self.sock = None

    def sendMsg(self, msg):
        try:
            self.sock.settimeout(None)
            try:
                self.sock.sendall((msg + "\n").encode('utf-8'))
                return True
            except UnicodeDecodeError as e:
                log.error(str(e))
                return False
        except socket.error as e:
            log.info("Socket error while sending: %s" % e)
            if e.errno == errno.EPIPE or e.errno == errno.EBADF:
                self.dumpOther()
                return False
            else:
                raise
        except Exception as e:
            log.info("Unknown error while sending: %s" % e)
            self.dumpOther()
            raise

    def recvMsg(self):
        self.sock.settimeout(None)
        out = ""

        try:
            while "\n" not in out:
                # print "in recv loop"
                out += unicode(self.sock.recv(4096), 'utf-8')
        except socket.error as e:
            log.info("Socket error while receiving: %s" % e)
            if e.errno == errno.EPIPE or e.errno == errno.EBADF:
                self.dumpOther()
                return False
            else:
                raise
        except Exception as e:
            log.info("Unknown error while receiving: %s" % e)
            self.dumpOther()
            raise
            
        return out

    def runCmd(self, command, inFrame=True, dolog=False, allowError=False):
        """Run command optionally in particular frame,
        set True for active frame."""

        if not self.sock:
            if not self.tryConnect():
                log.error("Can't run command, not connected: [%s]" % command)
                return "nil"

        # have to escape percent signs so python doesn't process them
        command = command.replace("%", "%%")

        # without this C-g can interrupt the running code
        # with this any cancels are deferred until after
        #wrapper = "(let ((md-inhibit-quit t) (inhibit-quit t)) %s)"
        # wrapper = "(let ((redisplay-dont-pause nil) (redisplay-preemption-period nil)) %s)"
        wrapper = "%s" # inhibit-quit doesn't seem to work
        if allowError:
            wrapper %= '(condition-case err %s (error nil))'
        else:
            wrapper %= '(condition-case err %s (error (message (concat "Mandimus error: " (error-message-string err))) nil))'

        if inFrame:
            wrapper %= "(with-current-buffer (window-buffer (if (window-minibuffer-p) (active-minibuffer-window) (selected-window))) %s)"

        command = wrapper % command

        # have to delete newlines since they're the protocol delimeter
        command = command.replace("\n", "")

        if dolog or logCommands:
            log.info('emacs cmd: ' + command)

        self.sock.settimeout(None)
        if not self.sendMsg(command):
            log.info("Couldn't send message: [%s]" % command)
            return "nil"

        out = self.recvMsg()
            
        if dolog or logCommands:
            log.info('emacs output: [%s]' % out)
        return out.rstrip() # delete trailing new line

clientInst = CommandClient()

def runEmacsCmd(command, inFrame=True, dolog=False, allowError=False):
    global clientInst
    return clientInst.runCmd(command, inFrame, dolog, allowError)

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
            

