from Rule import registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Actions import (
    Key, Text, Camel, Underscore, Hyphen, Speak, Action, runCmd, SelectChoice, Mimic,
    splitKeyString, FormatState, ActionList)
from Elements import Integer, Dictation
from Window import Window, getFocusedWindow
from EventLoop import getLoop
from wordUtils import extractWords, buildSelectMapping
from Events import GrammarEvent
from util import deepEmpty

from copy import copy
import re
import subprocess
import os
import os.path as op

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
        cmd = '(with-current-buffer "%s" %s)'
        if inFrame is True:
            command = cmd % (getFocusedWindow().name, command)
        else:
            command = cmd % (inFrame, command)
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

sexpFuncs = {
    "forward"            : "sp-forward-sexp",
    "backward"           : "sp-backward-sexp",
    "in"                 : "sp-down-sexp",
    "backward in"        : "sp-backward-down-sexp",
    "out"                : "sp-up-sexp",
    "backward out"       : "sp-backward-up-sexp",
    "next"               : "sp-next-sexp",
    "previous"           : "sp-previous-sexp",
    "beginning"          : "sp-beginning-of-sexp",
    "beginning next"     : "sp-beginning-of-next-sexp",
    "beginning previous" : "sp-beginning-of-previous-sexp",
    "end next"           : "sp-end-of-next-sexp",
    "end previous"       : "sp-end-of-previous-sexp",
    "select [next]"      : "sp-select-next-thing",
    "select previous"    : "sp-select-previous-thing",
}
        
sexpPairs = {
    "paren"        : "(",
    "brace"        : "{",
    "bracket"      : "[",
    "quote"        : "\"",
    "single quote" : "'",
    "angle"        : "<",
}


def bufferList():
    buffs = runEmacsCmd("(mapcar 'buffer-name (buffer-list))", inFrame=False)
    return getStringList(buffs)

def getStringList(output):
    output = re.findall('"[^"]*"', output)
    output = [x.strip().strip('"') for x in output]
    return output    

def currentBuffer():
    buf = runEmacsCmd("(buffer-name (current-buffer))")
    return buf.strip().strip('"')

def showBuffer(buf):
    runEmacsCmd("(switch-to-buffer \"%s\")" % buf)        

class SelectBuffer(SelectChoice):
    def _currentChoice(self):
        return currentBuffer()

    def _select(self, choice):
        showBuffer(choice)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)")                

class SelectCommand(SelectChoice):
    def _currentChoice(self):
        return None

    def _select(self, choice):
        runEmacsCmd("(call-interactively '%s)" % choice)

    def _noChoice(self):
        Key("c-x,c-m")()

def updateListGrammar(lst, leadingTerm, translate, action, clsname):
    bufs = lst
    spokenForms = {}
    for b in bufs:
        spokenForms[b] = [set(extractWords(b, translate=translate))] 
    omapping = buildSelectMapping(leadingTerm, spokenForms, action)
    class LocalMapping(MappingRule):
        mapping = omapping
    LocalMapping.__name__ = clsname
    #print omapping.keys()
    getLoop().put(GrammarEvent(True, LocalMapping))

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

def updateBufferGrammar():
    updateListGrammar(bufferList(), 'buff', {'*'},
                      SelectBuffer, "EmacsBufferMapping")    

getLoop().subscribeTimer(1, updateBufferGrammar)
getLoop().subscribeTimer(10, updateCommandGrammar)
updateCommandGrammar()

#

# We make an interactive function that does the combination of commands

"""
(defun test-ohtse ()
  (interactive)
  (execute-kbd-macro (kbd "jj"))
  (execute-kbd-macro (kbd "kk")))
(test-ohtse)
"""

class Cmd(Action):
    # def __add__(self, other):
    #     return CmdList() + self + other

    def _lisp(self, extras={}):
        fulldata = (self.data % extras)
        print fulldata
        return fulldata    

    def __call__(self, extras={}):
        runEmacsCmd(self._lisp(extras))

actionFunc = """
(defun mandimus-temp-function ()
  (interactive)
  %s
  )
(mandimus-temp-function)
"""            

class CmdList(ActionList):
    def __call__(self, extras={}):
        """By generating one big temporary function that does all the actions together, we
        get the intuitive behavior. If emacs and non-emacs actions are mixed, we only group
        consecutive emacs actions, since executing the actions out of order could have
        unknown consequences."""

        lstcopy = copy(self.lst)
        actions = []

        def publish():
            if not actions:
                return

            lisp = "\n  ".join([f._lisp(extras) for f in actions])
            cmd = actionFunc % lisp
            actions[:] = []
            print cmd
            runEmacsCmd(cmd)
        
        while lstcopy:
            f = lstcopy.pop(0)
            
            if isinstance(f, Cmd):
                actions.append(f)
                continue
            else:
                publish()
                f(extras)
        publish()
        
class PairCmd(Cmd):
    def __init__(self, pair, cmd):
        x = "(single-pair-only-sexp \"%s\" '%s)" % (pair, cmd)
        Cmd.__init__(self, x)        

class EmacsKey(Cmd):
    replacements = {
        "underscore" : "_",
        "comma"      : ",",
        "enter"      : "<return>",
        "percent"    : "%",
        "langle"     : "<",
        "rangle"     : ">",
        "space"      : "SPC",
        "up"         : "<up>",
        "down"       : "<down>",
        "left"       : "<left>",
        "right"      : "<right>",
        "F0"         : "<f0>",
        "F1"         : "<f1>",
        "F2"         : "<f2>",
        "F3"         : "<f3>",
        "F4"         : "<f4>",
        "F5"         : "<f5>",
        "F6"         : "<f6>",
        "F7"         : "<f7>",
        "F8"         : "<f8>",
        "F9"         : "<f9>",
        "F10"        : "<f10>",
        "F11"        : "<f11>",
        "F12"        : "<f12>",
        "F13"        : "<f13>",
        "F14"        : "<f14>",
        "F15"        : "<f15>",
    }
    
    def dfly2emacsKey(self, keystr):
        keys = keystr.split('-')
        keys = [k.strip() for k in keys]
        modifiers = [] 
        if len(keys) > 1:
            modifiers = [k.upper() for k in keys[0]]
            modifiers = ['M' if k == 'A' else k for k in modifiers]
            del keys[0]
        keys = [self.replacements[k] if k in self.replacements else k for k in keys]
        modifiers.extend(keys)
        return '-'.join(modifiers)
    
    def _lisp(self, extras={}):
        keys = []
        cmd = "(execute-kbd-macro (kbd \"%s\"))"
        keys = [self.dfly2emacsKey(k) for k in splitKeyString(self.data % extras)]
        cmd %= ' '.join(keys)
        return cmd

class EmacsText(Cmd):
    def _lisp(self, extras={}):
        words = (self.data % extras).lower().split(' ')
        if deepEmpty(words):
            return
        
        words = FormatState().format(words)
        cmd = "(insert \"%s\")" % (' '.join(words))
        return cmd
        
class AlignRegexp(Cmd):
    """Emacs inserts a special whitespace regex when called
    interactively that it doesn't if you call it manually.
    Also for some reason align-regexp blocks when called
    interactively."""
    def __init__(self, data):
        command = "(align-regexp (region-beginning) (region-end) \"%s\")"
        whitespace = "\\\(\\\s-*\\\)%s"
        command %= (whitespace % data)
        Cmd.__init__(self, command)
            
sexpRules = {}
for words, func in sexpFuncs.items():
    for pairWord, p in sexpPairs.items():
        sexpRules[words + ' ' + pairWord] = PairCmd(p, func)    

@registerRule
class EmacsRule(SeriesMappingRule):
    mapping  = {
        # general commands
        "cancel"                         : Key("c-g"),
        "eval"                           : Key("c-x,c-e"),
        "(start search | search)"        : Key('c-s'),
        "search [<text>]"                : Key('c-s') + Text("%(text)s"),
        "reverse search [<text>]"        : Key('c-r') + Text("%(text)s"),
        "start macro"                    : Key("F3"),
        "mack"                           : Key("F4"),
        "command"                        : Key("c-x,c-m"),
        "command <text>"                 : Key("c-x,c-m") + Text("%(text)s") + Key("enter"),
        "help function"                  : Key("c-h,f"),
        "help variable"                  : Key("c-h,v"),
        "help key"                       : Key("c-h,k"),

        
        # file commands
        "open file"                      : Key("c-x,c-f"),
        
        # buffer commands
        "switch (buff | buffer)"         : Key("c-x, b"),
        "list (buffs | buffers)"         : Key("c-x,c-b,c-x,o") + Cmd("(ace-jump-line-mode)"),
        "(kill | close) (buff | buffer)" : Key("c-x,k,enter"),
        "replace buff"                   : Key("c-x,c-v"),
        "replace buff <text>"            : Key("c-x,c-v") + Text("%(text)s") + Key("enter"),
        "folder"                         : Key("c-x,c-j"),
        
        # window commands
        "kill window"                    : Cmd("(delete-window)"),
        "other window"                   : Key("c-x, o"),
        "one window"                     : Key("c-x, 1"),
        "new frame"                      : Key("c-x, 5, 2"),
        
        # navigation commands
        "hedge"                          : Key("c-a"),
        "edge"                           : Key("c-e"),
        "top"                            : Key("a-langle"),
        "bottom"                         : Key("a-rangle"),
        "post [<n>]"                     : Key("a-f:%(n)d"),
        "pre [<n>]"                      : Key("a-b:%(n)d"),
        "go to line"                     : Key("a-g,a-g"),
        "go to line <line>"              : Key("a-g,a-g") + Text("%(line)d") + Key("enter"),
        "ace"                            : Key("c-c,space"),
        "ace care"                       : Key("c-u,c-c,space"),
        "ace line"                       : Key("c-u,c-u,c-c,space"),
        "pade"                           : Key("a-v"),
        "page"                           : Key("c-v"),
        "center"                         : Key("c-l"),
        "gruff [<n>]"                    : Key("c-up:%(n)d"),
        "graph [<n>]"                    : Key("c-down:%(n)d"),

        # text manip commands
        "mark"                           : Key("c-space"),
        "tark"                           : Cmd("(exchange-point-and-mark)"),
        "copy"                           : Key("a-w"),
        "copy line"                      : Cmd('(quick-copy-line)'),
        "copy word"                      : Cmd('(copy-word)'),

        "cut"                            : Key("c-x,c-k"),
        "kill [<n>]"                     : Key('c-k:%(n)d'),
        "(cut | kill) line"              : Cmd('(quick-cut-line)'),
        "snip [<n>]"                     : Key('a-d:%(n)d'),

        "yank"                           : Key("c-y"),
        "yank pop"                       : Key("a-y"),
        "term (yank | paste)"            : Key("s-insert"),
        
        "select all"                     : Key("c-a"),
        "comp"                           : Key("a-space"),
        "toke <text>"                    : Text("%(text)s") + Key("a-space"),
        "undo [that]"                    : Key("cs-underscore"),
        "redo [that]"                    : Key("as-underscore"),
        "pre slap"                       : Key("c-o"),

        "shift right"                    : Cmd("(call-interactively 'python-indent-shift-right)"),
        "shift left"                     : Cmd("(call-interactively 'python-indent-shift-left)"),
        "align regexp"                   : Cmd("(call-interactively 'align-regexp)"),

        "indent"                         : Cmd("(call-interactively 'indent-region)"),

        "comment"                        : Cmd("(call-interactively 'comment-or-uncomment-region)"),
        
        # replacing commands
        "replace"                        : Key('as-percent'),
        "replace <match> with <replace>" : Key('as-percent') + Text("%(match)s") + Key('enter') + Text("%(replace)s"),
        "yes"                            : Key('y'),
        "no"                             : Key('n'),
        
        # text commands
        "capitalize"                     : Key("a-c"),
        "upper case"                     : Key("a-u"),
        "lower case"                     : Key("a-l"),
        
        "parens"                         : Text("("),
        "braces"                         : Text("{"),
        "brackets"                       : Text("["),
        "single quotes"                  : Text("'"),
        "quotes [<text>]"                : Text("\""),
        "angles"                         : Text("<"),

        #"comma"
    }

    extras = [
        Integer("n", 1, 20),
        Integer("line", 0, 9999),
        Dictation("text"),
        Dictation("match"),
        Dictation("replace"),
        ]

    defaults = {
        "n"    : 1,
        "text" : "",
        }    

    @classmethod
    def activeForWindow(cls, window)     :
        return "emacs" in window.wmclass or "Emacs" in window.wmclass    

EmacsRule.mapping.update(sexpRules)    

@registerRule
class EmacsPython(SeriesMappingRule):
    mapping = {
        "align dic"                     : Cmd("(align-dict)"),
    }

    @classmethod
    def activeForWindow(cls, window):
        isemacs = EmacsRule.activeForWindow(window)
        if not isemacs:
            return False
        out = runEmacsCmd("major-mode", inFrame=window.iconName).strip()
        return out == "python-mode"    

