from Rule import registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, runCmd, SelectChoice, Mimic
from Elements import Integer, Dictation
from Window import Window
from EventLoop import getLoop
from wordUtils import extractWords, buildSelectMapping
from Events import GrammarEvent
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
    args = []
    args += [EMACSCLIENT]
    args += ['-e']
    if inFrame:
        cmd = '(with-current-buffer "%s" %s)'
        command = cmd % (Window().name, command)
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
    def __call__(self, extras={}):
        fulldata = (self.data % extras)
        runEmacsCmd(fulldata)

class PairCmd(Cmd):
    def __init__(self, pair, cmd):
        x = "(single-pair-only-sexp \"%s\" '%s)" % (pair, cmd)
        Cmd.__init__(self, x)

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

sexpRules = {}
for words, func in sexpFuncs.items():
    for pairWord, p in sexpPairs.items():
        sexpRules[words + ' ' + pairWord] = PairCmd(p, func)

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

class AlignRegexp(Cmd):
    """Emacs inserts a special whitespace regex when called
    interactively that it doesn't if you call it manually.
    Also for some reason align-regexp blocks when called
    interactively."""
    def __init__(self, data):
        command = "(align-regexp (region-beginning) (region-end) \"%s\")"
        whitespace = "\\\(\\\s-*\\\)%s"
        command = command % (whitespace % data)
        Cmd.__init__(self, command)

@registerRule
class EmacsRule(SeriesMappingRule):
    mapping  = {
        # general commands
        "cancel"                         : Key("c-g"),
        "eval"                           : Key("c-x,c-e"),
        "(start search | search)"        : Key('c-s'),
        "search [<text>]"                : Key('c-s') + Text("%(text)s"),
        "reverse search"                 : Key('c-r'),
        "start macro"                    : Key("F3"),
        "mack"                           : Key("F4"),
        "command"                        : Key("c-x,c-m"),
        "command <text>"                 : Key("c-x,c-m") + Text("%(text)s") + Key("enter"),
        
        # file commands
        "open file"                      : Key("c-x,c-f"),
        
        # buffer commands
        "switch (buff | buffer)"         : Key("c-x, b"),
        "list (buffs | buffers)"         : Key("c-x,c-b,c-x,o") + Cmd("(ace-jump-line-mode)"),
        "(kill | close) (buff | buffer)" : Key("c-x,k,enter"),
        "replace buff"                   : Key("c-x,c-v"),
        "replace buff <text>"            : Key("c-x,c-v") + Text("%(text)s") + Key("enter"),
        
        # window commands
        "kill window"                    : Cmd("(delete-window)"),
        "other window"                   : Key("c-x, o"),
        "one window"                     : Key("c-x, 1"),
        "new frame"                      : Key("c-x, 5, 2"),
        
        # navigation commands
        "home"                           : Key("c-a"),
        "end"                            : Key("c-e"),
        "top"                            : Key("a-langle"),
        "bottom"                         : Key("a-rangle"),
        "next word [<n>]"                : Key("a-f:%(n)d"),
        "back word [<n>]"                : Key("a-b:%(n)d"),
        "go to line"                     : Key("a-g,a-g"),
        "go to line <line>"              : Key("a-g,a-g") + Text("%(line)d") + Key("enter"),
        "(page down | next page)"        : Key("c-v"),
        "(page up | back page)"          : Key("a-v"),
        "ace"                            : Key("c-c,space"),
        "ace care"                       : Key("c-u,c-c,space"),
        "ace line"                       : Key("c-u,c-u,c-c,space"),
        
        # text manip commands
        "mark"                           : Key("c-space"),
        "tark"                           : Cmd("(exchange-point-and-mark)"),
        "copy"                           : Key("a-w"),
        "copy line"                      : Cmd('(quick-copy-line)'),
        "copy word"                      : Cmd('(copy-word)'),

        "cut"                            : Key("c-x,c-k"),
        "kill"                           : Key('c-k'),
        "(cut | kill) line"              : Cmd('(quick-cut-line)'),
        "kill word"                      : Key('a-d'),

        "yank"                           : Key("c-y"),
        "yank pop"                       : Key("a-y"),
        "term (yank | paste)"            : Key("s-insert"),

        "select all"                     : Key("c-a"),
        "comp"                           : Key("a-space"),
        "toke <text>"                    : Text("%(text)s") + Key("a-space"),
        "undo [that]"                    : Key("cs-underscore"),
        "redo [that]"                    : Key("as-underscore"),
        "open line"                      : Key("c-o"),
        "hit <text>"                     : Text("%(text)s") + Key("enter"),

        "shift right"                    : Cmd("(call-interactively 'python-indent-shift-right)"),
        "shift left"                     : Cmd("(call-interactively 'python-indent-shift-left)"),
        "align regexp"                   : Cmd("(call-interactively 'align-regexp)"),

        # TODO python only
        "align (dict | set)"             : PairCmd("{", "sp-select-next-thing") + AlignRegexp(":"),
        
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
