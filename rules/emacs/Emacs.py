from rules.Rule import registerRule
from rules.SeriesMappingRule import SeriesMappingRule
from rules.MappingRule import MappingRule
from Actions import (
    Key, Text, Camel, Underscore, Hyphen, Speak, Action, runCmd, SelectChoice, Mimic,
    splitKeyString, FormatState, ActionList)
from rules.Elements import Integer, Dictation
from Window import Window, getFocusedWindow
from EventLoop import getLoop
from wordUtils import extractWords, buildSelectMapping
from Events import GrammarEvent
from util import deepEmpty
from rules.emacs.Cmd import Cmd, runEmacsCmd
from rules.emacs.Key import Key as EmacsKey

import re

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
    "quote"        : "\\\"",
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
    b = bufferList()
    updateListGrammar(b, 'buff', {},
                      SelectBuffer, "EmacsBufferMapping")    

getLoop().subscribeTimer(1, updateBufferGrammar)
getLoop().subscribeTimer(10, updateCommandGrammar)
updateCommandGrammar()

class PairCmd(Cmd):
    def __init__(self, pair, cmd):
        x = "(single-pair-only-sexp \"%s\" '%s)" % (pair, cmd)
        Cmd.__init__(self, x)        

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
            
class Copy(Cmd):
    def _lisp(self, extras={}):
        words = extras['words']
        if "line" in words:
            return "(md-copy-line)"
        elif "word" in words:
            return "(md-copy-word)"
        elif "graph" in words:
            return "(md-copy-paragraph)"
        else:
            return "(kill-ring-save (region-beginning) (region-end) t)"

class Cut(Cmd):
    def _lisp(self, extras={}):
        words = extras['words']
        if "line" in words:
            return "(md-cut-line)"
        elif "word" in words:
            return "(md-forward-kill-word)"
        elif "graph" in words:
            return "(md-cut-paragraph)"
        else:
            return "(kill-region (region-beginning) (region-end) t)"        

class Mark(Cmd):
    def _lisp(self, extras={}):
        words = extras['words']
        if "line" in words:
            return "(md-mark-line)"
        elif "word" in words:
            return "(md-mark-word)"
        elif "graph" in words:
            return "(md-mark-paragraph)"
        else:
            return "(set-mark-command)"                

sexpRules = {}
for words, func in sexpFuncs.items():
    for pairWord, p in sexpPairs.items():
        sexpRules[words + ' ' + pairWord] = PairCmd(p, func)    

@registerRule
class Emacs(SeriesMappingRule):
    mapping  = {
        # general commands
        "cancel"                         : Key("c-g"),
        "eval"                           : Key("c-x,c-e"),
        "search [<text>]"                : Key('c-s') + Text("%(text)s"),
        "lurch [<text>]"                 : Key('c-r') + Text("%(text)s"),
        "start macro"                    : Key("F3"),
        "mack"                           : Key("F4"),
        "command"                        : Key("c-x,c-m"),
        "command <text>"                 : Key("c-x,c-m") + Text("%(text)s") + Key("enter"),
        "help function"                  : Key("c-h,f"),
        "help variable"                  : Key("c-h,v"),
        "help key"                       : Key("c-h,k"),

        
        # file commands
        "open file"                      : Key("c-x,c-f"),
        "alternate file"                 : Key("c-x,c-v"),
        
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
        "mini buffer"                    : Cmd("(md-select-minibuffer)"),        
        
        # navigation commands
        "home"                           : Key("c-a"),
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
        "mark [(line | word | graph)]"   : Mark(),
        "tark"                           : Cmd("(exchange-point-and-mark)"),
        "copy [(line | word | graph)]"   : Copy(),
        "cut [(line | word | graph)]"    : Cut(),        

        "kill [<n>]"                     : Key('c-k:%(n)d'),
        "snip [<n>]"                     : Cmd('(md-backward-kill-word)'),
        "snap [<n>]"                     : Cmd('(md-forward-kill-word)'),        

        "yank"                           : Key("c-y"),
        "yank pop"                       : Key("a-y"),
        "term (yank | paste)"            : Key("s-insert"),
        
        "select all"                     : Key("c-a"),
        "comp"                           : Key("a-space"),
        "toke <text>"                    : Text("%(text)s") + Key("a-space"),
        "undo"                           : Key("cs-underscore"),
        "redo"                           : Key("as-underscore"),
        "pre slap"                       : Key("c-o"),
        "turf"                           : Key("enter,c-o"),

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
        
        "lane"                           : Text("("),
        "rain"                           : Text(")"),
        "lace"                           : Text("{"),
        "race"                           : Text("}"),
        "lack"                           : Text("["),
        "rack"                           : Text("]"),
        "soak"                           : Text("'"),
        "quote [<text>]"                 : Text("\""),
        "angle"                          : Text("<"),
        "arg"                            : Text(","),
        "dash"                           : Text("-"),
        "cool"                           : Text(":"),
        
        # misc
        "start irc"                      : Key("c-x,c-m") + Text("irc-maybe") + Key("enter"),
    }

    extras = [
        Integer("n", 2, 20),
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
Emacs.mapping.update(sexpRules)    

