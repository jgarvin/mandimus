from Rule import registerRule
from SeriesMappingRule import SeriesMappingRule
from MappingRule import MappingRule
from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, runCmd, SelectChoice
from Elements import Integer, Dictation
from Window import Window
from EventLoop import getLoop
from wordUtils import extractWords, buildSelectMapping
import re
import subprocess

# TODO: fix this apparent POS by getting rid of the
# shell. not sure why it randomly fails....
def runEmacsCmd(command, inFrame=True):
    # escape single quotes, we actually close the string
    # add the escape single quote, and reopen the string
    args = []
    # args += ['echo']
    args += ['emacsclient']
    args += ['-e']
    if inFrame:
        command = '(with-current-buffer "%s" %s)'
        command = cmd % (Window().iconName, command)
    args += [command]
    print 'Running: [' + ' '.join(args) + ']'
    # doesn't work:
    # -disabling shell
    # -passing args as list
    # -0/-1 bufsize
    # close fds
    s = subprocess.Popen(args, bufsize=-1, shell=False, stdout=subprocess.PIPE, stderr=subprocess.PIPE, close_fds=True)
    s.wait()
    print s.returncode
    (out, err) = s.communicate()
    print type(out), type(err)
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
    print 't', buffs
    # buffs = re.findall('"[^"]*"', buffs)
    # # print 'e', buffs
    # buffs = [x.strip('"') for x in buffs]
    # print 'x', buffs
    return buffs

def currentBuffer():
    buf = runEmacsCmd("(buffer-name (current-buffer))")
    return buf.strip('"')

def showBuffer(buf):
    runEmacsCmd("(switch-to-buffer \"%s\")" % buf)        

class SelectBuffer(SelectChoice):
    def _tieSorter(self):
        return lambda x: x[0]

    def _currentChoice(self):
        return currentBuffer()

    def _select(self, choice):
        showBuffer(choice)

def updateBufferGrammar():
    bufs = bufferList()
    # print 'bufs: ' + str(bufs)
    # spokenForms = {}
    # for b in bufs:
    #     spokenForms[b] = [set(extractWords(b))] 
    # omapping = buildSelectMapping('buff', spokenForms, SelectBuffer)
    # class BufferMapping(MappingRule):
    #     mapping = omapping
    #print omapping
    
getLoop().subscribeTimer(1, updateBufferGrammar)

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
        "exchange"                       : Cmd("(exchange-point-and-mark)"),
        
        # file commands
        "open file"                      : Key("c-x,c-f"),
        
        # buffer commands
        "switch (buff | buffer)"         : Key("c-x, b"),
        "buff"                    : Key("c-x, b") + Key("enter"),
        "buff <text>"                    : Key("c-x, b") + Text("%(text)s") + Key("enter"),
        "list (buffs | buffers)"         : Key("c-x,c-b"),
        "(kill | close) (buff | buffer)" : Key("c-x,k,enter"),
        "replace buff"                   : Key("c-x,c-v"),
        "replace buff <text>"            : Key("c-x,c-v") + Text("%(text)s") + Key("enter"),
        
        # window commands
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
        "enter"                          : Key("enter"),
        "open line"                      : Key("c-o"),
        "hit <text>"                     : Text("%(text)s") + Key("enter"),

        "shift right"                    : Cmd("(call-interactively 'python-indent-shift-right)"),
        "shift left"                     : Cmd("(call-interactively 'python-indent-shift-left)"),
        "align regexp"                   : Cmd("(call-interactively 'align-regexp)"),
        "indent"                         : Cmd("(call-interactively 'indent-region)"),
        
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
