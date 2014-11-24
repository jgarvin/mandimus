import mdlog
log = mdlog.getLogger(__name__)

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
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText
import string

def bufferList():
    buffs = runEmacsCmd("(mapcar 'buffer-name (buffer-list))", inFrame=False)
    return getStringList(buffs)

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
        
def updateBufferGrammar():
    pass
    b = set(bufferList())
    channels = {k for k in b if k.startswith('#')}
    channels |= {k for k in b if k.startswith('irc')}
    special = {k for k in b if k.startswith(' *')}
    special |= {k for k in b if k.startswith('*')}
    b -= channels
    b -= special
    updateListGrammar(b, 'buff',
                      SelectBuffer, "EmacsBufferMapping",
                      Emacs.activeForWindow)
    updateListGrammar(channels, 'channel',
                      SelectBuffer, "EmacsChannelMapping",
                      Emacs.activeForWindow)
    updateListGrammar(special, 'special',
                      SelectBuffer, "EmacsSpecialMapping",
                      Emacs.activeForWindow)

class SelectTypeClosest(SelectChoice):
    def _currentChoice(self):
        return None

    def _select(self, choice):
        log.info(type(choice))
        EmacsText("%s" % choice, lower=False)()        

    def _noChoice(self):
        pass

def tokenList():
#    tokens = runEmacsCmd("(md-get-buffer-words)")
    tokens = runEmacsCmd("(md-get-window-words)")
    tokens = getStringList(tokens)
    # filter unicode crap
    tokens = [''.join([c for c in n if c in string.printable]) for n in tokens]
    return tokens

def extractTokeFunction(w):
    return extractWords(w, translate={}, useDict=False)

def updateTokenGrammar():
    t = set(tokenList())
    updateListGrammar(t, 'toke',
                      SelectTypeClosest, "EmacsTokenMapping",
                      Emacs.activeForWindow,
                      extractTokeFunction)
    
getLoop().subscribeTimer(1, updateBufferGrammar)
#getLoop().subscribeTimer(1, updateTokenGrammar)
#getLoop().subscribeTimer(10, updateCommandGrammar)
#updateCommandGrammar()

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
            Key("a-w")()
            return 

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
            Key("c-x,c-k")()
            return 

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
            Key("c-space")()
            return ""

@registerRule
class EmacsMapping(MappingRule):
    "this is for commands that are unlikely to be said in series"
    mapping = {
        "command"                        : Key("c-x,c-m"),
        "command <text>"                 : Key("c-x,c-m") + Text("%(text)s") + Key("enter"),
        "help function"                  : Key("c-h,f"),
        "help variable"                  : Key("c-h,v"),
        "help key"                       : Key("c-h,k"),
        "help docks"                     : Key("c-h,d"),
        "help news"                      : Key("c-h,n"),
        "toggle debug"                   : Cmd("(toggle-debug-on-error)"),
        "exit debug"                     : Key("c-rbracket"),

        "ace line"                       : Key("c-u,c-u,c-c,space"),

        "go to line"                     : Key("a-g,a-g"),
        "go to line <line>"              : Key("a-g,a-g") + Text("%(line)d") + Key("enter"),
        
        # window commands
        "kill window"                    : Cmd("(delete-window)"),
        "other"                          : Key("c-x, o"),
        "collapse"                       : Key("c-x, 1"),
        "other collapse"                 : Key("c-x, o") + Key("c-x, 1"),
        "new frame"                      : Key("c-x, 5, 2"),
        "mini buffer"                    : Cmd("(md-select-minibuffer)"),        

        "search [<text>]"                : Key('c-s') + Text("%(text)s"),
        "lurch [<text>]"                 : Key('c-r') + Text("%(text)s"),
        "list buffs"                     : Key("c-x,c-b,c-x,o") + Cmd("(ace-jump-line-mode)"),
                
        # file commands
        "open file"                      : Key("c-x,c-f"),
        "alternate file"                 : Key("c-x,c-v"),
        "recent files"                   : Key("c-c,c-e"),
        "man page"                       : Key("a-x") + Text("man") + Key("enter"),
        
        # buffer commands
        "switch (buff | buffer)"         : Key("c-x, b"),
        "kill buff"                      : Key("c-x,k,enter"),
        "folder"                         : Key("c-x,c-j"),

        "replace"                        : Key('as-percent'),
        "replace <match> with <replace>" : Key('as-percent') + Text("%(match)s") + Key('enter') + Text("%(replace)s"),

        "open client log"                : Cmd("(md-open-most-recent-file \"~/dragonshare/log\" \"client-[^.]*.log\")"),
        "open server log"                : Cmd("(md-open-most-recent-file \"/tmp\" \"server-[^.]*.log\")"),

        # misc
        "start irc"                      : Key("c-x,c-m") + Text("irc-maybe") + Key("enter"),
        "toggle tail mode"               : Cmd("(auto-revert-tail-mode)"),
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
    def activeForWindow(cls, window):
        return Emacs.activeForWindow(window)

@registerRule
class Emacs(SeriesMappingRule):
    mapping  = {
        # general commands
        "axe"                          : Key("c-g"),
        "eval"                         : Key("c-x,c-e"),
        "start macro"                  : Key("F3"),
        "mack"                         : Key("F4"),
        
        # navigation commands
        "fluff [<n>]"                  : Cmd("(md-next-whitespace-separated-thing)"),
        "fleece [<n>]"                 : Cmd("(md-previous-whitespace-separated-thing)"),
        "ace"                          : Key("c-c,space"),
        "ace care"                     : Key("c-u,c-c,space"),

        "home"                         : Key("c-a"),
        "edge"                         : Key("c-e"),
        "cliff"                        : Cmd("(md-go-to-cliff)"),
        "top"                          : Key("a-langle"),
        "bottom"                       : Key("a-rangle"),
        "window top"                   : Cmd("(goto-char (window-start))"),
        "window bottom"                : Cmd("(goto-char (- (window-end) 1)) (previous-line) (beginning-of-line)"),
        "post [<n>]"                   : Key("a-f:%(n)d"),
        "pre [<n>]"                    : Key("a-b:%(n)d"),
        "pade"                         : Key("a-v"),
        "page"                         : Key("c-v"),
        "center"                       : Key("c-l"),
        "gruff [<n>]"                  : Key("c-up:%(n)d"),
        "graph [<n>]"                  : Key("c-down:%(n)d"),
        "left [<n>]"                   : Key("left:%(n)d"),
        "right [<n>]"                  : Key("right:%(n)d"),
        "up [<n>]"                     : Key("up:%(n)d"),
        "down [<n>]"                   : Key("down:%(n)d"),
        "pa"                           : Key("space"),
        
        "slap [<n>]"                   : Key("enter:%(n)d"),
        "pals [<n>]"                   : Cmd("(md-new-line-anywhere)"),
        "open [<n>]"                   : Key("c-o:%(n)d"),
        "nepo [<n>]"                   : Cmd("(md-open-line-anywhere)"),

        # mark commands
        "mark [(line | word | graph)]" : Mark(),
        "tark"                         : Cmd("(exchange-point-and-mark)"),
        "select"                       : Key("c-equal"),
        "contract"                     : Key("a-equal"),
        
        # text manip commands
        "copy [(line | word | graph)]" : Copy(),
        "cut [(line | word | graph)]"  : Cut(),        

        "kill [<n>]"                   : Key('c-k:%(n)d'),
        "nip [<n>]"                    : Cmd('(md-backward-kill-word)'),
        "pin [<n>]"                    : Cmd('(md-forward-kill-word)'),        
        "pat [<n>]"                    : Key("delete:%(n)d"),
        "tap [<n>]"                    : Key("backspace:%(n)d"),
        "squeeze"                      : Cmd('(cycle-spacing)'),

        "yank"                         : Key("c-y"),
        "yank pop"                     : Key("a-y"),
        "term (yank | paste)"          : Key("s-insert"),
        
        "select all"                   : Key("c-a"),
        "fish"                         : Key("a-space"),
        #"toke <text>"                 : Text("%(text)s") + Key("a-space"),
        "undo"                         : Key("cs-underscore"),
        "redo"                         : Key("as-underscore"),
        "turf"                         : Key("enter,c-o"),

        "shift right"                  : Cmd("(call-interactively 'python-indent-shift-right)"),
        "shift left"                   : Cmd("(call-interactively 'python-indent-shift-left)"),
        "align regexp"                 : Cmd("(call-interactively 'align-regexp)"),

        "indent"                       : Cmd("(call-interactively 'indent-region)"),

        "comment"                      : Key("c-slash"),
        
        # replacing commands
        "yes"                          : Key('y'),
        "no"                           : Key('n'),
        
        # text commands
        "capital"                      : Key("a-c"),
        "upper"                        : Key("a-u"),
        "lower"                        : Key("a-l"),

        "push"                         : Key("c-space,c-space"),
        "snap"                         : Key("c-u,c-space"),
        "big snap"                     : Key("c-x,c-space"),

        "num <big>"                    : EmacsText("%(big)d"),
    }

    extras = [
        Integer("n", 2, 20),
        Dictation("text"),
        Dictation("match"),
        Dictation("replace"),
        Integer("big", 0, 2**14),
    ]

    defaults = {
        "n"    : 1,
        "text" : "",
        }    

    @classmethod
    def activeForWindow(cls, window)     :
        return "emacs" in window.wmclass or "Emacs" in window.wmclass    

