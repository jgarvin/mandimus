import mdlog
log = mdlog.getLogger(__name__)

from rules.Rule import registerRule
from rules.SeriesMappingRule import SeriesMappingRule
from rules.MappingRule import MappingRule
from Actions import (
    Key, Text, Camel, Underscore, Hyphen, Speak, Action, runCmd, SelectChoice, Mimic,
    splitKeyString, FormatState, ActionList)
from rules.Elements import Integer, Dictation, RuleRef
from Window import Window, getFocusedWindow
from EventLoop import getLoop
from wordUtils import extractWords, buildSelectMapping
from Events import GrammarEvent
from util import deepEmpty
from rules.emacs.Cmd import Cmd, runEmacsCmd, EmacsCommandWatcher, toggleCommandLogging
from rules.emacs.Key import Key as EmacsKey
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText
from rules.emacs.Base import EmacsBase
import rules.BaseRules as BaseRules
import string
import SelectOption
import EventList
import util

watchers = []
selectors = []

class EmacsOption(SelectOption.SelectOption):
    def _contextMatch(self, window):
        return window and Emacs.activeForWindow(window)
    
def currentBuffer():
    buf = runEmacsCmd("(buffer-name (current-buffer))")
    return buf.strip().strip('"')

class BufferWatcher(EmacsCommandWatcher):
    cmd = "(mapcar 'buffer-name (buffer-list))"
    inFrame = False
    eventType = EventList.BufferListEvent

watchers.append(BufferWatcher())

class SelectBufferBase(EmacsOption):
    eventType = EventList.BufferListEvent

    def _currentChoice(self):
        return currentBuffer()

    def _select(self, choice):
        runEmacsCmd("(switch-to-buffer \"%s\")" % choice)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)")

class SelectBuffer(SelectBufferBase):
    leadingTerm = "buff"
    
    def _filterChoices(self, choices):
        return [k for k in choices if (not k.startswith("#")
                                       and not k.startswith("*")
                                       and not k.startswith(" *"))]

class SelectChannel(SelectBufferBase):
    leadingTerm = "channel"

    def _filterChoices(self, choices):
        return [k for k in choices if k.startswith("#")]

class SelectSpecial(SelectBufferBase):
    leadingTerm = "special"

    def _filterChoices(self, choices):
        return [k for k in choices if k.startswith("*") or k.startswith(" *")]

class SelectTerminal(SelectBufferBase):
    leadingTerm = "term"

    def _filterChoices(self, choices):
        return [k for k in choices if k.startswith("*@")]

selectors.append(SelectBuffer())
selectors.append(SelectChannel())
selectors.append(SelectSpecial())
selectors.append(SelectTerminal())


# class SelectProject(SelectChoice):
#     @classmethod
#     def getChoices(cls):
#         buffs = runEmacsCmd("(projectile-relevant-known-projects)", inFrame=False)
#         return getStringList(buffs)
        
#     def _currentChoice(self):
#         return runEmacsCmd("(projectile-project-name)")

#     def _select(self, choice):
#         runEmacsCmd("(projectile-switch-project-by-name \"%s\")" % choice)

#     def _noChoice(self):
#         # TODO
#         pass
class ProjectFileWatcher(EmacsCommandWatcher):
    cmd = "(if (or (equal major-mode 'dired-mode) buffer-file-name) (projectile-current-project-files) nil)"
    allowError = True
    eventType = EventList.ProjectFileListEvent

watchers.append(ProjectFileWatcher())

openProjetileFileEl = """
(find-file-existing (concat (file-name-as-directory (projectile-project-root)) \"%s\"))
"""

class SelectProjectFile(EmacsOption):
    leadingTerm = "file"
    eventType = EventList.ProjectFileListEvent
    
    def _currentChoice(self):
        return currentBuffer()

    def _select(self, choice):
        runEmacsCmd(openProjetileFileEl % choice)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)")                

selectors.append(SelectProjectFile())
    
class WordWatcher(EmacsCommandWatcher):
    #cmd = "(md-get-buffer-words)"
    cmd = "(md-get-window-words)"
    eventType = EventList.EmacsWordEvent

    def _postProcess(self, output):
        lst = EmacsCommandWatcher._postProcess(self, output)
        # filter unicode
        lst = [''.join([c for c in n if c in string.printable]) for n in lst]
        lst = [x for x in lst if len(x) > 1 and not util.isNumber(x)]
        return lst

watchers.append(WordWatcher())

class SelectTypeClosest(EmacsOption):
    leadingTerm = "toke"
    eventType = EventList.EmacsWordEvent

    def _currentChoice(self):
        return None

    def _select(self, choice):
        EmacsText("%s" % choice, lower=False)()        

    def _noChoice(self):
        pass
    
    def _extractWords(self, choice):
        return extractWords(choice, translate={}, useDict=False)

selectors.append(SelectTypeClosest())

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
        "toggle debug"                   : Cmd("(toggle-debug-on-error)"),
        "exit debug"                     : Key("c-rbracket"),

        "ace line"                       : Key("c-u,c-u,c-c,space"),

        "go to line"                     : Key("a-g,a-g"),
        "go to line <line>"              : Key("a-g,a-g") + Text("%(line)d") + Key("enter"),
        
        # window commands
        "kill window"                    : Cmd("(delete-window)"),

        "new frame [<n>]"                : Cmd("(make-frame-command)"),
        "mini buffer"                    : Cmd("(md-select-minibuffer)"),        

        "search [<text>]"                : Key('c-s') + Text("%(text)s"),
        "lurch [<text>]"                 : Key('c-r') + Text("%(text)s"),
        "list buffs"                     : Key("c-x,c-b,c-x,o") + Cmd("(ace-jump-line-mode)"),
#"(projectile-project-root)"
        # projectile commands
        "project"                        : Key("c-c,p,p"),
        "open file"                      : Key("c-c,p,f"),
        "open folder"                    : Key("c-c,p,d"),
        "ack"                            : Key("c-c,p,s,a"),
        "open project"                   : Key("c-c,p,b"),
        "occur"                          : Key("c-c,p,o"),
        "project replace"                : Key("c-c,p,r"),
        "kill project"                   : Key("c-c,p,k"),
        "project root"                   : Key("c-c,p,D"),
        "compile"                        : Key("c-c,p,c"),
        "invalidate projectile cache"    : Key("c-c,p,i"),

        # compilation mode commands
        # don't think I can make these mode specific...
        "oops"                           : Key("a-g,n"),
        "spoo"                           : Key("a-g,p"),
        "file oops"                      : Key("a-rbrace"),
        "file spoo"                      : Key("a-lbrace"),
        "toggle oops trace"              : Key("c-c,c-f"),
                
        # file commands
        "plain open file"                : Key("c-x,c-f"),
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
        "stop irc"                       : Key("c-x,c-m") + Text("stop-irc") + Key("enter"),
        "toggle tail mode"               : Cmd("(auto-revert-tail-mode)"),
        "list packages"                  : Key("a-x") + Text("list-packages") + Key("enter"),
        "git status"                     : Key("a-x") + Text("magit-status") + Key("enter"),
        "submit"                         : Key("c-x,hash"),
        "open terminal"                  : Cmd("(etc-start-or-open-terminal)"),
        "show top"                       : Cmd("(etc-start-or-open-top)"),
        "open temp"                      : Cmd("(md-create-temp-file \"temp\")"),
        "toggle command logging"         : toggleCommandLogging,
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
class Emacs(EmacsBase):
    mapping  = {
        # general commands
        "axe [<n>]"                    : Key("cs-g:%(n)d"),
        "super [<n>] axe"              : Key("c-g:%(n)d"),
        "axe"                          : Key("c-g"),
        "eval"                         : Key("c-x,c-e"),
        "start macro"                  : Key("F3"),
        "mack"                         : Key("F4"),
        "other"                        : Key("c-x, o"),
        "collapse"                     : Key("c-x, 1"),
        "other collapse"               : Key("c-x, o") + Key("c-x, 1"),

        "help function"                : Key("c-h,f"),
        "help variable"                : Key("c-h,v"),
        "help key"                     : Key("c-h,k"),
        "help mode"                    : Key("c-h,m"),
        "help docks"                   : Key("c-h,d"),
        "help news"                    : Key("c-h,n"),
        
        # navigation commands
        "slof [<n>]"                   : Cmd("(md-next-whitespace-separated-thing)"),
        "slop [<n>]"                   : Cmd("(md-previous-whitespace-separated-thing)"),
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
        "paid [<n>]"                   : Key("a-v:%(n)d"),
        "page [<n>]"                   : Key("c-v:%(n)d"),
        "center"                       : Key("c-l"),
        "gruff [<n>]"                  : Key("c-up:%(n)d"),
        "graph [<n>]"                  : Key("c-down:%(n)d"),
        "left [<n>]"                   : Key("left:%(n)d"),
        "right [<n>]"                  : Key("right:%(n)d"),
        "up [<n>]"                     : Key("up:%(n)d"),
        "down [<n>]"                   : Key("down:%(n)d"),
        "pa [<n>]"                     : Key("space:%(n)d"),
        
        "slap [<n>]"                   : Key("enter:%(n)d"),
        "pals [<n>]"                   : Cmd("(md-new-line-anywhere)"),
        "open [<n>]"                   : Key("c-o:%(n)d"),
        "nepo [<n>]"                   : Cmd("(md-open-line-anywhere)"),

        # mark commands
        "mark [(line | word | graph)]" : Mark(),
        "tark"                         : Cmd("(exchange-point-and-mark)"),
        "select [<n>]"                 : Key("c-equal:%(n)d"),
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
        "yank pop [<n>]"               : Key("a-y:%(n)d"),
        "term (yank | paste)"          : Key("s-insert"),
        
        "select all"                   : Key("c-a"),
        "fish"                         : Key("a-space"),
        "undo [<n>]"                   : Key("cs-underscore:%(n)d"),
        "redo [<n>]"                   : Key("as-underscore:%(n)d"),

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
        "snap [<n>]"                   : Key("c-u,c-space:%(n)d"),
        "big snap [<n>]"               : Key("c-x,c-space:%(n)d"),

        "num <big>"                    : EmacsText("%(big)d"),
    }


