import mdlog
log = mdlog.getLogger(__name__)

from Actions import (
    Key, Text, Action, runCmd, Repeat,
    # SelectChoice,
)

from protocol import RuleType
from rules.emacs.common import emacsExtras, emacsDefaults
from Window import Window, getFocusedWindow
from EventLoop import getLoop
from wordUtils import extractWords
from rules.emacs.Cmd import Cmd, runEmacsCmd, toggleCommandLogging, Minibuf
# from rules.emacs.CommandWatcher import EmacsCommandWatcher
from rules.emacs.Text import EmacsText
# from rules.emacs.Base import EmacsBase
import rules.BaseRules as BaseRules
from rules.ContextualRule import makeContextualRule
import string
# import SelectOption
import EventList
from RefreshClient import toggleRefreshClientSources
from requirements.Emacs import IsEmacs

# class BufferWatcher(EmacsCommandWatcher):
#     cmd = "(mapcar 'buffer-name (buffer-list))"
#     inFrame = False
#     eventType = EventList.BufferListEvent

# class ProjectWatcher(EmacsCommandWatcher):
#     cmd = "(projectile-relevant-known-projects)"
#     allowError = True
#     eventType = EventList.ProjectListEvent

# class ProjectFileWatcher(EmacsCommandWatcher):
#     #cmd = "(if (or (equal major-mode 'dired-mode) buffer-file-name) (projectile-current-project-files) nil)"
#     cmd = "md-projectile-files"
#     allowError = True
#     eventType = EventList.ProjectFileListEvent

# class WordWatcher(EmacsCommandWatcher):
#     cmd = "md-symbols-cache"
#     eventType = EventList.EmacsWordEvent

#     def filter(self, x):
#         x = ''.join(c for c in x if c not in string.punctuation + string.digits)
#         if len(x) <= 2:
#             return False
#         return True

#     def _postProcess(self, output):
#         lst = EmacsCommandWatcher._postProcess(self, output)
#         # filter unicode
#         lst = [''.join([c for c in n if c in string.printable]) for n in lst]
#         lst = [x for x in lst if self.filter(x)]
#         return lst

# watchers = []
# watchers.append(BufferWatcher())
# watchers.append(ProjectWatcher())
# watchers.append(ProjectFileWatcher())
# watchers.append(WordWatcher())

# selectors = []

# class EmacsOption(SelectOption.SelectOption):
#     def _contextMatch(self, window):
#         return window and Emacs.activeForWindow(window)
    
# def currentBuffer():
#     buf = runEmacsCmd("(buffer-name (current-buffer))")
#     return buf.strip().strip('"')


# class SelectBufferBase(EmacsOption):
#     eventType = EventList.BufferListEvent

#     def _currentChoice(self):
#         return currentBuffer()

#     def _select(self, choice):
#         runEmacsCmd("(switch-to-buffer \"%s\")" % choice)

#     def _noChoice(self):
#         runEmacsCmd("(switch-to-buffer nil)")

# class SelectBuffer(SelectBufferBase):
#     leadingTerm = "buff"
    
#     def _filterChoices(self, choices):
#         return [k for k in choices if (not k.startswith("#")
#                                        and not k.startswith("*")
#                                        and not k.startswith(" *"))]

# class SelectChannel(SelectBufferBase):
#     leadingTerm = "channel"

#     def _filterChoices(self, choices):
#         return [k for k in choices if k.startswith("#")]

# class SelectSpecial(SelectBufferBase):
#     leadingTerm = "special"

#     def _filterChoices(self, choices):
#         return [k for k in choices if k.startswith("*") or k.startswith(" *")]

# class SelectTerminal(SelectBufferBase):
#     leadingTerm = "term"

#     def _filterChoices(self, choices):
#         return [k for k in choices if k.startswith("*@")]

# selectors.append(SelectBuffer())
# selectors.append(SelectChannel())
# selectors.append(SelectSpecial())
# selectors.append(SelectTerminal())


# class SelectProject(EmacsOption):
#     leadingTerm = "project"
#     eventType = EventList.ProjectListEvent

#     def _currentChoice(self):
#         return runEmacsCmd("(projectile-project-name)").strip("\"")

#     def _select(self, choice):
#         runEmacsCmd("(projectile-switch-project-by-name \"%s\")" % choice)

#     def _noChoice(self):
#         Key("c-c,p,p,enter")()

# selectors.append(SelectProject())


# openProjetileFileEl = """
# (find-file-existing (concat (file-name-as-directory (projectile-project-root)) \"%s\"))
# """

# class SelectProjectFile(EmacsOption):
#     leadingTerm = "file"
#     eventType = EventList.ProjectFileListEvent
#     classLog = True
    
#     def _currentChoice(self):
#         return runEmacsCmd("(when (md-current-path) (file-relative-name (md-current-path) (projectile-project-root))))").strip("\"")

#     def _select(self, choice):
#         runEmacsCmd(openProjetileFileEl % choice)

#     def _noChoice(self):
#         runEmacsCmd("(switch-to-buffer nil)")                

# selectors.append(SelectProjectFile())
    

# class SelectTypeClosest(EmacsOption):
#     leadingTerm = "toke"
#     eventType = EventList.EmacsWordEvent

#     def _currentChoice(self):
#         return None

#     def _select(self, choice):
#         EmacsText("%s" % choice, lower=False)()        

#     def _noChoice(self):
#         pass
    
#     def _extractWords(self, choice):
#         return extractWords(choice, translate={}, useDict=False)

# selectors.append(SelectTypeClosest())

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

_mapping = {
    "command"                        : Key("c-x,c-m"),
    #"command <text>"                : Key("c-x,c-m") + Text("%(text)s") + Key("enter"),
    "toggle debug"                   : Cmd("(toggle-debug-on-error)"),
    "exit debug"                     : Key("c-rbracket"),
    "debug function"                 : Key("c-x,c-m") + Text("debug-on-entry") + Key("enter"),
    "cancel debug function"          : Key("c-x,c-m") + Text("cancel-debug-on-entry") + Key("enter"),

    "ace line"                       : Key("c-u,c-u,c-c,space"),

    "go to line"                     : Key("a-g,a-g"),
    "go to line <line>"              : Key("a-g,a-g") + Text("%(line)d") + Key("enter"),

    # window commands
    "destroy emacs window"           : Cmd("(delete-window)"),

    "new frame [<n>]"                : Cmd("(make-frame-command)"),
    "mini buffer"                    : Cmd("(md-select-minibuffer)"),        

    "search [<text>]"                : Key('c-s') + Text("%(text)s"),
    "lurch [<text>]"                 : Key('c-r') + Text("%(text)s"),
    "list buffs"                     : Key("c-x,c-b,c-x,o") + Cmd("(ace-jump-line-mode)"),

    # projectile commands
    "switch project"                 : Key("c-c,p,p"),
    "open file"                      : Key("c-c,p,f"),
    "open folder"                    : Key("c-c,p,d"),
    "ack"                            : Key("c-c,p,s,a"),
    "open project"                   : Key("c-c,p,b"),
    "occur"                          : Key("c-c,p,o"),
    "project replace"                : Key("c-c,p,r"),
    "kill project"                   : Key("c-c,p,k"),
    "project root"                   : Key("c-c,p,D"),
    "build"                          : Key("c-c,p,c"),
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
    "find file"                      : Minibuf("find-name-dired"), 

    # buffer commands
    "switch (buff | buffer)"         : Key("c-x, b"),
    "destroy buff"                   : Key("c-x,k,enter"),
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
    "get status"                     : Key("a-x") + Text("magit-status") + Key("enter"),
    "submit"                         : Key("c-x,hash"),
    "open terminal"                  : Cmd("(etc-start-or-open-terminal)"),
    "open shell"                     : Minibuf("shell"),
    "show top"                       : Cmd("(etc-start-or-open-top)"),
    "open temp"                      : Cmd("(md-create-temp-file \"temp\")"),
    "toggle command logging"         : toggleCommandLogging,
    "toggle refresh client"          : toggleRefreshClientSources,
}


EmacsIsolatedRule = makeContextualRule("EmacsIsolated", _mapping, emacsExtras, emacsDefaults, ruleType=RuleType.INDEPENDENT)
EmacsIsolatedRule.context.addRequirement(IsEmacs)

_mapping  = {
    # general commands
    "axe [<n>]"                    : Cmd("(setq unread-command-events (append unread-command-events (list ?\\C-g)))"),
    "super axe [<n>]"              : Key("c-g:%(n)d"),
    "eval"                         : Key("c-x,c-e"),
    "start macro"                  : Key("F3"),
    "mack"                         : Key("F4"),
    "other [<n>]"                  : Key("c-x,o") * Repeat(extra="n"),
    "collapse"                     : Key("c-x, 1"),
    "other [<n>] collapse"         : (Key("c-x, o") * Repeat(extra="n")) + Key("c-x, 1"),

    "help function"                : Key("c-h,f"),
    "help variable"                : Key("c-h,v"),
    "help key"                     : Key("c-h,k"),
    "help mode"                    : Key("c-h,m"),
    "help docks"                   : Key("c-h,d"),
    "help news"                    : Key("c-h,n"),
    "help info"                    : Key("c-h,i"),
    "help syntax"                  : Key("c-h,s"),

    # navigation commands
    "ace"                          : Key("c-c,space"),
    "ace care"                     : Key("c-u,c-c,space"),

    "home"                         : Key("c-a"),
    "edge"                         : Cmd("(end-of-line)"),
    "cliff"                        : Cmd("(md-go-to-cliff)"),
    "top side"                     : Key("a-langle"),
    "bottom"                       : Key("a-rangle"),
    "window top side"              : Cmd("(goto-char (window-start))"),
    "window bottom"                : Cmd("(goto-char (- (window-end) 1)) (previous-line) (beginning-of-line)"),
    "pro [<n>]"                    : Key("a-f:%(n)d"),
    "per [<n>]"                    : Key("a-b:%(n)d"),
    "over [<n>]"                   : Cmd("(forward-symbol 1)"),
    "under [<n>]"                  : Cmd("(forward-symbol -1)"),
    "leaf [<n>]"                   : Key("c-v:%(n)d"),
    "feel [<n>]"                   : Key("a-v:%(n)d"),
    "center"                       : Key("c-l"),
    "gruff [<n>]"                  : Key("c-up:%(n)d"),
    "graph [<n>]"                  : Key("c-down:%(n)d"),
    "left [<n>]"                   : Key("left:%(n)d"),
    "right [<n>]"                  : Key("right:%(n)d"),
    "up [<n>]"                     : Key("up:%(n)d"),
    "down [<n>]"                   : Key("down:%(n)d"),

    "slap [<n>]"                   : Key("enter:%(n)d"),
    "pals [<n>]"                   : Cmd("(md-new-line-anywhere)"),
    "open [<n>]"                   : Key("c-o:%(n)d"),
    "nepo [<n>]"                   : Cmd("(md-open-line-anywhere)"),

    # mark commands
    "mark [<n>]"                   : Key("c-space:%(n)d"),
    "exchange"                     : Cmd("(exchange-point-and-mark)"),
    "select [<n>]"                 : Key("c-equal:%(n)d"),
    "contract"                     : Key("a-equal"),

    # text manip commands
    "copy [(line | word | graph)]" : Copy(),
    "cut [(line | word | graph)]"  : Cut(),        

    "kill [<n>]"                   : Key('c-k:%(n)d'),
    "nip [<n>]"                    : Cmd('(md-backward-kill-word)'),
    "pin [<n>]"                    : Cmd('(md-forward-kill-word)'),        
    "pat [<n>]"                    : Key("delete:%(n)d"),
    "knock [<n>]"                  : Key("backspace:%(n)d"),
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

    # text commands
    "capital"                      : Key("a-c"),
    "upper"                        : Key("a-u"),
    "lower"                        : Key("a-l"),

    # save mark, almost never use, need to get used to
    #"push"                        : Key("c-space,c-space"),

    "snap [<n>]"                   : Key("c-u,c-space:%(n)d"),
    "big snap [<n>]"               : Key("c-x,c-space:%(n)d"),

    "num <big>"                    : EmacsText("%(big)d"),
    "inspect character"            : Key("c-u,c-x,equal"),
    "insert character"             : Key("c-x,8,enter"),
    "complete"                     : Minibuf("company-complete"),
    "open this"                    : Minibuf("find-file-at-point"),
    "shell command"                : Minibuf("etc-shell-command"),
}

EmacsRule = makeContextualRule("Emacs", _mapping, emacsExtras, emacsDefaults)
EmacsRule.context.addRequirement(IsEmacs)
