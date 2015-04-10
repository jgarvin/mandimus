import mdlog
log = mdlog.getLogger(__name__)

from Actions import (
    Key, Text, Action, runCmd, Repeat,
)

from protocol import RuleType
from rules.emacs.common import emacsExtras, emacsDefaults
from Window import Window, getFocusedWindow
from EventLoop import getLoop
from wordUtils import extractWords
from rules.emacs.Cmd import Cmd, runEmacsCmd, toggleCommandLogging, Minibuf, InsertString
from rules.emacs.Text import EmacsText
import rules.BaseRules as BaseRules
from rules.ContextualRule import makeContextualRule
import string
import EventList
from RefreshClient import toggleRefreshClientSources
from requirements.Emacs import IsEmacs

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
    "command"                     : Key("c-x,c-m"),
    "toggle debug"                : Cmd("(toggle-debug-on-error)"),
    "exit debug"                  : Key("c-rbracket"),
    "debug function"              : Key("c-x,c-m") + Text("debug-on-entry") + Key("enter"),
    "cancel debug function"       : Key("c-x,c-m") + Text("cancel-debug-on-entry") + Key("enter"),

    "ace line"                    : Key("c-u,c-u,c-c,space"),

    "go to line"                  : Key("a-g,a-g"),
    "go to line <line>"           : Key("a-g,a-g") + Text("%(line)d") + Key("enter"),

    # window commands
    "destroy emacs window"        : Cmd("(delete-window)"),

    "new frame [<i>]"             : Cmd("(make-frame-command)"),
    "mini buffer"                 : Cmd("(md-select-minibuffer)"),        

    "list buffs"                  : Key("c-x,c-b,c-x,o") + Cmd("(ace-jump-line-mode)"),

    # projectile commands
    "switch project"              : Key("c-c,p,p"),
    "root folder"                 : Key("c-c,p,d"),
    "ack"                         : Key("c-c,p,s,a"),
    "occur"                       : Key("c-c,p,o"),
    "project replace"             : Key("c-c,p,r"),
    "kill project"                : Key("c-c,p,k"),
    "project root"                : Key("c-c,p,D"),
    "build"                       : Key("c-c,p,c"),
    "invalidate projectile cache" : Key("c-c,p,i"),

    # compilation mode commands
    # don't think I can make these mode specific...
    "oops"                        : Key("a-g,n"),
    "spoo"                        : Key("a-g,p"),
    "file oops"                   : Key("a-rbrace"),
    "file spoo"                   : Key("a-lbrace"),
    "toggle oops trace"           : Key("c-c,c-f"),

    # file commands
    "plain open file"             : Key("c-x,c-f"),
    "alternate file"              : Key("c-x,c-v"),
    "recent files"                : Key("c-c,c-e"),
    "man page"                    : Key("a-x") + Text("man") + Key("enter"),
    "find file"                   : Minibuf("find-name-dired"), 

    # buffer commands
    "switch (buff | buffer)"      : Key("c-x, b"),
    "destroy buff"                : Key("c-x,k,enter"),

    "open client log"             : Cmd("(md-open-most-recent-file \"~/dragonshare/log\" \"client-[^.]*.log\")"),

    "open server log"             : Cmd("(md-open-most-recent-file \"/tmp\" \"server-[^.]*.log\")"),

    # misc
    "start irc"                   : Key("c-x,c-m") + Text("irc-maybe") + Key("enter"),
    "stop irc"                    : Key("c-x,c-m") + Text("stop-irc") + Key("enter"),
    "toggle tail mode"            : Cmd("(auto-revert-tail-mode)"),
    "list packages"               : Key("a-x") + Text("list-packages") + Key("enter"),
    "get status"                  : Key("a-x") + Text("magit-status") + Key("enter"),
    "submit"                      : Key("c-x,hash"),
    "open terminal"               : Cmd("(etc-start-or-open-terminal)"),
    #"create shell"                : Cmd("(etc-open-shell nil)"),
    "show top"                    : Cmd("(etc-start-or-open-top)"),
    "open temp"                   : Cmd("(md-create-temp-file \"temp\")"),
    "toggle command logging"      : toggleCommandLogging,
    "toggle refresh client"       : toggleRefreshClientSources,
    "magnify [<i>]"               : Key("c-x,c-plus:%(i)d"),
    "demagnify [<i>]"             : Key("c-x,c-minus:%(i)d"),
}


EmacsIsolatedRule = makeContextualRule("EmacsIsolated", _mapping, emacsExtras, emacsDefaults, ruleType=RuleType.INDEPENDENT)
EmacsIsolatedRule.context.addRequirement(IsEmacs)

_mapping = {
    "search [<text>]" : Key('c-s') + Text("%(text)s"),
    "lurch [<text>]"  : Key('c-r') + Text("%(text)s"),
    "toggle"          : Key('a-t'),
}

EmacsSearchRule = makeContextualRule("EmacsSearch", _mapping, emacsExtras, emacsDefaults, ruleType=RuleType.TERMINAL)
EmacsSearchRule.context.addRequirement(IsEmacs)

_mapping  = {
    # general commands
    "axe [<i>]"                     : Cmd("(setq unread-command-events (append unread-command-events (list ?\\C-g)))", queryOnly=True),
    "super axe [<i>]"               : Key("c-g:%(i)d"),
    "eval"                          : Key("c-x,c-e"),
    "start macro"                   : Key("F3"),
    "mack"                          : Key("F4"),
    "other [<i>]"                   : Key("c-x,o") * Repeat(extra="i"),
    "collapse"                      : Key("ca-rbracket"),
    "other [<i>] collapse"          : (Key("c-x, o") * Repeat(extra="i")) + Key("c-x, 1"),

    "help function"                 : Key("c-h,f"),
    "help function slap"            : Key("c-h,f,enter"),
    "help variable"                 : Key("c-h,v"),
    "help variable slap"            : Key("c-h,v,enter"),
    "help key"                      : Key("c-h,k"),
    "help mode"                     : Key("c-h,m"),
    "help docks"                    : Key("c-h,d"),
    "help news"                     : Key("c-h,n"),
    "help info"                     : Key("c-h,i"),
    "help syntax"                   : Key("c-h,s"),
    "help bindings"                 : Key("c-h,b"),

    # navigation commands
    "ace"                           : Key("c-c,space"),
    "ace care"                      : Key("c-u,c-c,space"),

    "replace"                       : Key('as-percent'),

    "home [<i>]"                    : Key("c-a:%(i)d"),
    "edge"                          : Cmd("(end-of-line)"),
    "cliff"                         : Cmd("(md-go-to-cliff)"),
    "top side"                      : Key("a-langle"),
    "bottom"                        : Key("a-rangle"),
    "window top side"               : Cmd("(goto-char (window-start))"),
    "window bottom"                 : Cmd("(goto-char (- (window-end) 1)) (previous-line) (beginning-of-line)"),
    "pro [<n>]"                     : Key("a-f:%(n)d"),
    "per [<n>]"                     : Key("a-b:%(n)d"),
    "over [<n>]"                    : Cmd("(forward-symbol 1)"),
    "under [<n>]"                   : Cmd("(forward-symbol -1)"),
    "leaf [<n>]"                    : Key("pgdown:%(n)d"),
    "feel [<n>]"                    : Key("pgup:%(n)d"),
    "center"                        : Key("c-l"),
    "gruff [<n>]"                   : Key("a-lbrace:%(n)d"),
    "graph [<n>]"                   : Key("a-rbrace:%(n)d"),
    "left [<n>]"                    : Key("left:%(n)d"),
    "right [<n>]"                   : Key("right:%(n)d"),
    "up [<n>]"                      : Key("up:%(n)d"),
    "down [<n>]"                    : Key("down:%(n)d"),

    "slap [<i>]"                    : Key("enter:%(i)d"),
    "pals [<i>]"                    : Cmd("(md-new-line-anywhere)"),
    "open [<i>]"                    : Key("c-o:%(i)d"),
    "nepo [<i>]"                    : Cmd("(md-open-line-anywhere)"),

    # mark commands
    "mark [<i>]"                    : Key("c-space:%(i)d"),
    "exchange"                      : Cmd("(exchange-point-and-mark)"),
    "select [<i>]"                  : Key("c-equal:%(i)d"),
    "contract"                      : Key("a-equal"),

    # text manip commands
    "copy [(line | word | graph)]"  : Copy(),
    "cut [(line | word | graph)]"   : Cut(),        

    "kill [<n>]"                    : Key('c-k:%(n)d'),
    "nip [<n>]"                     : Cmd('(md-backward-kill-word)'),
    "pin [<n>]"                     : Cmd('(md-forward-kill-word)'),        
    "pat [<n>]"                     : Key("delete:%(n)d"),
    "knock [<n>]"                   : Key("backspace:%(n)d"),
    "squeeze"                       : Cmd('(cycle-spacing)'),

    "yank"                          : Key("c-y"),
    "yank pop [<i>]"                : Key("a-y:%(i)d"),
    "term (yank | paste)"           : Key("s-insert"),

    "select all"                    : Key("c-home,c-space,c-end"),
    "fish"                          : Key("a-space"),
    "undo [<i>]"                    : Key("cs-underscore:%(i)d"),
    "redo [<i>]"                    : Key("as-underscore:%(i)d"),

    "shift right"                   : Cmd("(call-interactively 'python-indent-shift-right)"),
    "shift left"                    : Cmd("(call-interactively 'python-indent-shift-left)"),
    "align regexp"                  : Cmd("(call-interactively 'align-regexp)"),

    "indent"                        : Cmd("(call-interactively 'indent-region)"),

    "comment"                       : Key("c-slash"),

    # text commands
    "capital"                       : Key("a-c"),
    "upper"                         : Key("a-u"),
    "lower"                         : Key("a-l"),

    # save mark, almost never use, need to get used to
    #"push"                         : Key("c-space,c-space"),

    "snap [<i>]"                    : Key("c-u,c-space:%(i)d"),
    "big snap [<i>]"                : Key("c-x,c-space:%(i)d"),

    "num <big>"                     : EmacsText("%(big)d"),
    "inspect character"             : Key("c-u,c-x,equal"),
    "insert character"              : Key("c-x,8,enter"),
    "complete"                      : Minibuf("company-complete"),
    "open this"                     : Minibuf("find-file-at-point"),
    "shell command"                 : Minibuf("etc-shell-command"),
    "insert path"                   : InsertString("(buffer-file-name)"),
    "insert base name"              : InsertString("(file-name-base (buffer-file-name))"),
    "insert buffer name"            : InsertString("(buffer-name)"),
    "insert name without extension" : InsertString("(file-name-sans-extension (buffer-file-name))"),
    "insert directory"              : InsertString("(file-name-directory (buffer-file-name))"),
    "insert extension"              : InsertString("(file-name-extension (buffer-file-name))"),    
}

EmacsRule = makeContextualRule("Emacs", _mapping, emacsExtras, emacsDefaults)
EmacsRule.context.addRequirement(IsEmacs)
