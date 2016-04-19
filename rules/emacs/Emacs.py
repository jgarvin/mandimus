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
from rules.BaseRules import PressKey
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
        for w in words:
            if w in UnitList:
                Mark()(extras)
                Key("a-w")()
                return
        else:
            Key("a-w")()
            return

class Cut(Cmd):
    def _lisp(self, extras={}):
        words = extras['words']
        for w in words:
            if w in UnitList:
                Mark()(extras)
                Key("c-w")()
                return
        else:
            Key("c-w")()
            return

#UnitList = ["line", "word", "symbol", "paragraph", "buffer", "filename", "sentence", "string", "parens", "brackets", "braces"]
UnitList = ["line", "word", "symbol", "paragraph", "buffer", "filename", "sentence", "string", "pair"]
UnitsOpt = "(" + " | ".join(UnitList) + ")"

class Mark(Cmd):
    def _lisp(self, extras={}):
        words = extras['words']
        for w in words:
            if w in UnitList:
                return "(md-mark-thing '%s)" % w
        Key("c-space")()
        return ""

class Comment(Cmd):
    def _lisp(self, extras={}):
        words = extras['words']
        for w in words:
            if w in UnitList:
                Mark()(extras)
        Key("a-semicolon")()
        return ""


_mapping = {
    "command"                     : Key("a-x"),
    "toggle emacs debug"          : Cmd("(toggle-debug-on-error)"),
    "exit debug"                  : Key("c-t,X,a"),
    "debug function"              : Key("c-t,c-m") + Text("debug-on-entry") + Key("enter"),
    "cancel debug function"       : Key("c-t,c-m") + Text("cancel-debug-on-entry") + Key("enter"),

    "ace line"                    : Key("c-u,c-u,c-c,space"),

    "go to line"                  : Key("a-g,a-g"),
    "go to line <big>"            : Key("a-g,a-g") + Text("%(big)d") + Key("enter"),

    "new frame [<i>]"             : Cmd("(make-frame-command)"),
    "mini buffer"                 : Cmd("(md-select-minibuffer)"),

    "list buffs"                  : Key("c-t,c-b,c-t,o") + Cmd("(ace-jump-line-mode)"),
    "list project files"          : Key("c-c,p,f"),

    # projectile commands
    "switch project"              : Key("c-c,p,p"),
    "root folder"                 : Key("c-c,p,d"),
    "ack"                         : Key("c-c,p,s,g"),
    "occur"                       : Key("c-c,p,o"),
    "project replace"             : Key("c-c,p,r"),
    "project file"                : Key("c-c,p,f"),
    "project kill"                : Key("c-c,p,k"),
    "project root"                : Key("c-c,p,D"),
    "build"                       : Key("c-c,p,c"),
    "invalidate projectile cache" : Key("c-c,p,i"),

    # compilation mode commands
    # don't think I can make these mode specific...
    "oops"                        : Key("a-g,n"),
    "spoo"                        : Key("a-g,p"),
    "file oops"                   : Key("a-rbrace"),
    "file spoo"                   : Key("a-lbrace"),
    "toggle trace"                : Key("c-c,c-f"),

    # file commands
    "plain open file"             : Key("c-t,c-f"),
    "alternate file"              : Key("c-t,c-v"),
    "recent files"                : Key("c-c,c-e"),
    "sudo open file"              : Key("c-c,o,s"),
    "man page"                    : Minibuf("man"),
    "find file"                   : Minibuf("find-name-dired"),

    # buffer commands
    "switch (buff | buffer)"      : Key("c-t, b"),
    "destroy buff"                : Key("c-t,k,enter", 100),

    "open client log"             : Cmd("(md-open-most-recent-file \"~/dragonshare/log\" \"client-[^.]*.log\")"),

    "open server log"             : Cmd("(md-open-most-recent-file \"/tmp\" \"server-[^.]*.log\")"),
    "show home folder"            : Cmd("(find-file \"~\")"),
    "show temp folder"            : Cmd("(find-file \"/tmp\")"),

    # misc
    "start irc"                   : Minibuf("irc-maybe"),
    "stop irc"                    : Minibuf("stop-irc"),
    "toggle tail mode"            : Cmd("(auto-revert-tail-mode)"),
    "list packages"               : Minibuf("list-packages"),
    "get status"                  : Minibuf("magit-status"),
    "submit"                      : Key("c-t,hash"),
    "open terminal"               : Cmd("(etc-start-or-open-terminal)"),
    "show top"                    : Cmd("(etc-start-or-open-top)"),
    "open temp"                   : Cmd("(md-create-temp-file \"temp\")"),
    "toggle command logging"      : toggleCommandLogging,
    "toggle refresh client"       : toggleRefreshClientSources,
    "magnify [<i>]"               : Key("c-t,c-plus:%(i)d"),
    "demagnify [<i>]"             : Key("c-t,c-minus:%(i)d"),
    # "compile"                     : Minibuf("compile"),
    "visual line mode"            : Minibuf("visual-line-mode"),
    "set indent <j>"              : Cmd("(etc-set-indent-preference %(j)d)"),
    "toggle namespace indent"     : Cmd("(etc-toggle-namespace-indent)"),
    "toggle read only"            : Key("c-t,c-q"),
}


EmacsIsolatedRule = makeContextualRule("EmacsIsolated", _mapping, emacsExtras, emacsDefaults, ruleType=RuleType.INDEPENDENT)
EmacsIsolatedRule.context.addRequirement(IsEmacs)

_mapping = {
    "toggle"             : Key('a-t'),
    "help function"      : Key("c-h,f"),
    "help function slap" : Key("c-h,f,enter"),
    "help variable"      : Key("c-h,v"),
    "help variable slap" : Key("c-h,v,enter"),
    "help key"           : Key("c-h,k"),
    "help mode"          : Key("c-h,m"),
    "help docks"         : Key("c-h,d"),
    "help news"          : Key("c-h,n"),
    "help info"          : Key("c-h,i"),
    "help syntax"        : Key("c-h,s"),
    "help bindings"      : Key("c-h,b"),

    "inspect character"  : Key("c-u,c-t,equal"),

    "last change"        : Key("c-period"),
    "next change"        : Key("c-comma"),
}

EmacsSearchRule = makeContextualRule("EmacsSearch", _mapping, emacsExtras, emacsDefaults, ruleType=RuleType.TERMINAL)
EmacsSearchRule.context.addRequirement(IsEmacs)

_mapping  = {
    # general commands
    "axe [<i>]"                     : Cmd("(setq unread-command-events (append unread-command-events (list ?\\C-g)))", queryOnly=True),
    "super axe [<i>]"               : Key("c-g:%(i)d"),
    "eval"                          : Key("c-t,c-e"),
    "start macro"                   : Key("F3"),
    "mack"                          : Key("F4"),
    "other [<i>]"                   : Key("c-t,o") * Repeat(extra="i"),
    "collapse"                      : Key("c-rbracket"),
    #"other [<i>] collapse"         : (Key("c-t, o") * Repeat(extra="i")) + Key("c-t, 1"),

    "replace"                       : Key('as-percent'),
    "center"                        : Key("c-l"),

    "slap [<i>]"                    : Key("enter:%(i)d"),
    "pals [<i>]"                    : Cmd("(md-new-line-anywhere)"),
    "open [<i>]"                    : Key("c-o:%(i)d"),
    "nepo [<i>]"                    : Cmd("(md-open-line-anywhere)"),

    # mark commands
    "exchange"                      : Cmd("(exchange-point-and-mark)"),
    "select [<i>]"                  : Key("c-equal:%(i)d"),
    "contract"                      : Key("a-equal"),

    # text manip commands
    "copy [%s]" % UnitsOpt          : Copy(),
    "cut [%s]" % UnitsOpt           : Cut(),
    "mark [%s]" % UnitsOpt          : Mark(),
    "comment [%s]" % UnitsOpt       : Comment(),
    "rectangle"                     : Key("c-t,space"),

    "kill [<n>]"                    : Key('c-k:%(n)d'),
    "chip [<n>]"                    : Cmd('(md-backward-kill-word)'),
    "pitch [<n>]"                   : Cmd('(md-forward-kill-word)'),
    "thump [<n>]"                   : Key("delete:%(n)d"),
    "knock [<n>]"                   : Key("backspace:%(n)d"),
    "squeeze"                       : Cmd('(cycle-spacing)'),

    "paste [<i>]"                   : Key("c-y:%(i)d"),
    "rotate [<i>]"                  : Key("a-y:%(i)d"),
    #"term (yank | paste)"          : Key("s-insert"),

    "select all"                    : Key("c-home,c-space,c-end"),
    "fish"                          : Key("a-space"),
    "undo [<i>]"                    : Key("c-slash:%(i)d"),
    "redo [<i>]"                    : Key("a-slash:%(i)d"),

    "shift right [<i>]"             : Cmd("(call-interactively 'python-indent-shift-right)"),
    "shift left [<i>]"              : Cmd("(call-interactively 'python-indent-shift-left)"),
    "align regexp"                  : Minibuf("align-regexp"),

    "indent"                        : Cmd("(call-interactively 'indent-region)"),


    # text commands
    "capitalize"                    : Key("a-c"),
    "bigger"                        : Key("a-u"),
    "smaller"                       : Key("a-l"),

    # navigation commands
    "jump <charrule>"               : Key("a-enter") + PressKey(),
    "jump char <charrule>"          : Key("c-u,a-enter") + PressKey(),

    # save mark, almost never use, need to get used to
    #"push"                         : Key("c-space,c-space"),

    "snap [<i>]"                    : Key("c-u,c-space:%(i)d"),
    "big snap [<i>]"                : Key("c-t,c-space:%(i)d"),

    "num <big>"                     : EmacsText("%(big)d"),
    "insert character"              : Key("c-t,8,enter"),
    "complete"                      : Minibuf("company-complete"),
    "open this"                     : Minibuf("find-file-at-point"),
    "shell command"                 : Minibuf("etc-shell-command"),
    "insert path"                   : InsertString("(buffer-file-name)"),
    "insert base name"              : InsertString("(file-name-base (buffer-file-name))"),
    "insert buffer name"            : InsertString("(buffer-name)"),
    "insert name without extension" : InsertString("(file-name-sans-extension (buffer-file-name))"),
    "insert directory"              : InsertString("(file-name-directory (buffer-file-name))"),
    "insert extension"              : InsertString("(file-name-extension (buffer-file-name))"),
    "insert username"               : InsertString('(user-login-name)'),

    "switch previous"               : Key("c-t,c-left"),
    "switch next"                   : Key("c-t,c-right"),

    "search"                        : Key('c-s'),
    "lurch"                         : Key('c-r'),
    "swoop"                         : Key("a-i"),
    "show kill ring"                : Key("cs-y"),

    "discard"                       : Key("c-c,c-k"),

    "view <charrule>"               : Key("a-d") + PressKey(),
}

EmacsRule = makeContextualRule("Emacs", _mapping, emacsExtras, emacsDefaults)
EmacsRule.context.addRequirement(IsEmacs)
