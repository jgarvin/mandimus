from Rule import registerRule
from SeriesMappingRule import SeriesMappingRule
from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, runCmd
from Elements import Integer, Dictation
from Window import Window

class Cmd(Action):
    def __call__(self, extras={}):
        fulldata = (self.data % extras)
        # escape single quotes, we actually close the string
        # add the escape single quote, and reopen the string
        fulldata = fulldata.replace("'", "'\\''")
        cmd = 'emacsclient -e \'(with-current-buffer "%s" %s)\''
        cmd = cmd % (Window().iconName, fulldata)
        runCmd(cmd)

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
        "toggle buff"                    : Key("c-x, b") + Key("enter"),
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
