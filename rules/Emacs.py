from Rule import registerRule
from SeriesMappingRule import SeriesMappingRule
from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action, runCmd
from Elements import Integer, Dictation
from Window import Window

class Cmd(Action):
    def __call__(self, extras={}):
        fulldata = (self.data % extras)
        cmd = 'emacsclient -e \'(with-current-buffer "%s" %s)\''
        cmd = cmd % (Window().iconName, fulldata)
        runCmd(cmd)

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

        "cut"                            : Key("c-x,c-k"),
        "kill"                           : Key('c-k'),
        "(cut | kill) line"              : Cmd('(quick-cut-line)'),

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
        
        # replacing commands
        "replace"                        : Key('as-percent'),
        "replace <match> with <replace>" : Key('as-percent') + Text("%(match)s") + Key('enter') + Text("%(replace)s"),
        "yes"                            : Key('y'),
        "no"                             : Key('n'),
        
        # text commands
        "capitalize"                     : Key("a-c"),
        "upper case"                     : Key("a-u"),
        "lower case"                     : Key("a-l"),
        "parens"                         : Text("()") + Key('left'),
        "braces"                         : Text("{}") + Key('left'),
        "brackets"                       : Text("[]") + Key('left'),
        "quotes [<text>]"                : Text("\"\"") + Key('left') + Text("%(text)s"),
        "single quotes"                  : Text("''") + Key('left'),
        "angles"                         : Text("<>") + Key('left'),
        }

    extras = [
        Integer("n", 1, 20),
        Integer("line", 0, 9999),
        Dictation("text"),
        Dictation("match"),
        Dictation("replace"),
        ]

    defaults = {
        "n"                              : 1,
        "text"                           : "",
        }    

    @classmethod
    def activeForWindow(cls, window)     :
        return "emacs" in window.wmclass or "Emacs" in window.wmclass    
