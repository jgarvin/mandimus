from Actions import Key, Text
from protocol import Integer, Dictation
from requirements.Emacs import NotEmacs
from rules.ContextualRule import makeContextualRule

_mapping = {
    "copy"                  : Key("c-c"),
    "cut"                   : Key("c-x"),
    "paste"                 : Key("c-v"),
    "term paste"            : Key("s-insert"),
    "select all"            : Key("c-a"),
    "undo"                  : Key("c-z"),
    "redo"                  : Key("c-y"),
    "next form"             : Key("tab"),
    "previous form"         : Key("s-tab"),
    "axe"                   : Key("escape"),
    "find [<search_terms>]" : Key("c-f") + Text("%(search_terms)s"),
    "find next [<n>]"       : Key("F3:%(n)d"),
    "find previous [<n>]"   : Key("s-F3:%(n)d"),
    "leaf [<n>]"            : Key("pgdown:%(n)d"),
    "feel [<n>]"            : Key("pgup:%(n)d"),
    "home"                  : Key("home"),
    "edge"                  : Key("end"),
    "left [<n>]"            : Key("left:%(n)d"),
    "right [<n>]"           : Key("right:%(n)d"),
    "up [<n>]"              : Key("up:%(n)d"),
    "down [<n>]"            : Key("down:%(n)d"),
    "pat [<n>]"             : Key("delete:%(n)d"),
    "knock [<n>]"           : Key("backspace:%(n)d"),
    "slap [<n>]"            : Key("enter:%(n)d"),
    "num <big>"             : Text("%(big)d"),
}

_extras = [
    Dictation("text"),
    Dictation("search_terms"),
    Integer("n", 2, 20),
    Integer("big", 0, 2**14),
]

_defaults = {
    "search_terms" : "",
    "n": 1,
}

CUARule = makeContextualRule("CUA", _mapping, _extras, _defaults)
CUARule.context.addRequirement(NotEmacs)
CUARule.activate()

