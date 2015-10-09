from Actions import Key, Text
from protocol import Integer, Dictation, RuleType
from requirements.Emacs import NotEmacs
from rules.ContextualRule import makeContextualRule

_mapping = {
    "find [<search_terms>]" : Key("c-f") + Text("%(search_terms)s"),
}

_extras = [
    Dictation("search_terms"),
]

CUAFindRule = makeContextualRule("CUAFind", _mapping, _extras, {},
                                 ruleType=RuleType.TERMINAL)
CUAFindRule.context.addRequirement(NotEmacs)

_mapping = {
    "copy"                : Key("c-c"),
    "cut"                 : Key("c-x"),
    "paste"               : Key("c-v"),
    "term paste"          : Key("s-insert"),
    "select all"          : Key("c-a"),
    "select per"          : Key("cs-left"),
    "select pro"          : Key("cs-right"),
    "select home"         : Key("s-home"),
    "select edge"         : Key("s-end"),
    "undo"                : Key("c-z"),
    "redo"                : Key("c-y"),
    "next form"           : Key("tab"),
    "previous form"       : Key("s-tab"),
    "axe"                 : Key("escape"),
    "find next [<n>]"     : Key("F3:%(n)d"),
    "find previous [<n>]" : Key("s-F3:%(n)d"),
    "leaf [<n>]"          : Key("pgdown:%(n)d"),
    "feel [<n>]"          : Key("pgup:%(n)d"),
    "home"                : Key("home"),
    "edge"                : Key("end"),
    "left [<n>]"          : Key("left:%(n)d"),
    "right [<n>]"         : Key("right:%(n)d"),
    "up [<n>]"            : Key("up:%(n)d"),
    "down [<n>]"          : Key("down:%(n)d"),
    "thump [<n>]"          : Key("delete:%(n)d"),
    "knock [<n>]"         : Key("backspace:%(n)d"),
    "slap [<n>]"          : Key("enter:%(n)d"),
    "num <big>"           : Text("%(big)d"),
    "per [<n>]"           : Key("c-left:%(n)d"),
    "pro [<n>]"           : Key("c-right:%(n)d"),
    "chip [<n>]"          : Key("c-backspace:%(n)d"),
    "pitch [<n>]"         : Key("c-delete:%(n)d"),
    "top side"            : Key("c-home"),
    "bottom"              : Key("c-end"),
    "save file"           : Key("c-s"),
    "open file"           : Key("c-o"),
    "new file"            : Key("c-n"),
}

_extras = [
    Integer("n", 3, 20),
    Integer("i", 3, 8),
    Integer("big", 0, 2**14),
]

_defaults = {
    "search_terms" : "",
    "n": 1,
    "i": 1,
}

CUARule = makeContextualRule("CUA", _mapping, _extras, _defaults)
CUARule.context.addRequirement(NotEmacs)
