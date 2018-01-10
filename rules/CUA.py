from Actions import Key, Text
from protocol import Integer, Dictation, RuleType
from requirements.Emacs import NotEmacs
from rules.emacs.Cmd import CharCmd
from rules.ContextualRule import makeContextualRule

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
    "find"                : Key("c-f"),
    "find it next [<n>]"     : Key("F3:%(n)d"),
    "find it previous [<n>]" : Key("s-F3:%(n)d"),
    "leaf [<n>]"          : Key("pgdown:%(n)d"),
    "feel [<n>]"          : Key("pgup:%(n)d"),
    "home"                : Key("home"),
    "edge"                : Key("end"),
    "left [<n>]"          : Key("left:%(n)d"),
    "right [<n>]"         : Key("right:%(n)d"),
    "hike [<n>]"          : Key("up:%(n)d"),
    "slide [<n>]"         : Key("down:%(n)d"),
    "bonk [<n>]"          : Key("delete:%(n)d"),
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

    # Also in the emacs rules, but since emacs rules are mutually exclusive with these
    # both definition should never be active at the same time.
    "view <charrule>"     : CharCmd("(md-select-window-with-glyph %s)"),
}

_extras = [
    Integer("n", 3, 100),
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
