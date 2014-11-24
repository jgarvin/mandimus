from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation
from rules.emacs.Emacs import Emacs

@registerRule
class CUARule(SeriesMappingRule):
    mapping = {
        "copy"                  : Key("c-c"),
        "cut"                   : Key("c-x"),
        "paste"                 : Key("c-v"),
        "term paste"            : Key("s-insert"),
        "select all"            : Key("c-a"),
        "undo [that]"           : Key("c-z"),
        "redo [that]"           : Key("c-y"),
        "next form"             : Key("tab"),
        "previous form"         : Key("s-tab"),
        "axe"                   : Key("escape"),
        "find [<search_terms>]" : Key("c-f") + Text("%(search_terms)s"),
        "find next"             : Key("F3"),
        "pade"                  : Key("pgup"),
        "page"                  : Key("pgdown"),
        "home"                  : Key("home"),
        "edge"                  : Key("end"),
        "left [<n>]"            : Key("left:%(n)d"),
        "right [<n>]"           : Key("right:%(n)d"),
        "up [<n>]"              : Key("up:%(n)d"),
        "down [<n>]"            : Key("down:%(n)d"),
        "pat [<n>]"             : Key("delete:%(n)d"),
        "tap [<n>]"             : Key("backspace:%(n)d"),
        "pa"                    : Key("space"),
        "slap [<n>]"            : Key("enter:%(n)d"),
        "num <big>"             : Text("%(big)d"),
    }

    extras = [
        Dictation("text"),
        Dictation("search_terms"),
        Integer("n", 2, 20),
        Integer("big", 0, 2**14),
    ]

    defaults = {
        "search_terms" : "",
        "n": 1,
    }

    @classmethod
    def activeForWindow(cls, window):
        return not Emacs.activeForWindow(window)
