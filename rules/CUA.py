from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation
from rules.Emacs import EmacsRule

@registerRule
class CUARule(SeriesMappingRule):
    mapping = {
        "copy" : Key("c-c"),
        "cut" : Key("c-x"),
        "paste" : Key("c-v"),
        "term paste" : Key("s-insert"),
        "select all" : Key("c-a"),
        "undo [that]" : Key("c-z"),
        "redo [that]" : Key("c-y"),
        "next form" : Key("tab"),
        "previous form" : Key("s-tab"),
        "escape" : Key("escape"),
        "find [<search_terms>]" : Key("c-f") + Text("%(search_terms)s"),
        "find next" : Key("F3"),
        "pade" : Key("pgup"),
        "page" : Key("pgdown"),
        "home" : Key("home"),
        "edge": Key("end"),
        }

    extras = [
        Dictation("text"),
        Dictation("search_terms"),
        ]

    defaults = {
        "search_terms" : "",
        }

    @classmethod
    def activeForWindow(cls, window):
        return not EmacsRule.activeForWindow(window)
