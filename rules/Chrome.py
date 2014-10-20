from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation

@registerRule
class ChromeRule(SeriesMappingRule):
    mapping  = {
        "new tab" : Key("c-t"),
        "close tab" : Key("c-w"),
        "address" : Key("c-l"),
        "next tab" : Key("c-tab"),
        "previous tab" : Key("cs-tab"),
        "(reopen tab | undo close tab)" : Key("cs-t"),
        "back" : Key("a-left"),
        "forward" : Key("a-right"),
        "refresh" : Key("F5"),
        "reopen tab" : Key("cs-t"),
        "enter" : Key("enter"),
        "tab" : Key("tab"),
        "reload" : Key("c-r"),
        "refresh" : Key("c-r"),
        "search <text>" : Key("c-l, c-a, backspace") + Text("%(text)s", formatting=False) + Key("enter"),
        "zoom in [<n>]" : Key("c-plus:%(n)d"),
        "zoom out [<n>]" : Key("c-minus:%(n)d"),
        # these are provided by the 'tabloid' extension
        "move tab right" : Key("as-l"),
        "move tab left" : Key("as-h"),
        "move tab to start" : Key("as-k"),
        "move tab to end" : Key("as-j"),
        # these are provided by the 'tabasco' extension
        "close other tabs" : Key("as-o"),
        "close tabs to the right" : Key("as-r"),
        "close right tabs" : Key("as-r"),
        "pin tab" : Key("as-p"),
        }

    extras = [
        Dictation("text"),
        Integer("n", 1, 20),
        ]

    defaults = {
        'n' : 1,
        }
    
    @classmethod
    def activeForWindow(cls, window):
        return "chrome" in window.wmclass or "chromium" in window.wmclass

