from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation

@registerRule
class ChromeRule(SeriesMappingRule):
    mapping  = {
        "new"                           : Key("c-t"),
        "close"                         : Key("c-w"),
        "address"                       : Key("c-l"),
        "post"                          : Key("c-tab"),
        "pre"                           : Key("cs-tab"),
        "(reopen tab | undo close tab)" : Key("cs-t"),
        "back"                          : Key("a-left"),
        "forward"                       : Key("a-right"),
        "refresh"                       : Key("F5"),
        "reopen tab"                    : Key("cs-t"),
        "tab"                           : Key("tab"),
        "reload"                        : Key("c-r"),
        "refresh"                       : Key("c-r"),
        "search <text>"                 : Key("c-l, c-a, backspace") + Text("%(text)s") + Key("enter"),
        "zoom in [<n>]"                 : Key("c-plus:%(n)d"),
        "zoom out [<n>]"                : Key("c-minus:%(n)d"),
        # these are provided by the 'tabloid' extension
        # TODO: not necessary, cs-pgdown/pgup
        "move tab right"                : Key("as-l"),
        "move tab left"                 : Key("as-h"),
        "move tab to start"             : Key("as-k"),
        "move tab to end"               : Key("as-j"),
        # these are provided by the 'tabasco' extension
        "close other tabs"              : Key("as-o"),
        "close tabs to the right"       : Key("as-r"),
        "close right tabs"              : Key("as-r"),
        "pin tab"                       : Key("as-p"),
        "wikipedia [<text>]"            : Key("c-l, c-a, backspace") + Text("wk %(text)s") + Key("enter"),
        "youtube [<text>]"              : Key("c-l, c-a, backspace") + Text("yt %(text)s") + Key("enter"),
        }

    extras = [
        Dictation("text"),
        Integer("n", 2, 20),
        ]

    defaults = {
        'n' : 1,
        }
    
    @classmethod
    def activeForWindow(cls, window):
        return "chrome" in window.wmclass or "chromium" in window.wmclass

