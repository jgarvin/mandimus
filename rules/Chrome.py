from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation

class WebSearch(Action):
    def __call__(self, extras={}):
        (Key("c-l, c-a, backspace")
         + Text(self.data + (" %(text)s" % extras))
         + Key("enter"))()

@registerRule
class ChromeRule(SeriesMappingRule):
    mapping  = {
        "new"                           : Key("c-t"),
        "close"                         : Key("c-w"),
        "address"                       : Key("c-l"),
        "post [<n>]"                    : Key("c-tab:%(n)d"),
        "pre [<n>]"                     : Key("cs-tab:%(n)d"),
        "(reopen tab | undo close tab)" : Key("cs-t"),
        "back"                          : Key("a-left"),
        "forward"                       : Key("a-right"),
        "refresh"                       : Key("F5"),
        "reopen tab"                    : Key("cs-t"),
        "reload"                        : Key("c-r"),
        "refresh"                       : Key("c-r"),
        "search <text>"                 : Key("c-l, c-a, backspace") + Text("%(text)s") + Key("enter"),
        "zoom in [<n>]"                 : Key("c-plus:%(n)d"),
        "zoom out [<n>]"                : Key("c-minus:%(n)d"),
        # these are provided by the 'tabloid' extension
        # TODO                          : not necessary, cs-pgdown/pgup
        "move right"                    : Key("as-l"),
        "move left"                     : Key("as-h"),
        "move start"                    : Key("as-k"),
        "move end"                      : Key("as-j"),
        # these are provided by the 'tabasco' extension
        "close other tabs"              : Key("as-o"),
        "close tabs to the right"       : Key("as-r"),
        "close right tabs"              : Key("as-r"),
        "pin tab"                       : Key("as-p"),
        # provided ty the "duplicate tab" extension
        "duplicate"                     : Key("as-d"),
        #misc
        "private browsing"              : Key("cs-n"),
        "link"                          : Key("f"),
        # search types
        "wikipedia [<text>]"            : WebSearch("wk"),
        "youtube [<text>]"              : WebSearch("yt"),
        "dictionary [<text>]"           : WebSearch("dict"),
        "thesaurus [<text>]"            : WebSearch("thes"),
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

