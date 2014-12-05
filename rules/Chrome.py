from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from MappingRule import MappingRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation

class WebSearch(Action):
    def __call__(self, extras={}):
        (Key("c-l, c-a, backspace")
         + Text(self.data + (" %(text)s" % extras))
         + Key("enter"))()

@registerRule
class ChromeSearch(MappingRule):
    mapping = {
        "wikipedia [<text>]"            : WebSearch("wk"),
        "youtube [<text>]"              : WebSearch("yt"),
        "dictionary [<text>]"           : WebSearch("dict"),
        "thesaurus [<text>]"            : WebSearch("thes"),
        "amazon [<text>]"               : WebSearch("az"),
        "c plus plus [<text>]"          : WebSearch("cpp"),
        "facebook [<text>]"             : WebSearch("fb"),
        "game facks [<text>]"           : WebSearch("gfaqs"),
        "images [<text>]"               : WebSearch("im"),
        "stack overflow [<text>]"       : WebSearch("so"),
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
        return ChromeRule.activeForWindow(window)

@registerRule
class ChromeRule(SeriesMappingRule):
    mapping  = {
        "new"                           : Key("c-t"),
        "close [<n>]"                   : Key("c-w:%(n)d"),
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
        "move right [<n>]"              : Key("as-l:%(n)d"),
        "move left [<n>]"               : Key("as-h:%(n)d"),
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
        "link"                          : Key("escape") + Key("f"),
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

