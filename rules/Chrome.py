from Actions import Key, Action, Text
from protocol import Integer, Dictation, RuleType
from requirements.WindowRequirement import WindowRequirement
from rules.ContextualRule import makeContextualRule

IsChrome = WindowRequirement(wmclass=('chrome', 'chromium', 'chromium-browser', 'Chromium-browser'))

class WebSearch(Action):
    def __call__(self, extras={}):
        text = self.data
        if self.data:
            text += " "
        # go to address bar, deleting existing text
        (Key("c-l, c-a, backspace")
         # enter the text
         + Text(text + (" %(text)s"))
         # delete any autocomplete results off the end and enter
         + Key("delete, enter"))(extras)

_mapping = {
    "wikipedia [<text>]"      : WebSearch("wk"),
    "youtube [<text>]"        : WebSearch("yt"),
    "dictionary [<text>]"     : WebSearch("dict"),
    "thesaurus [<text>]"      : WebSearch("thes"),
    "amazon [<text>]"         : WebSearch("az"),
    "see plus plus [<text>]"  : WebSearch("cpp"),
    "facebook [<text>]"       : WebSearch("fb"),
    "game facks [<text>]"     : WebSearch("gfaqs"),
    "images [<text>]"         : WebSearch("im"),
    "stack overflow [<text>]" : WebSearch("so"),
    "search <text>"           : WebSearch(""),
}

_extras = [
    Dictation("text"),
    Integer("n", 3, 20),
]

_defaults = {
    'n' : 1,
}

ChromeSearchRule = makeContextualRule("ChromeSearch", _mapping, _extras, _defaults, ruleType=RuleType.TERMINAL)
ChromeSearchRule.context.addRequirement(IsChrome)

_mapping  = {
    "new tab"                       : Key("c-t"),
    "new window"                    : Key("c-n"),
    "new incognito window"          : Key("cs-n"),
    "close [<n>]"                   : Key("c-w:%(n)d"),
    "address"                       : Key("c-l"),
    "next [<n>]"                    : Key("c-tab:%(n)d"),
    "previous [<n>]"                : Key("cs-tab:%(n)d"),
    "(reopen tab | undo close tab)" : Key("cs-t"),
    "back [<i>]"                    : Key("a-left:%(i)d"),
    "forward [<i>]"                 : Key("a-right:%(i)d"),
    "refresh"                       : Key("F5"),
    "reopen tab"                    : Key("cs-t"),
    "magnify [<i>]"                 : Key("c-plus:%(i)d"),
    "demagnify [<i>]"               : Key("c-minus:%(i)d"),
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

_extras = [
    Dictation("text"),
    Integer("n", 3, 20),
    Integer("i", 3, 8),
    ]

_defaults = {
    'n' : 1,
    'i' : 1,
    }

ChromeRule = makeContextualRule("Chrome", _mapping, _extras, _defaults)
ChromeRule.context.addRequirement(IsChrome)
