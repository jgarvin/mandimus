from Actions import Key, Action, Text
from protocol import Integer, Dictation, RuleType
from requirements.WindowRequirement import WindowRequirement
from requirements.Terminal import IsTerminal
from rules.ContextualRule import makeContextualRule
from rules.emacs.Cmd import CharCmd

_mapping  = {
    "home" : Key("c-a"),
    "edge" : Key("c-e"),
    "copy"                : Key("cs-c"),
    "undo"                : Key("c-slash"),
    "paste"               : Key("cs-v"),
    "per [<n>]"           : Key("c-left:%(n)d"),
    "pro [<n>]"           : Key("c-right:%(n)d"),
    "magnify [<i>]"                 : Key("cs-plus:%(i)d"),
    "demagnify [<i>]"               : Key("c-minus:%(i)d"),
    "axe [<n>]"          : Key("c-g:%(n)d"),
    "left [<n>]"          : Key("left:%(n)d"),
    "right [<n>]"         : Key("right:%(n)d"),
    "hike [<n>]"          : Key("up:%(n)d"),
    "slide [<n>]"         : Key("down:%(n)d"),
    "kill [<n>]"          : Key('c-k:%(n)d'),
    "bonk [<n>]"          : Key("delete:%(n)d"),
    "knock [<n>]"         : Key("backspace:%(n)d"),
    "slap [<n>]"          : Key("enter:%(n)d"),
    "history"           : Key("c-r"),
    "background"        : Key("c-z"),
    "interrupt"         : Key("c-c,c-c"),
    "chip [<n>]"          : Key("c-w:%(n)d"),
    "pitch [<n>]"         : Key("a-d:%(n)d"),
    "run top"         : Text("top") + Key("enter"),
    "run H top"         : Text("htop") + Key("enter"),
    "switch user" : Text("su - "),
    "C D" : Text("cd "),
    "run D message" : Text("dmesg") + Key("enter"),
    "S S H" : Text("ssh "),
    # Also in the emacs rules, but since emacs rules are mutually exclusive with these
    # both definition should never be active at the same time.
    "view <charrule>"     : CharCmd("(md-select-window-with-glyph %s)"),
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

TerminalRule = makeContextualRule("Terminal", _mapping, _extras, _defaults)
TerminalRule.context.addRequirement(IsTerminal)
