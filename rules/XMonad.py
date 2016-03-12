from Actions import Key, Text, RepeatPreviousAction
from protocol import Integer
from rules.ContextualRule import makeContextualRule

_mapping  = {
    # "mon (one | left)"            : Key("ca-up"),
    # "mon (two | middle | center)" : Key("ca-left"),
    # "mon (three | right)"         : Key("ca-right"),
    # "move mon one [<i>]"          : Key("csa-up:%(i)d"),
    # "move mon two [<i>]"          : Key("csa-left:%(i)d"),
    # "move mon three [<i>]"        : Key("csa-right:%(i)d"),
    "East [<i>]"                    : Key("ca-space:%(i)d"),
    "move East [<i>]"               : Key("csa-space:%(i)d"),
    "West [<i>]"                    : Key("ca-backspace:%(i)d"),
    "move West [<i>]"               : Key("csa-backspace:%(i)d"),
    "desk <d>"                      : Key("ca-%(d)d"),
    "move desk <d>"                 : Key("csa-%(d)d"),
    "clock [<i>]"                   : Key("ca-e:%(i)d"),
    "wise [<i>]"                    : Key("ca-h:%(i)d"),
    "move clock [<i>]"              : Key("cas-e:%(i)d"),
    "move wise [<i>]"               : Key("cas-h:%(i)d"),
    "expand [<n>]"                  : Key("ca-equal:%(n)d"),
    "shrink [<n>]"                  : Key("ca-minus:%(n)d"),
    "cycle [<i>]"                   : Key("csa-rbracket:%(i)d"),
    "destroy window"                : Key("ca-x"),
    "master"                        : Key("ca-enter"),
    "add master [<i>]"              : Key("ca-comma:%(i)d"),
    "remove master"                 : Key("ca-period"),

    "launch emacs"                  : Key("csa-w"),
    "browser"                       : Key("csa-b"),
    "new terminal"                  : Key("csa-t"),
    "restart window manager"        : Key("csa-q"),
    "reflect X"                     : Key("csa-x"),
    "reflect Y"                     : Key("csa-y"),
    # we need this because we are a different series merge group
    'rep [<n>]'                     : RepeatPreviousAction(),
}

_extras = [
    Integer("n", 3, 20),
    Integer("d", 0, 10),
    Integer("i", 3, 8),
]

_defaults = {
    "n": 1,
    "i": 1,
}

XMonadRule = makeContextualRule("XMonad", _mapping, _extras, _defaults, seriesMergeGroup=1)
XMonadRule.activate()
