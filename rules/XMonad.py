from Actions import Key, Text
from protocol import Integer
from rules.ContextualRule import makeContextualRule

_mapping  = {
    "mon (one | left)"            : Key("ca-p"),
    "mon (two | middle | center)" : Key("ca-f"),
    "mon (three | right)"         : Key("ca-u"),
    "move mon one [<i>]"          : Key("csa-p:%(i)d"),
    "move mon two [<i>]"          : Key("csa-f:%(i)d"),
    "move mon three [<i>]"        : Key("csa-u:%(i)d"),
    "desk <d>"                    : Key("ca-%(d)d"),
    "move desk <d>"               : Key("csa-%(d)d"),
    "clock [<i>]"                 : Key("ca-e:%(i)d"),
    "wise [<i>]"                  : Key("ca-o:%(i)d"),
    "move clock [<i>]"            : Key("cas-e:%(i)d"),
    "move wise [<i>]"             : Key("cas-o:%(i)d"),
    "expand [<n>]"                : Key("ca-i:%(n)d"),
    "shrink [<n>]"                : Key("ca-n:%(n)d"),
    "cycle [<i>]"                 : Key("ca-y:%(i)d"),
    "destroy window"              : Key("ca-x"),
    "master"                      : Key("ca-enter"),
    "add master [<i>]"            : Key("ca-comma:%(i)d"),
    "remove master"               : Key("ca-period"),
    "editor"                      : Key("csa-w"),
    "browser"                     : Key("csa-b"),
    "new terminal"                : Key("csa-t"),
    "restart window manager"      : Key("ca-q"),
}

_extras = [
    Integer("n", 2, 20),
    Integer("d", 0, 10),
    Integer("i", 2, 8),
]

_defaults = {
    "n": 1,
    "i": 1,
}

XMonadRule = makeContextualRule("XMonad", _mapping, _extras, _defaults, seriesMergeGroup=1)
XMonadRule.activate()
