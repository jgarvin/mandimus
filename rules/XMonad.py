from Actions import Key, Text
from protocol import Integer
from rules.ContextualRule import makeContextualRule

_mapping  = {
    "mon (one | left)"            : Key("ca-p"),
    "mon (two | middle | center)" : Key("ca-f"),
    "mon (three | right)"         : Key("ca-u"),
    "move mon one [<n>]"          : Key("csa-p:%(n)d"),
    "move mon two [<n>]"          : Key("csa-f:%(n)d"),
    "move mon three [<n>]"        : Key("csa-u:%(n)d"),
    "desk <d>"                    : Key("ca-%(d)d"),
    "move desk <d>"               : Key("csa-%(d)d"),
    "clock [<n>]"                 : Key("ca-e:%(n)d"),
    "wise [<n>]"                  : Key("ca-o:%(n)d"),
    "move clock [<n>]"            : Key("cas-e:%(n)d"),
    "move wise [<n>]"             : Key("cas-o:%(n)d"),
    "expand [<n>]"                : Key("ca-i:%(n)d"),
    "shrink [<n>]"                : Key("ca-n:%(n)d"),
    "cycle [<n>]"                 : Key("ca-y:%(n)d"),
    "destroy window"              : Key("ca-x"),
    "master"                      : Key("ca-enter"),
    "add master [<n>]"            : Key("ca-comma:%(n)d"),
    "remove master"               : Key("ca-period"),
    "editor"                      : Key("csa-w"),
    "browser"                     : Key("csa-b"),
    "new terminal"                : Key("csa-t"),
    "restart window manager"      : Key("ca-q"),
}

_extras = [
    Integer("n", 2, 20),
    Integer("d", 0, 10),
]

_defaults = {
    "n": 1,
}

XMonadRule = makeContextualRule("XMonad", _mapping, _extras, _defaults, seriesMergeGroup=1)
XMonadRule.activate()
