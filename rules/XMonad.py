from Actions import Key, Text, RepeatPreviousAction
from protocol import Integer, RuleType
from rules.ContextualRule import makeContextualRule


_extras = [
    Integer("n", 3, 100),
    Integer("d", 0, 10),
    Integer("i", 3, 8),
]

_defaults = {
    "n": 1,
    "i": 1,
}

# put the more dangerous stuff by itself
_mapping  = {
    "destroy window"                : Key("ca-x"),
    "restart window manager"        : Key("csa-q"),
    "launch emacs"                  : Key("csa-w"),
}

XMonadIndependentRule = makeContextualRule("XMonadIndependent", _mapping, _extras, _defaults, ruleType=RuleType.INDEPENDENT)
XMonadIndependentRule.activate()

_mapping  = {
    # "mon (one | left)"            : Key("ca-up"),
    # "mon (two | middle | center)" : Key("ca-left"),
    # "mon (three | right)"         : Key("ca-right"),
    # "move mon one [<i>]"          : Key("csa-up:%(i)d"),
    # "move mon two [<i>]"          : Key("csa-left:%(i)d"),
    # "move mon three [<i>]"        : Key("csa-right:%(i)d"),
    "East [<i>]"                    : Key("ca-backspace:%(i)d"),
    "move East [<i>]"               : Key("csa-backspace:%(i)d"),
    "West [<i>]"                    : Key("ca-space:%(i)d"),
    "move West [<i>]"               : Key("csa-space:%(i)d"),
    # "desk <d>"                      : Key("ca-%(d)d"),
    # "move desk <d>"                 : Key("csa-%(d)d"),
    "clock [<i>]"                   : Key("ca-e:%(i)d"),
    "wise [<i>]"                    : Key("ca-h:%(i)d"),
    "move clock [<i>]"              : Key("cas-e:%(i)d"),
    "move wise [<i>]"               : Key("cas-h:%(i)d"),
    "expand [<n>]"                  : Key("ca-equal:%(n)d"),
    "shrink [<n>]"                  : Key("ca-minus:%(n)d"),
    "cycle [<i>]"                   : Key("csa-rbracket:%(i)d"),
    # "destroy window"                : Key("ca-x"),
    "master"                        : Key("ca-enter"),
    "add master [<i>]"              : Key("ca-comma:%(i)d"),
    "remove master"                 : Key("ca-period"),

    "browser"                       : Key("csa-b"),
    "new terminal"                  : Key("csa-t"),
    "reflect X"                     : Key("csa-x"),
    "reflect Y"                     : Key("csa-y"),
    # we need this because we are a different series merge group
    # 'rep [<n>]'                     : RepeatPreviousAction(),
}


numeric_to_symbol = ["exclamation", "at", "hash", "dollar", "percent", "caret", "ampersand", "asterisk", "lparen", "rparen"]

number_names = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

_desk_mapping = { "desk {}".format(number_names[i]) : Key("ca-{}".format(numeric_to_symbol[i])) for i in range(9)}

_move_desk_mapping = { "move desk {}".format(number_names[i]) : Key("csa-{}".format(numeric_to_symbol[i])) for i in range(9)}

_mapping.update(_desk_mapping)
_mapping.update(_move_desk_mapping)

# XMonadRule = makeContextualRule("XMonad", _mapping, _extras, _defaults, seriesMergeGroup=1)
XMonadRule = makeContextualRule("XMonad", _mapping, _extras, _defaults)
XMonadRule.activate()
