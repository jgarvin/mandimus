from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation


@registerRule
class XMonadRule(SeriesMappingRule):
    allowCombining = False

    mapping  = {
        "mon (one | left)"            : Key("ca-p"),
        "mon (two | middle | center)" : Key("ca-f"),
        "mon (three | right)"         : Key("ca-u"),
        "move mon one [<n>]"          : Key("csa-p:%(n)d"),
        "move mon two [<n>]"          : Key("csa-f:%(n)d"),
        "move mon three [<n>]"        : Key("csa-u:%(n)d"),
        "desktop <d>"                 : Key("ca-%(d)d"),
        "move desktop <d>"            : Key("csa-%(d)d"),
        "post win [<n>]"              : Key("ca-e:%(n)d"),
        "per win [<n>]"               : Key("ca-o:%(n)d"),
        "move next [<n>]"             : Key("cas-e:%(n)d"),
        "move previous [<n>]"         : Key("cas-o:%(n)d"),
        "expand [<n>]"                : Key("ca-i:%(n)d"),
        "shrink [<n>]"                : Key("ca-n:%(n)d"),
        "cycle [<n>]"                 : Key("ca-y:%(n)d"),
        "destroy window"              : Key("ca-x"),
        "master"                      : Key("ca-enter"),
        "add master [<n>]"            : Key("ca-comma:%(n)d"),
        "remove master"               : Key("ca-period"),
        "editor"                      : Key("ca-w"),
        "browser"                     : Key("ca-b"),
        "new terminal"                : Key("csa-t"),
        "restart window manager"      : Key("ca-q"),
        }
    
    extras = [
        Integer("n", 2, 20),
        Integer("d", 0, 10),
        Dictation("text"),
        ]
    
    defaults = {
        "n": 1,
        }

    @classmethod
    def activeForWindow(cls, window):
        return True
