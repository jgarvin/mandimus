from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation


@registerRule
class XMonadRule(SeriesMappingRule):
    mapping  = {
        "mon (one | left)"            : Key("ca-p"),
        "mon (two | middle | center)" : Key("ca-f"),
        "mon (three | right)"         : Key("ca-u"),
        "move mon one"                : Key("csa-p"),
        "move mon two"                : Key("csa-f"),
        "move mon three"              : Key("csa-u"),
        "desktop <d>"                 : Key("ca-%(d)d"),
        "move desktop <d>"            : Key("csa-%(d)d"),
        "post win [<n>]"              : Key("ca-e:%(n)d"),
        "pre win [<n>]"               : Key("ca-o:%(n)d"),
        "move next"                   : Key("cas-e"),
        "move previous"               : Key("cas-o"),
        "expand [<n>]"                : Key("ca-i:%(n)d"),
        "shrink [<n>]"                : Key("ca-n:%(n)d"),
        "cycle"                       : Key("ca-y"),
        "destroy window"              : Key("ca-x"),
        "master"                      : Key("ca-enter"),
        "add master"                  : Key("ca-comma"),
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
