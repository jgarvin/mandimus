from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, Action
from Rule import commandTally, registerRule
from SeriesMappingRule import SeriesMappingRule
from Elements import Integer, Dictation


@registerRule
class XMonadRule(SeriesMappingRule):
    mapping  = {
        "mon left [<n>]" : Key("ca-backspace:%(n)d"),
        "mon right [<n>]" : Key("ca-space:%(n)d"),
        "move left" : Key("ca-a"),
        "move right" : Key("ca-t"),
        "next [<n>]" : Key("ca-e:%(n)d"),
        "previous [<n>]" : Key("ca-o:%(n)d"),
        "move next" : Key("cas-e"),
        "move previous" : Key("cas-o"),
        "expand [<n>]" : Key("ca-i:%(n)d"),
        "shrink [<n>]" : Key("ca-n:%(n)d"),
        "cycle" : Key("ca-y"),
        "kill window" : Key("ca-x"),
        "make master" : Key("ca-enter"),
        "editor" : Key("ca-w"),
        "browser" : Key("ca-b"),
        "new terminal" : Key("csa-t"),
        "restart window manager" : Key("ca-q"),
        }
    
    extras = [
        Integer("n", 1, 20),
        Dictation("text"),
        ]
    
    defaults = {
        "n": 1,
        }

    @classmethod
    def activeForWindow(cls, window):
        return True
