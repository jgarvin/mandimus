from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from Actions import Key, Text

@registerRule
class Dired(SeriesMappingRule):
    mapping = {
        "rename"              : Key("R"),
        "toggle read only"    : Key("c-x,c-q"),
        "pick"                : Key("m"),
        "unpick"              : Key("u"),
        "unpick all"          : Key("U"),
        "pick executables"    : Text("**"),
        "pick links"          : Text("*@"),
        "pick folders"        : Text("*/"),
        "post pick"           : Key("a-rbrace"),
        "pre pick"            : Key("a-lbrace"),
        "toggle picks"        : Text("*t"),
        "pick regex"          : Text("%m"),
        "pick contents regex" : Text("%g"),
        "make dir"            : Text("+"),
        "flag"                : Key("d"),
        "unflag"              : Key("u"),
        "commit delete"       : Key("x"),
        "delete picks"        : Key("D"),
        "ascend"              : Text("^"),
        "refresh"             : Key("g"),
    }

    @classmethod
    def activeForWindow(cls, window):
        isemacs = Emacs.activeForWindow(window)
        if not isemacs:
            return False
        out = runEmacsCmd("major-mode").strip()
        return out == "dired-mode"
