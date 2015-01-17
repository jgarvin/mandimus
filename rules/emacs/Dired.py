from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from Actions import Key, Text
from rules.emacs.Base import EmacsBase

@registerRule
class Dired(EmacsBase):
    majorMode = "dired-mode"
    
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
        "per pick"            : Key("a-lbrace"),
        "toggle picks"        : Text("*t"),
        "pick regex"          : Text("%m"),
        "pick contents regex" : Text("%g"),
        "make folder"         : Text("+"),
        "flag"                : Key("d"),
        "unflag"              : Key("u"),
        "commit delete"       : Key("x"),
        "delete picks"        : Key("D"),
        "ascend"              : Text("^"),
        "refresh"             : Key("g"),
        "open picks"          : Key("c-u,0,F"),
        "copy"                : Key("C"),
    }

