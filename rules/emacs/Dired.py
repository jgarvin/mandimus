from Actions import Key, Text
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults


_mapping = {
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

DiredRule = makeContextualRule("Dired", _mapping, emacsExtras, emacsDefaults)
DiredRule.context.addRequirement(IsEmacs)
DiredRule.context.addRequirement(ModeRequirement(modes="dired-mode"))

