from Actions import Key, Text
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults


_mapping = {
    "rename"              : Key("R"),
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
    "ascend [<n>]"        : Key("caret:%(n)d"),
    "refresh"             : Key("g"),
    "open picks"          : Key("c-u,0,F"),
    "copy"                : Key("C"),
}

## Turns out that since these are all single letter triggered,
## it's better to jus remember the letter and say the phrase for
## that. That way there's nothing different to remember between
## using voice vs. keyboard. So we comment out.

# DiredRule = makeContextualRule("Dired", _mapping, emacsExtras, emacsDefaults)
# DiredRule.context.addRequirement(IsEmacs)
# DiredRule.context.addRequirement(ModeRequirement(modes="dired-mode"))

