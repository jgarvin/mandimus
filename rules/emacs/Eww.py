from Actions import Key 
from rules.Rule import registerRule
from rules.emacs.Base import EmacsBase
from rules.emacs.Text import EmacsText

@registerRule
class Eww(EmacsBase):
    majorMode = ["eww-mode"]

    mapping = {
        "external"          : Key("ampersand"),
    }

