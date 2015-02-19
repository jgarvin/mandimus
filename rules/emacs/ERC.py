import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Text import EmacsText

_mapping = {
    "hiss"                : Key("a-p"),
    "piss"                : Key("a-n"),
    "slash join [<text>]" : EmacsText("/join #%(text)s"),
    "smiley wink"         : EmacsText(";)"),
    "smiley tongue"       : EmacsText(":P", lower=False),
    "smiley wink tongue"  : EmacsText(";P", lower=False),
    "slash part"          : EmacsText("/part"),
    "kick dragon"         : EmacsText("/me kicks Dragon"),
    "slash me [<text>]"   : EmacsText("/me %(text)s"),
    "slash message"       : EmacsText("/msg"),
}

ERCRule = makeContextualRule("ERC", _mapping, emacsExtras, emacsDefaults)
ERCRule.context.addRequirement(IsEmacs)
ERCRule.context.addRequirement(ModeRequirement(modes="erc-mode"))
