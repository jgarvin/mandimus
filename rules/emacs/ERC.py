import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Text import EmacsText
from protocol import RuleType

IsErcMode = ModeRequirement(modes="erc-mode")

_mapping = {
    "slash join [<text>]" : EmacsText("/join #%(text)s"),
    "slash me [<text>]"   : EmacsText("/me %(text)s"),
}

ERCTextRule = makeContextualRule("ERCText", _mapping, emacsExtras, emacsDefaults,
                                 ruleType=RuleType.TERMINAL)
ERCTextRule.context.addRequirement(IsEmacs)
ERCTextRule.context.addRequirement(IsErcMode)

_mapping = {
    "prior"              : Key("a-p"),
    "future"             : Key("a-n"),
    "smiley wink"        : EmacsText(";)"),
    "smiley tongue"      : EmacsText(":P", lower=False),
    "smiley wink tongue" : EmacsText(";P", lower=False),
    "slash part"         : EmacsText("/part"),
    "kick dragon"        : EmacsText("/me kicks Dragon"),
    "slash message"      : EmacsText("/msg"),
    "slash whois"        : EmacsText("/whois"),
}

ERCRule = makeContextualRule("ERC", _mapping, emacsExtras, emacsDefaults)
ERCRule.context.addRequirement(IsEmacs)
ERCRule.context.addRequirement(IsErcMode)
