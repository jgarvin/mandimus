from protocol import RuleType, RuleRef, makeHashedRule
from EventLoop import pushEvent
from EventList import RuleRegisterEvent
import rules.BaseRules as BaseRules
from rules.emacs.Cmd import CharCmd, Cmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "red"    : "red",
    "green"  : "green",
    "white"  : "white",
    "purple" : "purple",
    "yellow" : "yellow",
    "orange" : "orange",
}

ColorRule = makeHashedRule("ColorRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(ColorRule))

_mapping = {
    "circle" : 0x030a,
    "corner" : 0x031a,
    "hair"   : 0x030f,
}

AccentRule = makeHashedRule("AccentRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(AccentRule))
    
class PickSymbol(Cmd):
    classLog = True
    def __init__(self, data, leadingWords=1):
        self.leadingWords = leadingWords
        Cmd.__init__(self, data)

    def _lisp(self, extras={}):
        color = extras['colorrule']
        letter = BaseRules.lookup(extras)
        mark = AccentRule.rule.mapping[extras['accentrule']] if 'accentrule' in extras else None
        mark = ("#x%x" % mark) if mark else "nil"
        return '(%s "%s" %s "%s")' % (self.data, letter, mark, color)
 
_mapping = {
    "<colorrule> <alpharule> [<accentrule>]"      : PickSymbol("md-hl-insert-symbol", 0),
    "jump <colorrule> <alpharule> [<accentrule>]" : PickSymbol("md-hl-jump-symbol"),
    "toggle picker"                               : Cmd("(md-toggle-symbol-picker-mode)"),
}

_extras = emacsExtras + [
    RuleRef(AccentRule, "accentrule"),
    RuleRef(ColorRule, "colorrule"),
    ]

SymbolPickerRule = makeContextualRule("SymbolPicker", _mapping, _extras, emacsDefaults)
SymbolPickerRule.context.addRequirement(IsEmacs)

