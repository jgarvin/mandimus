from Actions import Key, Text, SelectChoice
from EventLoop import getLoop
import EventList
from EventList import FocusChangeEvent
from rules.Elements import Dictation, Integer, RuleRef
from rules.BaseRules import AlphaRule
from rules.MappingRule import MappingRule
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd
from wordUtils import extractWords
import SelectOption
from Window import getFocusedWindow

@registerRule
class ColorRule(MappingRule):
    refOnly = True
    mapping = {
        "red"    : "red",
        "green"  : "green",
        "white"  : "white",
        "purple" : "purple",
        "yellow" : "yellow",
        "orange" : "orange",
    }

@registerRule
class AccentRule(MappingRule):
    refOnly = True
    mapping = {
        "circle" : 0x030a,
        "corner" : 0x031a,
        "hair"   : 0x030f,
    }
    
class PickSymbol(Cmd):
    classLog = False
    def _lisp(self, extras={}):
        words = extras['words'].split()
        color = ColorRule.mapping[words[0]]
        letter = AlphaRule.mapping[words[1]]
        mark = AccentRule.mapping[words[2]] if len(words) > 2 else None
        mark = ("#x%x" % mark) if mark else "nil"
        return '(md-hl-pick-symbol "%s" "%s" %s)' % (letter, color, mark)
 
@registerRule
class SymbolPicker(EmacsBase):
    mapping = {
        "<colorrule> <alpharule> [<accentrule>]" : PickSymbol(),
    }

    alpharef = RuleRef(AlphaRule, "alpharule")
    colorref = RuleRef(ColorRule, "colorrule")
    accentref = RuleRef(AccentRule, "accentrule")
        
    extras = EmacsBase.extras + [
        alpharef,
        colorref,
        accentref,
        ]

    
