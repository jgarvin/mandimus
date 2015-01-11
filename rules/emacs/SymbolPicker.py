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
        "pink"   : "deep pink",
    }

class PickSymbol(Cmd):
    classLog = True
    def _lisp(self, extras={}):
        color = ColorRule.mapping[extras['words'].split()[0]]
        letter = AlphaRule.mapping[extras['words'].split()[1]]
        return '(md-hl-pick-symbol "%s" "%s")' % (letter, color)

@registerRule
class SymbolPicker(EmacsBase):
    mapping = {
        "<colorrule> <alpharule>" : PickSymbol(),
    }

    alpharef = RuleRef(AlphaRule, "alpharule")
    colorref = RuleRef(ColorRule, "colorrule")
    
    extras = EmacsBase.extras + [
        alpharef,
        colorref,
        ]

    
