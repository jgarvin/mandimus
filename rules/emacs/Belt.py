from Actions import Key, Text, SelectChoice
from EventLoop import getLoop
import EventList
from EventList import FocusChangeEvent
from rules.Elements import Dictation, Integer
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
class BeltRules(EmacsBase):
    mapping = {
        "near <charrule> [<n>]" : CharCmd('(md-activate-belt-item "nearest" ?%s)', log=True),
        "jerk <charrule> [<n>]" : CharCmd('(md-activate-belt-item "kill" ?%s)', log=True),
        "beat <charrule> [<n>]" : CharCmd('(md-activate-belt-item "frequency" ?%s)', log=True),  
    }

    
