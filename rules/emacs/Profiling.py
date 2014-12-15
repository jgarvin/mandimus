from Actions import Key, Text
from EventLoop import getLoop
import EventList
from EventList import FocusChangeEvent
from rules.Elements import Dictation, Integer
from rules.MappingRule import MappingRule
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd, Minibuf
from rules.Rule import registerRule
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText
from rules.emacs.Base import EmacsBase
from rules.emacs.Cmd import CharCmd
from wordUtils import extractWords
import SelectOption
from Window import getFocusedWindow

@registerRule
class ProfilingRule(EmacsBase):
    mapping = {
        "profiler start"         : Minibuf("profiler-start"),
        "profiler stop"          : Minibuf("profiler-stop"),
        "profiler report"        : Minibuf("profiler-report"),
        "instrument function"    : Minibuf("elp-instrument-function"),          
        "instrument results"     : Minibuf("elp-results"),         
        "instrument restore"     : Minibuf("elp-restore-function"),          
        "instrument restore all" : Minibuf("elp-restore-all"),      
        "instrument master"      : Minibuf("elp-set-master"),         
        "instrument package"     : Minibuf("elp-instrument-package"),          
    }
