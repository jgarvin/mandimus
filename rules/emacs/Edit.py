from Actions import Key, Text, SelectChoice
from EventLoop import getLoop
import EventList
from EventList import FocusChangeEvent
from rules.Elements import Dictation, Integer
from rules.MappingRule import MappingRule
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd, EmacsCommandWatcher
from rules.Rule import registerRule
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText
from rules.emacs.Base import EmacsBase
from rules.BaseRules import CharRule
from wordUtils import extractWords
import SelectOption
from Window import getFocusedWindow

# TODO: would be nice for all of these
# to be ace'able
@registerRule
class UnitRule(MappingRule):
    refOnly = True
    mapping = {
        "word"     : "'word",
        "line"     : "'line",
        "ace"      : "'ace",
        "ace line" : "'ace-line",
        "graph"    : "'paragraph",
        "block"    : "'block",
        "larp"     : "'parens",
        "lack"     : "'brackets",
        "lace"     : "'lace",
        "lesser"   : "'angles",
    }


@registerRule
class ActionRule(MappingRule):
    refOnly = True
    mapping = {
        "mark" : "'mark",
        "cut"  : "'cut",
        "copy" : "'copy",
        "dupe" : "'dupe",
        "swap" : "'swap",
    }


class CharCmd(Cmd):
    classLog = True
    def _lisp(self, extras={}):
        char = CharRule.lookup(extras['words'].split()[1]) 
        return self.data % char
    
class Zap(Cmd):
    #classLog = True
    def _lisp(self, extras={}):
        char = CharRule.lookup(extras['words'].split()[1])
        return "(zap-to-char 1 ?%s)" % char

@registerRule
class EditRules(EmacsBase):
    mapping = {
        "zap <charrule> [<n>]"         : CharCmd("(zap-to-char 1 ?%s)"),
    }

    
