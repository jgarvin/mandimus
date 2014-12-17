from Actions import Key, Text
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

@registerRule
class EditRules(EmacsBase):
    mapping = {
        "zap <charrule> [<n>]"  : CharCmd("(zap-up-to-char 1 ?%s)"),
        "taze <charrule> [<n>]" : CharCmd("(zap-up-to-char -1 ?%s)"),
        "fizz <charrule> [<n>]" : CharCmd("(md-copy-up-to-char 1 ?%s)"),
        "buzz <charrule> [<n>]" : CharCmd("(md-copy-up-to-char -1 ?%s)"),
        "flip [<n>]"            : Cmd("(transpose-sexps 1)"),
        "pilf [<n>]"            : Cmd("(transpose-sexps -1)"),
        "rise [<n>]"            : Key("a-up:%(n)d"),
        "drop [<n>]"            : Key("a-down:%(n)d"),
    }
