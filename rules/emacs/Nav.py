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

# TODO: vertical versions of commands? could be handy for navigation to have
# a jump that considers vertical to cost 0 and horizontal to cost normal,
# so you could say whatever mike to go from the closing brace to mapping.

@registerRule
class NavRules(EmacsBase):
    mapping = {
        "go <charrule> [<n>]"   : CharCmd("(md-move-up-to-char 1 ?%s)"),
        "ugg <charrule> [<n>]"  : CharCmd("(md-move-up-to-char -1 ?%s)"),
        "tuck [<n>]"            : Cmd("(md-find-indentation-change 1 '>)"),
        "snug [<n>]"            : Cmd("(md-find-indentation-change -1 '>)"),
        "slack [<n>]"           : Cmd("(md-find-indentation-change 1 '<)"),
        "lacks [<n>]"           : Cmd("(md-find-indentation-change -1 '<)"),
        "sled [<n>]"            : Cmd("(md-next-whitespace-separated-thing)"),
        "dels [<n>]"            : Cmd("(md-previous-whitespace-separated-thing)"),
        "sym <charrule> [<n>]"  : CharCmd("(md-move-up-to-symbol-starting-with-char 1 ?%s)"),
        "miss <charrule> [<n>]" : CharCmd("(md-move-up-to-symbol-starting-with-char -1 ?%s)"),
        "line <charrule> [<n>]" : CharCmd("(md-find-line-starting-with-char 1 ?%s)"),
        "Nile <charrule> [<n>]" : CharCmd("(md-find-line-starting-with-char -1 ?%s)"),
        "store <charrule>"      : CharCmd("(point-to-register ?%s)"),
        "load <charrule>"       : CharCmd("(jump-to-register ?%s)"),
    }

    
