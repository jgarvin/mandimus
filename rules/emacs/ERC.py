import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, SelectChoice
from EventLoop import getLoop
import EventList
from EventList import FocusChangeEvent
from rules.Elements import Dictation, Integer
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd, getMajorMode, EmacsCommandWatcher
from rules.Rule import registerRule
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText
from wordUtils import extractWords
import SelectOption
from Window import getFocusedWindow

class NickWatcher(EmacsCommandWatcher):
    cmd = "(md-get-active-erc-nicknames)"
    eventType = EventList.NickEvent

watchers = []
watchers.append(NickWatcher())

class SelectNick(SelectOption.SelectOption):
    leadingTerm = "nick"
    eventType = EventList.NickEvent
    
    def _select(self, choice):
        if runEmacsCmd("(md-at-start-of-erc-input-line)").strip() == 't':
            # we're addressing them, include the colon
            EmacsText("%s: " % choice, lower=False, capitalCheck=False)()
        else:
            # we're referring to them, omit the colon
            EmacsText("%s" % choice, lower=False, capitalCheck=False)()

    def _extractWords(self, w):
        return extractWords(w, translate={},
                            useDict=True,
                            detectBadConsonantPairs=True,
                            removeLeetSpeak=True)

    def _contextMatch(self, window):
        return window and ERC.activeForWindow(window)

nickSelect = SelectNick()
        
@registerRule
class ERC(SeriesMappingRule):
    mapping = {
        "hiss"               : Key("a-p"),
        "piss"               : Key("a-n"),
        "join [<text>]"      : EmacsText("/join #%(text)s"),
        "smiley wink"        : EmacsText(";)"),
        "smiley tongue"      : EmacsText(":P", lower=False),
        "smiley wink tongue" : EmacsText(";P", lower=False),
        "part"               : EmacsText("/part"),
    }

    extras = [
        Integer("n", 2, 20),
        Dictation("text"),
        ]

    defaults = {
        "n"    : 1,
        "text" : "",
        }        

    @classmethod
    def activeForWindow(cls, window):
        isemacs = Emacs.activeForWindow(window)
        if not isemacs:
            return False
        return getMajorMode() == "erc-mode"
