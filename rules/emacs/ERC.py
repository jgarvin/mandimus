import mdlog
log = mdlog.getLogger(__name__)

from Actions import Key, Text, SelectChoice
from EventLoop import getLoop
from rules.Elements import Dictation, Integer
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd, getMajorMode
from rules.Rule import registerRule
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText
from wordUtils import extractWords
from Window import getFocusedWindow

class SelectNick(SelectChoice):
    def _currentChoice(self):
        return None

    def _select(self, choice):
        if runEmacsCmd("(md-at-start-of-erc-input-line)").strip() == 't':
            # we're addressing them, include the colon
            EmacsText("%s: " % choice, lower=False, capitalCheck=False)()
        else:
            # we're referring to them, omit the colon
            EmacsText("%s" % choice, lower=False, capitalCheck=False)()
            
    def _noChoice(self):
        pass

def nickList():
    nicks = runEmacsCmd("(md-get-active-erc-nicknames)").strip()
    if nicks == "nil":
        return []
    return getStringList(nicks)

def nickExtractFunction(w):
    return extractWords(w, translate={}, useDict=True, detectBadConsonantPairs=True)

_lastMapping = None

def updateNickGrammar():
    window = getFocusedWindow()
    if window and not ERC.activeForWindow(window):
        return

    nicks = set(nickList())
    mapping = updateListGrammar(nicks, 'nick',
                                SelectNick, "EmacsNickMapping",
                                ERC.activeForWindow,
                                nickExtractFunction, register=False)

    global _lastMapping
    if (mapping and not _lastMapping) or  (_lastMapping and mapping and _lastMapping.mapping != mapping.mapping):  
        print "registering"
        registerRule(mapping)
        getLoop().determineRules(window)
    _lastMapping = mapping
            
getLoop().subscribeTimer(1, updateNickGrammar)

@registerRule
class ERC(SeriesMappingRule):
    mapping = {
        "hiss"               : Key("a-p"),
        "piss"               : Key("a-n"),
        "join [<text>]"      : Text("/join #%(text)s"),
        "smiley wink"        : EmacsText(";)"),
        "smiley tongue"      : EmacsText(":P", lower=False),
        "smiley wink tongue" : EmacsText(";P", lower=False),
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
