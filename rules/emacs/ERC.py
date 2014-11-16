from Actions import Key, Text, SelectChoice
from EventLoop import getLoop
from rules.Elements import Dictation, Integer
from rules.SeriesMappingRule import SeriesMappingRule
from rules.emacs.Emacs import Emacs
from rules.emacs.Cmd import runEmacsCmd, Cmd
from rules.Rule import registerRule
from rules.emacs.grammar import updateListGrammar, getStringList
from rules.emacs.Text import EmacsText

class SelectNick(SelectChoice):
    def _currentChoice(self):
        return None

    def _select(self, choice):
        if runEmacsCmd("(md-at-start-of-erc-input-line)").strip() == 't':
            # we're addressing them, include the colon
            EmacsText("%s: " % choice, capitalCheck=False)()
        else:
            # we're referring to them, omit the colon
            EmacsText("%s" % choice, capitalCheck=False)()
            
    def _noChoice(self):
        pass

def nickList():
    nicks = runEmacsCmd("(md-get-active-erc-nicknames)").strip()
    if nicks == "nil":
        return []
    return getStringList(nicks)

def updateNickGrammar():
    nicks = set(nickList())
    #print 'building with : ' + str(nicks)
    mapping = updateListGrammar(nicks, 'nick', {},
                                SelectNick, "EmacsNickMapping",
                                ERC.activeForWindow)
    # if mapping:
    #     print mapping.keys()

getLoop().subscribeTimer(1, updateNickGrammar)

@registerRule
class ERC(SeriesMappingRule):
    mapping = {
        "hiss" : Key("a-p"),
        "piss" : Key("a-n"),
        "join [<text>]" : Text("/join #%(text)s"),
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
        out = runEmacsCmd("major-mode").strip()
        return out == "erc-mode"
