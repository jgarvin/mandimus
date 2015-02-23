import mdlog
log = mdlog.getLogger(__name__)
from rules.emacs.Cmd import runEmacsCmd 
from rules.WordSelector import WordSelector, PhraseType
from rules.emacs.EmacsEventGenerator import EmacsEventGenerator
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import EmacsWordEvent, EmacsSymbolEvent
from requirements.Emacs import IsEmacs
from Actions import Key
import string
from rules.emacs.Text import EmacsText
from protocol import RuleType
from copy import copy

class EmacsWordGen(EmacsEventGenerator):
    def _filter(self, x):
        x = ''.join(c for c in x if c not in string.punctuation + string.digits)
        if len(x) <= 2:
            return False
        return True

    def _postProcess(self, output):
        lst = EmacsEventGenerator._postProcess(self, output)
        # filter unicode
        lst = [''.join([c for c in n if c in string.printable]) for n in lst]
        lst = [x for x in lst if self._filter(x)]
        return lst
    
emacsWordGen = EmacsWordGen("EmacsWord", "md-global-word-cache", EmacsWordEvent)
emacsSymbolGen = EmacsWordGen("EmacsSymbol", "md-global-symbol-cache", EmacsSymbolEvent)

class EmacsWordNames(WordSelector):
    def __init__(self, name, cmdWord, eventType, phraseType):
        WordSelector.__init__(self, name, cmdWord, allowNoChoice=False,
                              phraseType=phraseType,
                              ruleType=RuleType.SERIES)
        self.rule.context.addRequirement(IsEmacs)
        getLoop().subscribeEvent(eventType, self._onEmacsWord)

    def _onEmacsWord(self, ev):
        self._update(self._filter(ev.choices))
        # self._update(ev.choices)

    def _select(self, choice):
        EmacsText("%s" % choice, lower=False)()        

    # TODO: this filtering should really be done on the emacs side
    def _filter(self, words):
        newWords = copy(words)
        for w in words:
            if len(w) < 3:
                newWords.remove(w)
            
            try:
                int(w)
                newWords.remove(w)
            except ValueError:
                pass

            try:
                int(w, 16)
                newWords.remove(w)
            except ValueError:
                pass

            #log.info("Not filtering: [%s]" % w)
        return newWords


_emacsWordNameSelector = EmacsWordNames("EmacsWordNames", "word", EmacsWordEvent, phraseType=PhraseType.SINGLE_WORD)
_emacsSymbolNameSelector = EmacsWordNames("EmacsSymbolNames", "toke", EmacsSymbolEvent, phraseType=PhraseType.BOTH)


