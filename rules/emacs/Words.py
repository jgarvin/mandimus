import mdlog
log = mdlog.getLogger(__name__)
from rules.emacs.Cmd import runEmacsCmd 
from rules.WordSelector import WordSelector
from rules.emacs.EmacsEventGenerator import EmacsEventGenerator
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import EmacsWordEvent
from requirements.Emacs import IsEmacs
from Actions import Key
import string
from rules.emacs.Text import EmacsText

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
    
emacsWordGen = EmacsWordGen("EmacsWord", "md-symbols-cache", EmacsWordEvent)

class EmacsWordNames(WordSelector):
    def __init__(self, name, cmdWord):
        WordSelector.__init__(self, name, cmdWord)
        self.rule.context.addRequirement(IsEmacs)
        getLoop().subscribeEvent(EmacsWordEvent, self._onEmacsWord)

    def _onEmacsWord(self, ev):
        self._update(ev.choices)

    def _select(self, choice):
        EmacsText("%s" % choice, lower=False)()        

_emacsWordNameSelector = EmacsWordNames("EmacsWordNames", "toke")

