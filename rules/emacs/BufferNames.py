import mdlog
log = mdlog.getLogger(__name__)
from rules.emacs.Cmd import runEmacsCmd 
from rules.WordSelector import WordSelector
from rules.emacs.EmacsEventGenerator import EmacsEventGenerator
from rules.ContextualRule import makeContextualRule
from rules.emacs.Cmd import Cmd
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import BufferListEvent
from requirements.Emacs import IsEmacs
import string

bufferListGen = EmacsEventGenerator("Buffer", "(mapcar 'buffer-name (buffer-list))", BufferListEvent)

class BufferNames(WordSelector):
    def __init__(self, name, cmdWord, filterFunc):
        WordSelector.__init__(self, name, cmdWord)
        self.rule.context.addRequirement(IsEmacs)
        self.filterFunc = filterFunc
        getLoop().subscribeEvent(BufferListEvent, self._onBufferList)

    def _onBufferList(self, ev):
        self._update(self.filterFunc(ev.choices))

    def _currentChoice(self):
        buf = runEmacsCmd("(buffer-name (current-buffer))")
        return buf.strip().strip('"')

    def _select(self, cmd, choice):
        runEmacsCmd("(switch-to-buffer \"%s\")" % choice, queryOnly=False)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)", queryOnly=False)

# TODO: In the future should probably just have buffers sent with derived-mode info
# so that we can filter more accurately based on that. Technically this punctuation
# can be used in real unix filenames...

def filterBuffs(choices):
    punctuation = string.punctuation.replace("_", "") + " "
    return [c for c in choices if not any(c.startswith(s) for s in punctuation)]

def filterChannels(choices):
    return [c for c in choices if c.startswith("#")]

def filterShells(choices):
    return [c for c in choices if c.startswith("$")]

def filterSpecial(choices):
    return [c for c in choices if c.startswith(("*", " *"))]

def filterFolder(choices):
    return [c for c in choices if c.startswith("!")]

# Because uniquify buffer code will start naming things $shell<2>, $shell<3>,
# but leaves the first shell as just $shell, we add this for voice command
# uniformity
_mapping = {
    "shell one" : Cmd("(switch-to-buffer \"$shell\")")
}
ShellOneRule = makeContextualRule("ShellOneRule", _mapping)
ShellOneRule.context.addRequirement(IsEmacs)

_bufferNameSelector = BufferNames("BufferNames", "buff", filterBuffs)
_channelNameSelector = BufferNames("ChannelNames", "channel", filterChannels)
_shellNameSelector = BufferNames("ShellNames", "shell", filterShells)
_specialNameSelector = BufferNames("SpecialNames", "special", filterSpecial)
_folderNameSelector = BufferNames("FolderNames", "folder", filterFolder)
