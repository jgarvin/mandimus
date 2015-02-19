import mdlog
log = mdlog.getLogger(__name__)
from rules.emacs.Cmd import runEmacsCmd 
from rules.WordSelector import WordSelector
from rules.emacs.EmacsEventGenerator import EmacsEventGenerator
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import ProjectFileListEvent
from requirements.Emacs import IsEmacs
from Actions import Key

projectFileListGen = EmacsEventGenerator("ProjectFile", "md-projectile-files", ProjectFileListEvent)

_openProjetileFileEl = """
(find-file-existing (concat (file-name-as-directory (projectile-project-root)) \"%s\"))
"""

class ProjectFileNames(WordSelector):
    def __init__(self, name, cmdWord):
        WordSelector.__init__(self, name, cmdWord)
        self.rule.context.addRequirement(IsEmacs)
        getLoop().subscribeEvent(ProjectFileListEvent, self._onProjectFileList)

    def _onProjectFileList(self, ev):
        self._update(ev.choices)

    def _currentChoice(self):
        return runEmacsCmd("(when (md-current-path) (file-relative-name (md-current-path) (projectile-project-root))))").strip("\"")

    def _select(self, choice):
        runEmacsCmd(_openProjetileFileEl % choice)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)")                

_projectFileNameSelector = ProjectFileNames("ProjectFileNames", "file")
