import mdlog
log = mdlog.getLogger(__name__)
from rules.emacs.Cmd import runEmacsCmd 
from rules.WordSelector import WordSelector
from rules.emacs.EmacsEventGenerator import EmacsEventGenerator
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import ProjectListEvent
from requirements.Emacs import IsEmacs
from Actions import Key

projectListGen = EmacsEventGenerator("Project", "(projectile-relevant-known-projects)", ProjectListEvent)

class ProjectNames(WordSelector):
    def __init__(self, name, cmdWord):
        WordSelector.__init__(self, name, cmdWord)
        self.rule.context.addRequirement(IsEmacs)
        getLoop().subscribeEvent(ProjectListEvent, self._onProjectList)

    def _onProjectList(self, ev):
        self._update(ev.choices)

    def _currentChoice(self):
        return runEmacsCmd("(projectile-project-name)").strip("\"")

    def _select(self, choice):
        runEmacsCmd("(projectile-switch-project-by-name \"%s\")" % choice)

    def _noChoice(self):
        Key("c-c,p,p,enter")()

_projectNameSelector = ProjectNames("ProjectNames", "project")
