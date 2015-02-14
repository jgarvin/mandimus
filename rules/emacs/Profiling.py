from rules.emacs.Cmd import Minibuf
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults

_mapping = {
    "profiler start"         : Minibuf("profiler-start"),
    "profiler stop"          : Minibuf("profiler-stop"),
    "profiler report"        : Minibuf("profiler-report"),
    "instrument function"    : Minibuf("elp-instrument-function"),          
    "instrument results"     : Minibuf("elp-results"),         
    "instrument restore"     : Minibuf("elp-restore-function"),          
    "instrument restore all" : Minibuf("elp-restore-all"),      
    "instrument master"      : Minibuf("elp-set-master"),         
    "instrument package"     : Minibuf("elp-instrument-package"),
    "macro expand"           : Minibuf("macroexpand-point"),
}

ProfilerRule = makeContextualRule("Profiler", _mapping, emacsExtras, emacsDefaults)
ProfilerRule.context.addRequirement(IsEmacs)
