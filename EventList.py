from namedtuple import namedtuple

RuleActivateEvent = namedtuple("RuleActivateEvent", "rule")
RuleRegisterEvent = namedtuple("RuleRegisterEvent", "rule")
RuleDeactivateEvent = namedtuple("RuleDeactivateEvent", "rule")
MicrophoneEvent = namedtuple("MicrophoneEvent", "state")
RuleMatchEvent = namedtuple("RuleMatchEvent", "hash phrase extras words")
FocusChangeEvent = namedtuple("FocusChangeEvent", "window") 
WindowListEvent = namedtuple("WindowListEvent", "windows") 
WordEvent = namedtuple("WordEvent", "words")
EmacsConnectedEvent = namedtuple("EmacsConnectedEvent", [])
LoadingRulesEvent = namedtuple("LoadingRulesEvent", "state")
MajorModeEvent = namedtuple("MajorModeEvent", "modeList")
WordListEvent = namedtuple("WordListEvent", "name words")

# choice events
BufferListEvent = namedtuple("BufferListEvent", "choices") 
ProjectListEvent = namedtuple("ProjectListEvent", "choices") 
ProjectFileListEvent = namedtuple("BufferListEvent", "choices") 
NickEvent = namedtuple("NickEvent", "choices")
EmacsWordEvent = namedtuple("EmacsWordEvent", "choices")
EmacsSymbolEvent = namedtuple("EmacsSymbolEvent", "choices")

class DisconnectedEvent(object): pass
class ConnectedEvent(object): pass
class StartupCompleteEvent(object): pass
class RestartEvent(object): pass
class ExitEvent(object): pass
class EventsDrainedEvent(object): pass

### DRAGONSHARE RSYNC
