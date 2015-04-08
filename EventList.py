from namedtuple import namedtuple

FocusChangeEvent = namedtuple("FocusChangeEvent", "window") 
LoadingRulesEvent = namedtuple("LoadingRulesEvent", "state")
MicrophoneEvent = namedtuple("MicrophoneEvent", "state")
RuleActivateEvent = namedtuple("RuleActivateEvent", "rule")
RuleDeactivateEvent = namedtuple("RuleDeactivateEvent", "rule")
RuleMatchEvent = namedtuple("RuleMatchEvent", "hash phrase extras words")
RuleRegisterEvent = namedtuple("RuleRegisterEvent", "rule")
WindowListEvent = namedtuple("WindowListEvent", "windows") 
WordEvent = namedtuple("WordEvent", "words")
WordListEvent = namedtuple("WordListEvent", "name words")
RecognitionStateEvent = namedtuple("RecognitionStateEvent", "state") 
PedalsEvent = namedtuple("PedalsEvent", "pedalStates")
ExitEvent = namedtuple("ExitEvent", [])

# emacs events
BufferListEvent = namedtuple("BufferListEvent", "query choices") 
EmacsConnectedEvent = namedtuple("EmacsConnectedEvent", [])
EmacsSymbolEvent = namedtuple("EmacsSymbolEvent", "choices")
EmacsWordEvent = namedtuple("EmacsWordEvent", "choices")
MajorModeEvent = namedtuple("MajorModeEvent", "modeList")
NickEvent = namedtuple("NickEvent", "choices")
ProjectFileListEvent = namedtuple("BufferListEvent", "choices") 
ProjectListEvent = namedtuple("ProjectListEvent", "choices") 

class ConnectedEvent(object): pass
class DisconnectedEvent(object): pass
class EventsDrainedEvent(object): pass
class RestartEvent(object): pass
class StartupCompleteEvent(object): pass

### DRAGONSHARE RSYNC
