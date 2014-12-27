from namedtuple import namedtuple

RuleChangeEvent = namedtuple("RuleChangeEvent", "rule")
MicrophoneEvent = namedtuple("MicrophoneEvent", "state")
RuleMatchEvent = namedtuple("RuleMatchEvent", "rule, extras")
FocusChangeEvent = namedtuple("FocusChangeEvent", "window") 
WindowListEvent = namedtuple("WindowListEvent", "windows") 
WordEvent = namedtuple("WordEvent", "words")
EmacsConnectedEvent = namedtuple("EmacsConnectedEvent", [])

# choice events
BufferListEvent = namedtuple("BufferListEvent", "choices") 
ProjectListEvent = namedtuple("ProjectListEvent", "choices") 
ProjectFileListEvent = namedtuple("BufferListEvent", "choices") 
NickEvent = namedtuple("NickEvent", "choices")
EmacsWordEvent = namedtuple("EmacsWordEvent", "choices")

class DisconnectedEvent(object): pass
class ConnectedEvent(object): pass
class StartupCompleteEvent(object): pass

### DRAGONSHARE RSYNC
