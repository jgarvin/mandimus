from namedtuple import namedtuple

MicrophoneEvent = namedtuple("MicrophoneEvent", "state")
RuleMatchEvent = namedtuple("RuleMatchEvent", "rule, extras")

class DisconnectedEvent(object): pass
class ConnectedEvent(object): pass
class StartupCompleteEvent(object): pass

### DRAGONSHARE RSYNC
