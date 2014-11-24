from namedtuple import namedtuple

MicrophoneEvent = namedtuple("MicrophoneEvent", "state")
RuleMatchEvent = namedtuple("RuleMatchEvent", "rule, extras")

### DRAGONSHARE RSYNC
