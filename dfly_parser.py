import mdlog
log = mdlog.getLogger(__name__)

import sys

# terminator just picked to be something that doesn't come up
# in python code
MESSAGE_TERMINATOR = '###>>>'                
ARG_DELIMETER = '$$$%%%'
KEY_VALUE_SEPARATOR = ':.:'
MATCH_MSG_START = 'MATCH'

def parseMessages(buf):
    messages = []

    try:
        idx = 0
        while idx < len(buf):
            message_end = buf.find(MESSAGE_TERMINATOR, idx)

            if message_end != -1: # found message terminator
                message = buf[idx:message_end]
                messages.append(message)
                idx = message_end + len(MESSAGE_TERMINATOR)
            else:
                # searched all the way to the end of the buf without finding
                # a terminator
                break

        # get rid of parsed messages
        buf = buf[idx:]
        return (buf, messages)

    except Exception, e:
        log.info("Exception wile receiving message: ", e)
        log.info("Buf state: %s" % (buf,))
        sys.exit(1)

### DRAGONSHARE RSYNC
