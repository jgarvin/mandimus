import sys

def test():
    print 'hiccup'

def parseMessages(buf):
    messages = []

    try:
        # terminator just picked to be something that doesn't come up
        # in python code
        MESSAGE_TERMINATOR = '###>>>'            

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
        print "Exception wile receiving message: ", e
        print "Buf state: %s" % (buf,)
        sys.exit(1)

# Local Variables:
# eval: (add-hook 'after-save-hook (lambda () (shell-command (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem" (buffer-file-name) (getenv "HOME") ))) nil t)
# End:
