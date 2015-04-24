from EventLoop import getLoop
from rules.emacs.Cmd import runEmacsCmd
from EventList import WordEvent
import sys

def onWordEvent(ev):
    runEmacsCmd(u"(mandimus-word-event \"%s\")" % ev.words)

getLoop().subscribeEvent(WordEvent, onWordEvent)
