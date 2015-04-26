#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mdlog
log = mdlog.getLogger(__name__)

from protocol import RuleType
from ContextualRule import makeContextualRule
from EventList import PedalsEvent
from EventLoop import getLoop
from Actions import runCmd
from Actions import Key
from Pedals import setPedalCallback, getPedalCallback


# def onPedalTypingDay(ev):
#     updown = ["keyup", "keydown"]
#     pedalKeys = ["control", "shift", "alt"]
#     runCmd("xdotool %s %s" % (updown[ev.pedalStates[ev.changed]], pedalKeys[ev.changed]))

# def onPedalSpeakingDay(ev):
#     updown = ["keyup", "keydown"]
#     pedalKeys = ["Up", "shift", "Down"]
#     runCmd("xdotool %s %s" % (updown[ev.pedalStates[ev.changed]], pedalKeys[ev.changed]))

#getLoop().subscribeEvent(PedalsEvent, onPedalTypingDay)
#getLoop().subscribeEvent(PedalsEvent, onPedalSpeakingDay)

def toggleMic():
    runCmd("amixer -c $(arecord -l | grep Icicle | cut -d' ' -f2 | sed 's/://') sset Mic toggle")

def pedalArrowCb2(pedalStates, changed):
    updown = ["keyup", "keydown"]
    pedalKeys = ["Up", toggleMic, "Down"]
    global _lastPedal
    if not pedalStates[1] and changed == 1 and _lastPedal == 1:
        pedalKeys[changed]()
        _lastPedal = changed
        return
    runCmd("xdotool %s %s" % (updown[pedalStates[changed]], pedalKeys[changed]))
    _lastPedal = changed    

def pedalModifierCb(pedalStates, changed):
    updown = ["keyup", "keydown"]
    pedalKeys = ["control", "shift", "alt"]
    runCmd("xdotool %s %s" % (updown[pedalStates[changed]], pedalKeys[changed]))

def pedalArrowCb(pedalStates, changed):
    updown = ["keyup", "keydown"]
    pedalKeys = ["Up", cyclePedals, "Down"]
    global _lastPedal
    if not pedalStates[1] and changed == 1 and _lastPedal == 1:
        pedalKeys[changed]()
        _lastPedal = changed
        return
    runCmd("xdotool %s %s" % (updown[pedalStates[changed]], pedalKeys[changed]))
    _lastPedal = changed

_lastPedal = None

# TODO: keydown event!

def pedalWindowCb(pedalStates, changed):
    log.info("cb: [%s] [%s]" % (pedalStates, changed))
    pedalKeys = [Key("ca-h"), cyclePedals, Key("ca-e")]
    if pedalStates[0] and not pedalStates[1] and changed == 1:
        Key("ca-backspace")()
    elif pedalStates[0] and not pedalStates[2] and changed == 2:
        Key("ca-space")()
    elif pedalStates[1] and not pedalStates[2] and changed == 2:
        Key("csa-rbracket")()
    elif not pedalStates[changed] and _lastPedal == changed:
        pedalKeys[changed]()
    global _lastPedal
    _lastPedal = changed

# default assume speaking day
#setPedalCallback(pedalWindowCb)
setPedalCallback(pedalArrowCb2)

def cyclePedals(extras={}):
    cb = getPedalCallback()
#    cbs = [pedalArrowCb, pedalWindowCb]
    # cbs = [pedalArrowCb2, pedalWindowCb]
    newCb = cbs[(cbs.index(cb) + 1) % len(cbs)]
    log.info("Setting pedal mode: [%s]" % newCb)
    setPedalCallback(newCb)

def setModifierPedals(extras={}):
    log.info("Setting pedals to modifier mode.")
    setPedalCallback(pedalModifierCb)

_mapping = {
    "pedals"                               : cyclePedals,
    "modifierPedals" : setModifierPedals,
}
PedalRule = makeContextualRule("PedalRule", _mapping, ruleType=RuleType.INDEPENDENT)
PedalRule.activate()
