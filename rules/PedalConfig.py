#!/usr/bin/env python
# -*- coding: utf-8 -*-

from EventList import PedalsEvent
from EventLoop import getLoop
from Actions import runCmd

def onPedal(ev):
    updown = ["keyup", "keydown"]
    pedalKeys = ["control", "shift", "alt"]
    runCmd("xdotool %s %s" % (updown[ev.pedalStates[ev.changed]], pedalKeys[ev.changed]))

getLoop().subscribeEvent(PedalsEvent, onPedal)
