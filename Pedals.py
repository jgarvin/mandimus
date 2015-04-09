#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mdlog
log = mdlog.getLogger(__name__)

from EventLoop import getLoop, pushEvent
from EventList import PedalsEvent, ExitEvent, RestartEvent

from piehid32 import *
from ctypes import *
from eventfd import eventfd
import struct
import sys, os
import atexit

wakeupFd = eventfd(0, 0)

oldPedals = [0] * 3

def dataCb(data, deviceId, error):
    global oldPedals
    pedals = [0] * 3
    pedals[0] = (data[3] & 2) != 0
    pedals[1] = (data[3] & 4) != 0
    pedals[2] = (data[3] & 8) != 0
    for i in range(3):
        if pedals[i] != oldPedals[i]:
            changed = i
    oldPedals = pedals
    log.info("Pedals: [%s]" % pedals)
    pushEvent(PedalsEvent(pedals, changed))
    global wakeupFd
    # We use eventfd to wakeup the main thread so it will
    # see the pedal event
    written = os.write(wakeupFd, c_longlong(1))
    if written != 8:
        log.error("Error writing to eventfd.")
    return 0

def errorCb(deviceId, status):
    log.error("Error in pedals: [%s] [%s]" % (deviceId, status))
    return 0

def readOut():
    num = os.read(wakeupFd, 8)
    log.debug("Pedal EventFD Read: %d" % struct.unpack('@Q', num)[0])

getLoop().subscribeFile(wakeupFd, getLoop().FILE_INPUT, readOut)

_dataCb = PHIDDataEvent(dataCb)
_errorCb = PHIDErrorEvent(errorCb)

infoArrayType = TEnumHIDInfo * MAX_XKEY_DEVICES
TEnumHIDInfoPtr = POINTER(TEnumHIDInfo)

info = infoArrayType()
count = c_long(0)

result = EnumeratePIE(PI_VID, info, pointer(count))

dev = None
for i in range(count.value):
    dev = pointer(info[i])
    log.info("Found XKeys Device:")
    log.info("\tPID: %04x" % dev.contents.PID)
    log.info("\tUsage Page: %04x" % dev.contents.UP)
    log.info("\tUsage: %04x" % dev.contents.Usage)
    log.info("\tVersion: %04x" % dev.contents.Version)

    log.info("\tSetting up interface.")
    result = SetupInterfaceEx(dev.contents.Handle)
    if result != 0:
        log.info("Unable to open device. Error: %d" % result)

    # Why break after one iteration? No idea, but if I don't
    # do this then my keyboard stops working until I unplug it
    # and plug it back in!
    break

if dev.contents.Handle < 0:
    log.error("Unable to open device.")
    sys.exit(1)

dataArrayType = c_char * 33
data = dataArrayType()
idx = c_int(0)

result = SetDataCallback(dev.contents.Handle, _dataCb)
if result != 0:
    log.error("Unable to set data callback, Error: %d" % result)
    result = SetErrorCallback(dev.contents.Handle, _errorCb)
    if result != 0:
        log.error("Unable to set error callback, Error: %d" % result)

def exitHandler():
    global dev
    if dev:
        SetDataCallback(dev.contents.Handle, cast(None, PHIDDataEvent))
        SetErrorCallback(dev.contents.Handle, cast(None, PHIDErrorEvent))
        CleanupInterface(dev.contents.Handle)
        ClearBuffer(dev.contents.Handle)
        dev = None

#atexit.register(exitHandler)
getLoop().subscribeEvent(ExitEvent, exitHandler)
getLoop().subscribeEvent(RestartEvent, exitHandler)

# def pedalsTest(ev):
#     log.info("got pedals event")

# getLoop().subscribeEvent(PedalsEvent, pedalsTest)
