#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mdlog
log = mdlog.getLogger(__name__)

from EventLoop import getLoop, pushEvent
from EventList import PedalsEvent, ExitEvent, RestartEvent

from piehid32 import *
from ctypes import *
from eventfd import eventfd
import sys
import atexit

def bitsToNum(bits):
    m = { 4 : 3, 8 : 4, 16 : 5, 32 : 6 }
    val = ord(bits)
    if val in m:
        return m[val]
    return val

def getIndexOfKey(data):
    if data[3] > 0:
        return bitsToNum(data[3])-1;
    if data[4] > 0:
        return bitsToNum(data[4])+7;
    if data[5] > 0:
        return bitsToNum(data[5])+15;
    if data[6] > 0:
        return bitsToNum(data[6])+23;
    return -1; 

def printBuf(data, length):
    for i in range(length):
        print "%02x" % ord(data[i]),
        if (i + 1) % 8 == 0:
            print " ",
            if (i + 1) % 16 == 0:
                print
                print
                print

def dataCb(data, deviceId, error):
    log.info("==============Got data==============")
    print data, deviceId, error
    return 0

def errorCb(deviceId, status):
    log.info("==============ERROR================")
    print deviceId, status
    return 0

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

atexit.register(exitHandler)
getLoop().subscribeEvent(ExitEvent, exitHandler)
getLoop().subscribeEvent(RestartEvent, exitHandler)

