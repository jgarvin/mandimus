#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Taken from: http://sgros-students.blogspot.com/2013/05/calling-eventfd-from-python.html
# By: Frane Kurtović

from ctypes import *
libc = cdll.LoadLibrary("libc.so.6") 
def eventfd(init_val, flags):
    return libc.eventfd(init_val, flags)
