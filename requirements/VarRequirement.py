#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mdlog
log = mdlog.getLogger(__name__)
from EventList import FocusChangeEvent, MajorModeEvent, EmacsConnectedEvent
from EventLoop import getLoop, pushEvent
from rules.emacs.Cmd import runEmacsCmd, Cmd
from requirements.Requirement import Requirement

class VarRequirement(Requirement):
    def __init__(self, var, value, contexts=None, useTimer=True, useFocus=False):
        self.var = var
        self.value = value
        Requirement.__init__(self, contexts)

        getLoop().subscribeEvent(EmacsConnectedEvent, self._query, priority=0)
        if useTimer:
            getLoop().subscribeTimer(1, self._query, priority=0)
        if useFocus:
            getLoop().subscribeEvent(FocusChangeEvent, self._query, priority=0)

    def _query(self, ev=None):
        output = runEmacsCmd(self.var)
        try:
            output = int(output)
        except ValueError:
            pass

        self._met(output == self.value)
