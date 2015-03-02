#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mdlog
log = mdlog.getLogger(__name__)
from rules.emacs.Cmd import runEmacsCmd 
from rules.WordSelector import WordSelector
from rules.emacs.EmacsEventGenerator import EmacsEventGenerator
from rules.ContextualRule import makeContextualRule
from rules.emacs.Cmd import Cmd
from wordUtils import extractWords
from EventLoop import getLoop
from EventList import BufferListEvent
from requirements.Emacs import IsEmacs
import string

_bufferQueryTable = [
    ("(md-get-buffers-in-modes 'dired-mode)", "folder"),
    ("(md-get-buffers-in-modes 'comint-mode)", "shell"),
    ("(md-get-buffers-in-modes 'erc-mode)", "channel"),
    ("(md-get-special-buffers)", "special"),
]

_appendCmd = "(append %s)" % " ".join([e[0] for e in _bufferQueryTable])
_allBuffQuery = "(md-all-buffers-except %s)" % _appendCmd

_bufferQueryTable.append((_allBuffQuery, "buff"))

class BufferEventGenerator(EmacsEventGenerator):
    def __init__(self, name, query):
        self.query = query
        cmd = "(md-get-buffer-names %s)" % self.query
        EmacsEventGenerator.__init__(self, name, cmd, BufferListEvent)

    def _makeEvent(self, newOutput):
        return self.eventType(self.query, newOutput)

class BufferNames(WordSelector):
    def __init__(self, name, cmdWord, query):
        WordSelector.__init__(self, name, cmdWord)
        self.rule.context.addRequirement(IsEmacs)
        self.query = query
        getLoop().subscribeEvent(BufferListEvent, self._onBufferList)

    def _onBufferList(self, ev):
        log.info("Ev: [%s]" % (ev,))
        if ev.query == self.query:
            log.info("Name: [%s] query: [%s] buffers: [%s]" % (self.name, ev.query, ev.choices))
            self._update(ev.choices)

    def _currentChoice(self):
        buf = runEmacsCmd("(buffer-name (current-buffer))")
        return buf.strip().strip('"')

    def _select(self, cmd, choice):
        runEmacsCmd("(switch-to-buffer \"%s\")" % choice, queryOnly=False)

    def _noChoice(self):
        runEmacsCmd("(switch-to-buffer nil)", queryOnly=False)

_generators = []
_selectors = []
for e in _bufferQueryTable:
    _generators.append(BufferEventGenerator(e[1], e[0]))
    _selectors.append(BufferNames(e[1] + "Names", e[1], e[0]))

# Because uniquify buffer code will start naming things $shell<2>, $shell<3>,
# but leaves the first shell as just $shell, we add this for voice command
# uniformity
_mapping = {
    "shell one" : Cmd("(switch-to-buffer \"$shell\")")
}
ShellOneRule = makeContextualRule("ShellOneRule", _mapping)
ShellOneRule.context.addRequirement(IsEmacs)
