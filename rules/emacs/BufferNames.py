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
from Actions import Key
from protocol import RuleType

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
        # log.info("Ev: [%s]" % (ev,))
        if ev.query == self.query:
            log.info("Name: [%s] query: [%s] buffers: [%s]" % (self.name, ev.query, ev.choices))
            self._update(ev.choices)

    def _currentChoice(self):
        buf = runEmacsCmd("(buffer-name (current-buffer))")
        return buf.strip().strip('"')

    def _select(self, cmd, choice):
        runEmacsCmd("(switch-to-buffer \"%s\")" % choice, queryOnly=False)

    def _noChoice(self):
        runEmacsCmd("(md-switch-to-next-buffer-in-list %s)" % self.query, queryOnly=False)

class FolderNames(BufferNames):
    phrase = "folder"
    query = "(md-get-buffers-in-modes 'dired-mode)"

    def _noChoice(self):
        runEmacsCmd("(md-folder-switch)", queryOnly=False)

class ShellNames(BufferNames):
    phrase = "shell"
    query = "(md-get-buffers-in-modes 'shell-mode)"

    def _noChoice(self):
        Key("c-z")()

class EShellNames(BufferNames):
    phrase = "eshell" # wanted to make "E shell" or "e-shell "but then rules never load
    query = "(md-get-buffers-in-modes 'eshell-mode)"

    def _noChoice(self):
        (Key("c-c") + Key("z"))()

class ChannelNames(BufferNames):
    phrase = "channel"
    query = "(md-get-buffers-in-modes 'erc-mode)"

class SpecialNames(BufferNames):
    phrase = "special"
    query = "(md-get-special-buffers)"

_bufferQueryTable = [
    FolderNames,
    ShellNames,
    EShellNames,
    ChannelNames,
    SpecialNames,
]

_appendCmd = "(append %s)" % " ".join([e.query for e in _bufferQueryTable])
_allBuffQuery = "(md-all-buffers-except %s)" % _appendCmd

class GeneralBufferNames(BufferNames):
    phrase = "buff"
    query = "(md-all-buffers-except %s)" % _appendCmd

_bufferQueryTable.append(GeneralBufferNames)

_generators = []
_selectors = []
for e in _bufferQueryTable:
    _generators.append(BufferEventGenerator(e.phrase, e.query))
    _selectors.append(e(e.phrase + "Names", e.phrase, e.query))

# Because uniquify buffer code will start naming things $shell<2>, $shell<3>,
# but leaves the first shell as just $shell, we add this for voice command
# uniformity
_mapping = {
    "shell one" : Cmd("(switch-to-buffer \"*shell*\")")
}
ShellOneRule = makeContextualRule("ShellOneRule", _mapping)
ShellOneRule.context.addRequirement(IsEmacs)

# Because uniquify buffer code will start naming things $eshell<2>, $eshell<3>,
# but leaves the first eshell as just $eshell, we add this for voice command
# uniformity
_mapping = {
    "eshell one" : Cmd("(switch-to-buffer \"*eshell*\")")
}
EshellOneRule = makeContextualRule("EshellOneRule", _mapping)
EshellOneRule.context.addRequirement(IsEmacs)

_mapping = {}
for e in _bufferQueryTable:
    _mapping["last " + e.phrase] = Cmd("(md-switch-to-next-buffer-in-list %s)" % e.query)

LastBufferRule = makeContextualRule("LastBufferRule", _mapping, [], {}, ruleType=RuleType.TERMINAL)
LastBufferRule.context.addRequirement(IsEmacs)
