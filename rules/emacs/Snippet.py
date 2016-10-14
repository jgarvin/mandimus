#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mdlog
log = mdlog.getLogger(__name__)
from rules.emacs.Cmd import CharCmd, Cmd, runEmacsCmd
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults
from EventList import WordListEvent, MajorModeEvent
from EventLoop import getLoop, pushEvent
from protocol import ListRef
import grammar

_mapping = {
    "warp <charrule>"   : CharCmd('(md-sn-find-slot %s)'),
    "blank"             : Cmd('(md-sn-next-slot)'),
    "make blank"        : Cmd('(md-sn-drop-slot)'),
    "funk call [<i>]"   : Cmd("(md-insert-call-snippet %(i)d)"),
    "make <snippetList>" : Cmd("(md-insert-snippet \"%(snippetList)s\")"),
}

_extras = [
    ListRef("SnippetList", "snippetList", [])
]

SnippetRule = makeContextualRule("Snippet", _mapping, emacsExtras + _extras, emacsDefaults)
SnippetRule.context.addRequirement(IsEmacs)

def _onModeChange(self):
    snippetList = grammar.getStringList(runEmacsCmd("(md-get-snippet-names)"))
    log.info("Snippet names: [%s]" % snippetList)
    pushEvent(WordListEvent("SnippetList", snippetList))

getLoop().subscribeEvent(MajorModeEvent, _onModeChange)
