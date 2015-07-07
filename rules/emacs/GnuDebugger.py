#!/usr/bin/env python
# -*- coding: utf-8 -*-

from Actions import Key, Repeat
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from requirements.VarRequirement import VarRequirement
#from requirements.Or import OrRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd, Minibuf
from rules.emacs.Keywords import KeywordRule

import keyword

_mapping = {
    "track debugger"         : Minibuf("realgud-track-mode"),
    "debug run"              : Key("c-c,d"),
    "toggle debugger"        : Key("c-c,t,d"),
    "toggle valgrind"        : Key("c-c,t,v"),
    "good source"            : Key("c-x,c-a,c-l"),
    "set break point"        : Key("c-x,c-a,c-b"),
    "delete break point"     : Key("c-x,c-a,c-d"),
    "good step [<i>]"        : Key("c-x,c-a,c-s") * Repeat(extra="i"),
    "good next [<i>]"        : Key("c-x,c-a,c-n") * Repeat(extra="i"),
    "good instruction [<i>]" : Key("c-x,c-a,c-i") * Repeat(extra="i"),
    "good eval"              : Key("c-x,c-a,c-p"),
    "good continue"          : Key("c-x,c-a,c-r"),
    "good up"                : Key("c-x,c-a,langle"),
    "good down"              : Key("c-x,c-a,rangle"),
    "good until"             : Key("c-x,c-a,c-u"),
    "good finish"            : Key("c-x,c-a,c-f"),
    "good jump"              : Key("c-x,c-a,c-j"),
}

# TODO: check gdb-running/gdb-stopped variable
GnuDebuggerRule = makeContextualRule("GnuDebugger", _mapping, emacsExtras, emacsDefaults)
GnuDebuggerRule.context.addRequirement(IsEmacs)
GnuDebuggerRule.context.addRequirement(ModeRequirement(modes=["c-mode", "c++-mode"]))

keywords = [
    "all",
    "backtrace",
    "break",
    "catch",
    "clear",
    "commands",
    "condition",
    "continue",
    "delete",
    "disable",
    "enable",
    "finish",
    "handle",
    "ignore",
    "info breakpoints",
    "info catch",
    "info proc ID",
    "info proc all",
    "info proc mappings",
    "info proc status",
    "info proc times",
    "info signals",
    "info threads",
    "info watchpoints",
    "next",
    "pass",
    "print",
    "run",
    "step",
    "stop",
    "thread apply all",
    "thread apply",
    "thread",
    "until",
    "watch",
    "where",
    ["awatch", "access watch"],
    ["nexti", "next instruction"],
    ["nopass", "no pass"],
    ["noprint", "no print"],
    ["nostop", "no stop"],
    ["rbreak", "regex break"],
    ["rwatch", "read watch"],
    ["stepi", "step instruction"],
    ["tbreak", "one-shot break"],
]

IsDebugWindow = VarRequirement(r'(string-match-p "\\*\\(gud\\|gdb\\).*" (buffer-name))', 0, useFocus=True, useTimer=False)
IsDebugTrackWindow = VarRequirement(r'realgud-track-mode', 't', useFocus=True, useTimer=False)
#GnuDebuggerKeywordRule = KeywordRule([OrRequirement([IsDebugWindow, IsDebugTrackWindow])], keywords, "GnuDebuggerKeyword")
GnuDebuggerKeywordRule = KeywordRule([IsDebugTrackWindow], keywords, "GnuDebuggerKeyword")

