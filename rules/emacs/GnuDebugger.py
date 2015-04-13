#!/usr/bin/env python
# -*- coding: utf-8 -*-

from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from requirements.VarRequirement import VarRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule

import keyword

# _mapping = {
#     "align dic"   : Cmd("(align-dict)"),
#     "align list"  : Cmd("(align-list)"),
#     "mark block"  : Cmd("(er/mark-python-block)"),
#     "mark state"  : Cmd("(er/mark-python-statement)"),
#     "send funk"   : Key("ca-x"),
#     "send buff"   : Key("c-c,c-c"),
#     "send region" : Key("c-c,c-r"),
#     "interpreter" : Key("c-c,c-z"),
# }

# GnuDebuggerRule = makeContextualRule("GnuDebugger", _mapping, emacsExtras, emacsDefaults)
# GnuDebuggerRule.context.addRequirement(IsEmacs)
# GnuDebuggerRule.context.addRequirement(ModeRequirement(modes=["python-mode", "inferior-python-mode"]))

keywords = [
    "all",
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

IsDebugWindow = VarRequirement(r'(string-match-p "\\*gdb.*" (buffer-name))', 0, useFocus=True, useTimer=False)
GnuDebuggerKeywordRule = KeywordRule([IsDebugWindow], keywords, "GnuDebuggerKeyword")
