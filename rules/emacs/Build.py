#!/usr/bin/env python
# -*- coding: utf-8 -*-

from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from rules.emacs.common import emacsExtras, emacsDefaults
from Actions import Key
from protocol import RuleType

_mapping = {
    "build menu"       : Key("c-u,c-c,b"),
    "run menu"         : Key("c-u,c-c,r"),
    "build only"       : Key("c-c,b"),
    "stale run"        : Key("c-c,r"),
    "execute"          : Key("c-c,c"),
    "new build script" : Key("c-c,n,b"),
    "new run script"   : Key("c-c,n,r"),
}

BuildRule = makeContextualRule("Build", _mapping, emacsExtras, emacsDefaults, ruleType=RuleType.TERMINAL)
BuildRule.context.addRequirement(IsEmacs)
