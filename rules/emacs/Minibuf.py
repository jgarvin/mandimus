#!/usr/bin/env python
# -*- coding: utf-8 -*-

from Actions import Key, Repeat
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.VarRequirement import VarRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Text import EmacsText

_mapping = {
    "prior [<i>]"   : Key("a-p:%(i)d"),
    "future [<i>]"  : Key("a-n:%(i)d"),
    "surface [<i>]" : Key("c-l") * Repeat(extra="i"),
}

MinibufRule = makeContextualRule("Minibuf", _mapping, emacsExtras, emacsDefaults)
MinibufRule.context.addRequirement(IsEmacs)
MinibufRule.context.addRequirement(VarRequirement("(minibufferp)", "t"))
