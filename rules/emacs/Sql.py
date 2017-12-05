#!/usr/bin/env python
# -*- coding: utf-8 -*-

from Actions import Key
from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.common import emacsExtras, emacsDefaults
from rules.emacs.Cmd import Cmd
from rules.emacs.Keywords import KeywordRule

keywords = [
    ["SELECT", "select"],
    ["FROM", "from"],
    ["ORDER BY", "order by"],
    ["DESC", "descending"],
    ["ASC", "ascending"],
    ["GROUP BY", "group by"],
    ["LIMIT", "limit"],
    ["WHERE", "where"],
    ["JOIN", "join"],
    ["MERGE", "merge"],
    ["COUNT", "count"],
    ["INSERT", "insert"],
    ["INTO", "INTO"],
    ["DISTINCT", "distinct"],
]

SqlKeywordRule = KeywordRule(["sql-mode", "sql-interactive-mode"], keywords)

_mapping = {
    "history"           : Key("a-r"),
    "interrupt"         : Key("c-c,c-c"),
    "exit"              : Key("c-d"),
    "prompt up [<n>]"   : Key("c-c,c-p:%(n)d"),
    "prompt down [<n>]" : Key("c-c,c-n:%(n)d"),
}

SqlRule = makeContextualRule("Sql", _mapping, emacsExtras, emacsDefaults)
SqlRule.context.addRequirement(IsEmacs)
SqlRule.context.addRequirement(ModeRequirement(modes="sql-interactive-mode"))
