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
    ["fn", "function"],
    ["println!", "print"],
    ["std", "stood"],
    "as",
    "break",
    "const",
    "continue",
    "crate",
    "dyn",
    "else",
    "enum",
    "extern",
    "false",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "Self",
    "self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while",
    ["i8", "signed 8"],
    ["i16", "signed 16"],
    ["i32", "signed 32"],
    ["i64", "signed 64"],
    ["isize", "signed size"],
    ["u8", "unsigned 8"],
    ["u16", "unsigned 16"],
    ["u32", "unsigned 32"],
    ["u64", "unsigned 64"],
    ["usize", "unsigned size"],
    ["f32", "float 32"],
    ["f64", "float 64"],
    "bool",
    "true",
    "false",
    "char"
]

RustKeywordRule = KeywordRule("rust-mode", keywords)
