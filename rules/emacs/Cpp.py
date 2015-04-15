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
    # true keywords
    ["alignas", "align as"],
    ["alignof", "align of"],
    "assert",
    ["asm", "assembly"],
    "auto",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    ["char16_t", "char 16"],
    ["char32_t", "char 32"],
    "class",
    "const",
    ["const_cast", "const cast"],
    ["constexpr", "const expr"],
    "continue",
    ["decltype", "decl type"],
    "default",
    "delete",
    "do",
    "double",
    ["dynamic_cast", "dynamic cast"],
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    ["noexcept", "no except"],
    "nullptr",
    "operator",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "return",
    "short",
    "signed",
    ["sizeof", "size of"],
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    ["typeid", "type I D"],
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    ["wchar_t", "wide char"],
    "while",

    # common enough
    ["endl", "end line"],
    "final",
    ["#include", "include"],
    ["int32_t", "int 32"],
    ["int64_t", "int 64"],
    "main"
    "override",
    ["std", "stood"],
    ["uint32_t", "you int 32"],
    ["uint64_t", "you int 64"],

    # boost
    ["bxxst", "boost"],
    "optional",
]

types = [
    "deque"
    "map",
    "multimap",
    "pair",
    "set",
    ["unique_ptr", "unique pointer"],
    ["unordered_map", "unordered map"],
    "vector",
]

CppKeywordRule = KeywordRule("c++-mode", keywords + types)
