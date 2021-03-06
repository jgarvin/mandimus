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
    ["char16_t", "char sixteen"],
    ["char32_t", "char thirty two"],
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
    ["#endif", "end if"],
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    ["__FILE__", "file"],
    "float",
    "for",
    "friend",
    ["__func__", "function"],
    "goto",
    "if",
    "inline",
    ["int", "integer"],
    "long",
    "main",
    "mutable",
    "namespace",
    "new",
    ["noexcept", "no except"],

    # redundant entries because it has a really hard time recognizing this
    ["nullptr", "knoll pointer"],
    ["nullptr", "knoll puter"],
    ["nullptr", "null pointer"],
    ["nullptr", "null puter"],

    "operator",
    "private",
    "protected",
    "public",
    "register",
    ["reinterpret_cast", "reinterpret cast"],
    "return",
    "short",
    "signed",
    ["std::size_t", "stood size"],
    ["sizeof", "size of"],
    "static",
    ["static_assert", "static assert"],
    ["static_cast", "static cast"],
    "struct",
    "switch",
    "template",
    "this",
    ["thread_local", "thread local"],
    "throw",
    "true",
    "try",
    "typedef",
    ["typeid", "type ID"],
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
    ["int32_t", "signed thirty two"],
    ["int64_t", "signed sixty four"],
    "main",
    "override",
    ["std", "stood"],
    ["std::string", "string"],
    ["uint8_t", "unsigned 8"],
    ["uint16_t", "unsigned 16"],
    ["uint32_t", "unsigned 32"],
    ["uint64_t", "unsigned 64"],
    ["__PRETTY_FUNCTION__", "pretty"],
    ["__LINE__", "line"],
    ["cout", "see out"],
    ["cerr", "see air"],
    "iterator",
    "const_iterator",
    ["std::size_t", "size T"],
    ["#else", "pound else"],
    "NULL",

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

# not sure why but having the modes be a list without naming the rule
# causes Dragon to stop being able to recognize anything as soon as I enter a C/C++ buffer!
CppKeywordRule = KeywordRule(["c++-mode", "c-mode"], keywords + types, "c_common")
