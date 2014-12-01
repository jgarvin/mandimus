import mdlog
log = mdlog.getLogger(__name__)

from rules.emacs.Cmd import Cmd
from Actions import splitKeyString

replacements = {
    "underscore" : "_",
    "comma"      : ",",
    "enter"      : "<return>",
    "percent"    : "%",
    "langle"     : "<",
    "rangle"     : ">",
    "lparen"     : "(",
    "rparen"     : ")",
    "lbrace"     : "{",
    "rbrace"     : "}",
    "lbracket"   : "[",
    "rbracket"   : "]",
    "dquote"     : "\\\"",
    "squote"     : "'",
    "space"      : "SPC",
    "up"         : "<up>",
    "down"       : "<down>",
    "left"       : "<left>",
    "right"      : "<right>",
    "F0"         : "<f0>",
    "F1"         : "<f1>",
    "F2"         : "<f2>",
    "F3"         : "<f3>",
    "F4"         : "<f4>",
    "F5"         : "<f5>",
    "F6"         : "<f6>",
    "F7"         : "<f7>",
    "F8"         : "<f8>",
    "F9"         : "<f9>",
    "F10"        : "<f10>",
    "F11"        : "<f11>",
    "F12"        : "<f12>",
    "F13"        : "<f13>",
    "F14"        : "<f14>",
    "F15"        : "<f15>",
}

def dfly2emacsKey(keystr):
    keys = keystr.split('-')
    keys = [k.strip() for k in keys]
    modifiers = [] 
    if len(keys) > 1:
        modifiers = [k.upper() for k in keys[0]]
        modifiers = ['M' if k == 'A' else k for k in modifiers]
        del keys[0]
    keys = [replacements[k] if k in replacements else k for k in keys]
    modifiers.extend(keys)
    return '-'.join(modifiers)

tempCommand = """
(defun md-temp-command ()
  (interactive)
  %s)
(md-temp-command)
"""    

def makeKbdMacro(dflyKeyStr):
    keys = []
    cmd = "(execute-kbd-macro (kbd \"%s\"))"
    keys = [dfly2emacsKey(k) for k in splitKeyString(dflyKeyStr)]
    cmd %= ' '.join(keys)
    cmd = tempCommand % cmd
    log.info(cmd)
    return cmd

class Key(Cmd):
    pass
    
    

