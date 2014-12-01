import mdlog
log = mdlog.getLogger(__name__)

from rules.MappingRule import MappingRule
from rules.Rule import registerRule
from wordUtils import buildSelectMapping, extractWords
from EventLoop import getLoop
from Window import getFocusedWindow
import re

def defaultExtract(w):
    return extractWords(w, translate={})

def updateListGrammar(lst, leadingTerm, action, clsname, filterFunction,
                      extractFunction=defaultExtract, register=True, dolog=False):
    bufs = lst
    spokenForms = {}
    for b in bufs:
        spokenForms[b] = [extractFunction(b)] 
    omapping = buildSelectMapping(leadingTerm, spokenForms, action)
    
    if omapping is None:
        # TODO: make exception
        return
    
    class LocalMapping(MappingRule):
        mapping = omapping
        
        @classmethod
        def activeForWindow(cls, window):
            return filterFunction(window)
    LocalMapping.__name__ = clsname
    if register:
        registerRule(LocalMapping)
        w = getFocusedWindow()
        if w:
            getLoop().determineRules(w)

    if dolog:
        log.info(str(LocalMapping()))

    return LocalMapping

def getStringList(output):
    output = output.strip()
    if output == "nil":
        return []
    output = re.findall('"[^"]*"', output)
    output = [x.strip('"') for x in output]
    return output    
