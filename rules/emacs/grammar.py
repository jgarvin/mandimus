from rules.MappingRule import MappingRule
from rules.Rule import registerRule
from wordUtils import buildSelectMapping, extractWords
from EventLoop import getLoop
from Window import getFocusedWindow
import re

def updateListGrammar(lst, leadingTerm, translate, action, clsname, filterFunction, useDict=True):
    bufs = lst
    spokenForms = {}
    for b in bufs:
        spokenForms[b] = [set(extractWords(b, translate=translate, useDict=useDict))] 
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
    registerRule(LocalMapping)
    w = getFocusedWindow()
    if w:
        getLoop().determineRules(w)

    return omapping

def getStringList(output):
    output = re.findall('"[^"]*"', output)
    output = [x.strip('"') for x in output]
    return output    
