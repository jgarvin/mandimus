from rules.MappingRule import MappingRule
from rules.Rule import registerRule
from wordUtils import buildSelectMapping, extractWords
from EventLoop import getLoop
from Window import getFocusedWindow
import re

def defaultExtract(w):
    return extractWords(w, translate={}, useDict=True)

# def updateListGrammar(lst, leadingTerm, translate, action, clsname, filterFunction,
#                       useDict=True):
def updateListGrammar(lst, leadingTerm, action, clsname, filterFunction,
                      extractFunction=defaultExtract):
    bufs = lst
    spokenForms = {}
    for b in bufs:
        spokenForms[b] = [set(extractFunction(b))] 
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
