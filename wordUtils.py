from listHelpers import splitFlatten, deCamelize
import re, string
from Actions import Noop
from util import deepEmpty

punc2Words = {
    "*" : ["star", "asterisk"],
}

def extractWords(wordstr, splitters={' '} | set(string.punctuation), translate=set()):
    """Split a string into a list using multiple delimeters, and optionally
    translating some characters to one or more words. Also lowercase everything."""
    splitters = splitters - translate
    all_words = []
    word = []
    strlen = len(wordstr)

    def finish(w):
        all_words.extend([i.lower() for i in deCamelize(''.join(w))])        
    
    for c in wordstr:
        if c in splitters:
            if word:
                finish(word)
                word = []
        elif c in translate:
            if word:
                finish(word)
                word = []
            all_words.extend(punc2Words[c])
        else:
            word.append(c)
    if word:
        finish(word)
    return all_words

def buildSelectMapping(leadingTerm, spokenSelects, selectAction):
    """
    Builds a mapping that can be used by a MappingRule.

    leadingTerm is the prefix for each command that will
    be generated.
    
    spokenSelects is a dictionary that maps choices to a list
    of sets of words that correspond to that choice.

    selectAction is the desired handler.
    """
    omapping = {}
    word2Selects = {}
    for w, spokenForms in spokenSelects.items():
        if deepEmpty(spokenForms):
            continue

        grammar = [leadingTerm]
        first = True
        grammar += ["("]
        for form in spokenForms:
            if not first:
                grammar += ["|"]
            for word in form:
                if word not in word2Selects:
                    word2Selects[word] = set()
                word2Selects[word].add(w)
                grammar.append("[%s]" % word)
            first = False
        grammar += [")"]
        grammar = ' '.join(grammar)
        omapping[grammar] = selectAction(word2Selects, leadingTerm)

    return omapping
