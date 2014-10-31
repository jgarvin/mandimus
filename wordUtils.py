from listHelpers import splitFlatten, deCamelize
import re, string

def extractWords(wordstr):
    words = [wordstr.strip()]
    words = [s.strip() for s in splitFlatten(words, ' ')]
    words = [s.strip() for s in splitFlatten(words, '-')]
    words = [s.strip() for s in splitFlatten(words, '_')]
    
    newWords = []
    for w in words:
        newWords.extend(filter(None, re.split("[" + string.punctuation + "]+", w)))
    words = newWords

    newWords = []
    for w in words:
        newWords.extend(deCamelize(w))
    words = newWords
    
    return [w.lower() for w in words]

def buildSelectMapping(leadingTerm, spokenSelects, selectAction):
    """
    Builds a mapping that can be used by a MappingRule.
    
    spokenSelects is a dictionary that maps choices to a list
    of sets of words that correspond to that choice.

    selectAction is the desired handler.
    """
    omapping = {}
    word2Selects = {}
    for w, spokenForms in spokenSelects.items():
        if not spokenForms:
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
        omapping[grammar] = selectAction(word2Selects)

    return omapping