from listHelpers import splitFlatten, deCamelize
import re, string
from Actions import Noop
from util import deepEmpty

punc2Words = {
    "*" : ["star", "asterisk"],
    "#" : ["sharp"],
}

englishWords = set()
with open("/usr/share/dict/american-english") as f:
    for w in f.readlines():
        # strip punctuation because there's a bunch of weird
        # apostrophes and other things.
        word = "".join(c for c in w if c not in string.punctuation)
        word = word.lower()
        englishWords.update(set(re.findall("[a-z]+", word)))

# TODO: maybe give translate a better default
def extractWords(wordstr, splitters={' '} | set(string.punctuation), translate={}):
    """Split a string into a list using multiple delimeters, and optionally
    translating some characters to one or more words. Also lowercase everything."""
    splitters = splitters - set(translate.keys())
    all_words = []
    word = []
    strlen = len(wordstr)

    def finish(w):
        new_words = [i.lower() for i in deCamelize(''.join(w))]
        new_subwords = set()
        for word in new_words:
            for i in range(len(word)):
                for j in range(len(word)-1):
                    subword = word[i:j+2]
                    if subword in englishWords and len(subword) > 2:
                        new_subwords.add(subword)
        all_words.extend(new_words)
        all_words.extend(new_subwords)
                        
    for c in wordstr:
        if c in splitters:
            if word:
                finish(word)
                word = []
        elif c in translate:
            if word:
                finish(word)
                word = []
            all_words.extend(translate[c])
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

    if not omapping:
        return None
    return omapping

if __name__ == "__main__":
    print extractWords("wgreenhouse")
