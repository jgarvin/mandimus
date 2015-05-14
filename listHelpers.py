import mdlog
log = mdlog.getLogger(__name__)

from itertools import dropwhile, chain
import string

def dictReplace(s, dic):
    "Whole word replace keys with values in s"
    keys = dic.keys()

    # we want to replace the largest strngs first
    keys.sort(key=lambda x: len(x), reverse=True)

    for k in keys:
        i = s.find(k)
        if i == -1:
            continue

        # and we only want to match whole words
        leftWordBoundary = (i == 0 or s[i-1] == u' ')
        rightWordBoundary = (i + len(k) == len(s) or s[i+1] == u' ')
        if rightWordBoundary and leftWordBoundary:
            s = s.replace(k, dic[k])

    return s

def rindex(lst, item):
    try:
        return dropwhile(lambda x: lst[x] != item, reversed(xrange(len(lst)))).next()
    except StopIteration:
        raise ValueError, "rindex(lst, item): item not in list"

def flattenList(l):
    return list(chain.from_iterable(l))

def splitFlatten(slist, s=' '):
    x = []
    for i in slist:
        x.extend(i.split(s))
    return x

UPPER = 0
LOWER = 1
OTHER = 2

def category(c):
    if c in string.ascii_uppercase:
        return UPPER
    elif c in string.ascii_lowercase:
        return LOWER
    else:
        return OTHER

def deCamelize(word):
    cat = None
    wordList = []
    buildingWord = []

    for i, c in enumerate(word):
        newCategory = category(c)
        isLast = i == len(word)-1
        if (cat and cat != newCategory) or (newCategory == UPPER and cat == UPPER):
            wordList.append(''.join(buildingWord))
            buildingWord = []
        if isLast:
            buildingWord.append(c)
            wordList.append(''.join(buildingWord))
            buildingWord = []
        cat = newCategory
        buildingWord.append(c)
    return wordList

if __name__ == "__main__":
    assert(deCamelize("helloWorld") == ["hello", "World"])
    assert(deCamelize("hello2world") == ["hello", "2", "world"])
    assert(deCamelize("3hello@WorldThere") == ["3", "hello", "@", "World", "There"])
    print deCamelize("a7a")
    assert(deCamelize("a7a") == ["a", "7", "a"])
