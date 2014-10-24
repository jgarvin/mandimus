from itertools import dropwhile, chain

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
        print leftWordBoundary, rightWordBoundary
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
    
