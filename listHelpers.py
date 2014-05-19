from itertools import dropwhile, chain

def rindex(lst, item):
    try:
        return dropwhile(lambda x: lst[x] != item, reversed(xrange(len(lst)))).next()
    except StopIteration:
        raise ValueError, "rindex(lst, item): item not in list"

def flattenList(l):
    return list(chain.from_iterable(l))
    
