import StringIO
import collections
from itertools import dropwhile

def rindex(lst, item):
    # taken from: http://stackoverflow.com/a/6892096/50385
    # by: 'senderle'
    def index_ne(x):
        return lst[x] != item
    try:
        return dropwhile(index_ne, reversed(xrange(len(lst)))).next()
    except StopIteration:
        raise ValueError("rindex(lst, item): item not in list")

def enum(**enums):
    return type('Enum', (), enums)

def isNumber(s):
    try:
        int(s)
    except ValueError:
        return False
    return True

def deepEmpty(x):
    if not isinstance(x, collections.Iterable):
        return True
    elif not x:
        return True
    elif isinstance(x, dict):
        return all([deepEmpty(i) for i in x.values()])
    elif type(x) == str or type(x) == unicode:
        # this has to be its own case because python
        # doesn't distinguish strings from characters
        return len(x) == 0
    else:
        return all([deepEmpty(i) for i in x])

class EqualityMixin(object):
    "Without this python equality is bonkers"

    def __eq__(self, other):
        """Override the default Equals behavior"""
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __ne__(self, other):
        """Define a non-equality test"""
        if isinstance(other, self.__class__):
            return not self.__eq__(other)
        return NotImplemented

    def __hash__(self):
        """Override the default hash behavior (that returns the id or the object)"""
        return hash(tuple(sorted(self.__dict__.items())))

### DRAGONSHARE RSYNC

