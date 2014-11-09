import collections

def deepEmpty(x):
    if not isinstance(x, collections.Iterable):
        return True
    elif not x:
        return True
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
