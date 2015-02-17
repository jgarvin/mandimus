import mdlog
log = mdlog.getLogger(__name__)

class Requirement(object):
    def __init__(self, contexts=None):
        if contexts is None:
            self.contexts = set()
        else:
            self.contexts = contexts

        self._satisfied = False

    @property
    def satisfied(self):
        return self._satisfied

    def addContext(self, ctx):
        self.contexts.add(ctx)

    def removeContext(self, ctx):
        self.contexts.remove(ctx)

    def _met(self, value=True):
        self._satisfied = value
        if value:
            for ctx in self.contexts:
                ctx.met(self)
        else:
            self._unmet()

    def _unmet(self):
        self._satisfied = False
        for ctx in self.contexts:
            ctx.unmet(self)
