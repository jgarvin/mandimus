class Requirement(object):
    def __init__(self, contexts=None):
        if contexts is None:
            self.contexts = set()
        else:
            self.contexts = contexts

    def addContext(self, ctx):
        self.contexts.add(ctx)

    def removeContext(self, ctx):
        self.contexts.remove(ctx)

    def _met(self, value=True):
        if value:
            for ctx in self.contexts:
                ctx.met(self)
        else:
            self._unmet()

    def _unmet(self):
        for ctx in self.contexts:
            ctx.unmet(self)
