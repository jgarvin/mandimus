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


