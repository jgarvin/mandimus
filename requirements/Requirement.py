class Requirement(object):
    def addContext(self, ctx):
        self.contexts.add(ctx)

    def removeContext(self, ctx):
        self.contexts.remove(ctx)


