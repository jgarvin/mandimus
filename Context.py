class Context(object):
    """A Context represents a set of requirements that
    activates its targets when those requirements are met
    and deactivates them otherwise."""

    def __init__(self, targets):
        self.requirements = set()
        self.met = set()
        self.targets = targets
    
    def addRequirement(self, req):
        self.requirements.add(req)
        req.setContext(self)
        self._maybeFire()

    def met(req):
        assert req in self.requirements
        self.met.add(req)
        self._maybeFire()

    def unmet(req):
        assert req in self.requirements
        self.met.remove(req)
        self._maybeFire()

    def _maybeFire(self):
        if not (self.requirements - self.met):
            for t in self.targets:
                t.activate()
        else:
            for t in self.targets:
                t.deactivate()
