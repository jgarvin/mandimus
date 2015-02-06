class Context(object):
    """A Context represents a set of requirements that
    activates its targets when those requirements are met
    and deactivates them otherwise."""

    def __init__(self, targets):
        self.requirements = set()
        self.metReqs = set()
        self.targets = targets
    
    def addTarget(self, target):
        self.targets.add(target)
        self._maybeFire()

    def removeTarget(self, target):
        self.targets.remove(target)
        target.deactivate()

    def addRequirement(self, req):
        self.requirements.add(req)
        req.addContext(self)
        self._maybeFire()

    def removeRequirement(self, req):
        self.requirements.remove(req)
        req.removeContext(self)
        self._maybeFire()
        
    def met(self, req):
        assert req in self.requirements
        self.metReqs.add(req)
        self._maybeFire()

    def unmet(self, req):
        assert req in self.requirements
        self.metReqs.remove(req)
        self._maybeFire()

    def _maybeFire(self):
        if not (self.requirements - self.metReqs):
            for t in self.targets:
                t.activate()
        else:
            for t in self.targets:
                t.deactivate()
