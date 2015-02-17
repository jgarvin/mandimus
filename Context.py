import mdlog
log = mdlog.getLogger(__name__)

class Context(object):
    """A Context represents a set of requirements that
    activates its targets when those requirements are met
    and deactivates them otherwise."""

    def __init__(self, targets):
        self.requirements = set()
        self.targets = targets
        self.lastState = False # unmet

    def __str__(self):
        return ", ".join([str((type(r), r.satisfied)) for r in self.requirements])

    @property
    def satisfied(self):
        return self.lastState

    def addTarget(self, target):
        self.targets.add(target)
        # bypass checking lastState
        if self.satisfied:
            target.activate()
        else:
            target.deactivate()

    def removeTarget(self, target):
        self.targets.remove(target)
        if self.lastState == True:
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
        self._maybeFire()

    def unmet(self, req):
        assert req in self.requirements
        self._maybeFire()

    def _maybeFire(self, targetSet=None):
        if all([r.satisfied for r in self.requirements]):
            if self.lastState == False:
                for t in self.targets:
                    t.activate()
            self.lastState = True
        else:
            if self.lastState == True:
                for t in self.targets:
                    t.deactivate()
            self.lastState = False
