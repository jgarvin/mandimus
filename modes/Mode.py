class Mode(object):
    def __init__(self):
        pass
        
    @property
    def commands(self):
        pass

    @property
    def wordSet(self):
        s = set()
        for key in self.commands.keys():
            assert key == key.strip() # no leading/trailing whitespace allowed
            s.update(key.split())
        return s
