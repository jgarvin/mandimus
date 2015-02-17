from requirements.Requirement import Requirement

class Toggle(Requirement):
    def flip(self):
        if self.satisfied:
            self._unmet()
        else:
            self._met()

    def enable(self):
        self._met()

    def disable(self):
        self._unmet()
