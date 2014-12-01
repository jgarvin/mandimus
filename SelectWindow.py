import SelectChoice

class SelectWindow(SelectChoice.SelectChoice):
    def _tieSorter(self):
        return lambda x: x[0].winId

    def _currentChoice(self):
        return getFocusedWindow()

    def _select(self, choice):
        cmd = "xdotool windowactivate %d" % (choice.winId)
        runCmd(cmd)
