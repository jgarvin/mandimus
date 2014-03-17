from Actions import keys, click, moveRelativeToWindow
from Number import NumberMode
from Window import getWindowFocus, getWindowDimensions

class RatMode(NumberMode):
    def __init__(self):
        NumberMode.__init__(self)
        self.lastDigit = 4
        self.zoomLevel = 1

    @property
    def commands(self):
        c = {}
        c.update(NumberMode.commands.fget(self))        
        c.update({
            "click" : click(1),
            "middle click" : click(2),
            "right click" : click(3),
            "center" : self.center,
            "zoom" : self.zoom,
            "unzoom" : self.unzoom,
            })
        return c

    def center(self):
        window = getWindowFocus()
        (width, height) = getWindowDimensions(window)
        x, y = (width / 2, height / 2)
        moveRelativeToWindow(x, y, window)

    def zoom(self):
        self.zoom += 1

    def unzoom(self):
        self.zoom -= 1        

    def onDigit(self, d):
        window = getWindowFocus()
        (width, height) = getWindowDimensions(window)
        blockWidth, blockHeight = (width / (3**self.zoomLevel), height / (3**self.zoomLevel))
        blockWidthCenter, blockHeightCenter = (blockWidth - blockWidth/2, blockHeight - blockHeight/2)
        moveRelativeToWindow(
            blockWidth * (d % 3) + blockWidthCenter,
            blockHeight * (d / 3) + blockHeightCenter, window)
        self.lastDigit = d
