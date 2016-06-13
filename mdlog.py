import logging
import time
import os.path as op
import sys
import os
from copy import copy

def initLogging(name, loggingFolder="E:\\log\\", stdOut=False, level=20):
    if not op.exists(loggingFolder):
        os.makedirs(loggingFolder)

    filename = op.join(loggingFolder, "%s-%d.log" % (name, time.time()))
    #logfile = open(filename, 'w')

    rootLogger = logging.getLogger()
    #handler = logging.StreamHandler(logfile)
    handler = logging.FileHandler(filename, mode='w')
    formatter = logging.Formatter(fmt="%(asctime)s @ %(filename)s:%(lineno)s @ %(levelname)s: [%(message)s]")
    handler.setFormatter(formatter)
    rootLogger.addHandler(handler)
    rootLogger.setLevel(level)

    if stdOut:
        s = logging.StreamHandler(sys.stdout)
        s.setFormatter(formatter)
        rootLogger.addHandler(s)

def getLogger(name):
    return logging.getLogger(name)

def flush():
    rootLogger = logging.getLogger()
    for h in rootLogger.handlers:
        h.flush()

def shutdown():
    flush()
    rootLogger = logging.getLogger()
    # we have to manually remove handlers or
    # they will accumulate when we snore/wake
    # because natlink reloads our code but not
    # the logging module
    handlers = copy(rootLogger.handlers)
    for h in handlers:
        h.close()
        rootLogger.removeHandler(h)
    logging.shutdown()

### DRAGONSHARE RSYNC
