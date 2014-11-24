import logging
import time
import os.path as op
import sys

def initLogging(name, loggingFolder="E:\\log\\", stdOut=False, level=20):
    if not op.exists(loggingFolder):
        os.makedirs(loggingFolder)

    logfile = open(op.join(loggingFolder, "%s-%d.log" % (name, time.time())), 'w')

    rootLogger = logging.getLogger()
    handler = logging.StreamHandler(logfile)
    formatter = logging.Formatter(fmt="%(asctime)s @ %(module)s @ [%(message)s]")
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

### DRAGONSHARE RSYNC
