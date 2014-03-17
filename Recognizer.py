#This is part of Blather
# -- this code is licensed GPLv3
# Copyright 2013 Jezra

import pygst
pygst.require('0.10')
import gst
import os.path
import gobject

from threading import Lock

#define some global variables
this_dir = os.path.dirname( os.path.abspath(__file__) )

class RecognizerSetup:
    def __init__(self, lm, dic, cb):
        self.lm = lm
        self.dic = dic
        self.cb = cb

class Recognizer(gobject.GObject):
    def __init__(self, setupList, src = None):
        gobject.GObject.__init__(self)
        self.commands = {}
        if src:
            audio_src = 'alsasrc device="hw:%d,0"' % (src)
        else:
            audio_src = 'autoaudiosrc'

        # making one big recognizer instead of multiple separate ones
        # lets us avoid some redundant work. we'd prefer to only have
        # one voice activity detector feeding the different sphinx
        # instances for example.
        cmd = audio_src + ' ! audioconvert ! audioresample ! vader name=vad ! tee name=t ! pocketsphinx name=asr0 ! appsink sync=false'
        for i in range(1, len(setupList)):
            cmd += ' t. ! queue ! pocketsphinx name=asr%d ! appsink sync=false' % (i)

        self.pipeline=gst.parse_launch(cmd)

        self.callbacks = {} 
        self.received = {}
        
        for idx, setup in enumerate(setupList):
            asr=self.pipeline.get_by_name('asr%d' % (idx))
            asr.connect('result', self.result)
            asr.set_property('lm', setup.lm)
            asr.set_property('dict', setup.dic)        
            asr.set_property('configured', True)
            self.callbacks[asr] = setup.cb
            self.received[asr] = False

        # otherwise callbacks can intermix
        self.lock = Lock()
            
    def listen(self):
        self.pipeline.set_state(gst.STATE_PLAYING)

    def pause(self):
        # self.vad.set_property('silent', True)
        self.pipeline.set_state(gst.STATE_PAUSED)

    def result(self, asr, text, uttid):
        with self.lock:
            self.callbacks[asr](text, self.checkFinished(asr))

    def checkFinished(self, asr):
        "See if we have an interpretation from all recognizers yet."
        self.received[asr] = True
        finished = False
        if False not in self.received.values():
            for key in self.received:
                self.received[key] = False
            finished = True
        return finished
        
