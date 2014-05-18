#!/usr/bin/env python2

import sys
import signal
import gobject
import os.path
import subprocess
import itertools
from tempfile import NamedTemporaryFile
from optparse import OptionParser
from contextlib import nested
from functools import partial
import Queue
import traceback
import time

from SphinxParser import (
    parseSphinxDictionaryFile, parseSphinxLanguageModelFile,
    writeSphinxDictionaryFile, writeSphinxLanguageModelFile)

from listHelpers import rindex
from Window import Window
from WindowEventWatcher import WindowEventWatcher, FocusChangeEvent
from xbox import XboxPadSubscription

import config

#where are the files?
conf_dir = os.path.expanduser("~/.config/mandimus")
lang_dir = os.path.join(conf_dir, "language")
strings_file = os.path.join(conf_dir, "sentences.corpus")
history_file = os.path.join(conf_dir, "mandimus.history")
lang_file = os.path.join(lang_dir,'lm')
dic_file = os.path.join(lang_dir,'dic')
#make the lang_dir if it doesn't exist
if not os.path.exists(lang_dir):
    os.makedirs(lang_dir)

def flattenList(l):
    return list(itertools.chain.from_iterable(l))

### currently we don't push mode immediately with phrases
### like next/previous/editor/browser. we need to change
### that so phrases like 'editor browser new tab editor'
### do the expected thing. when an app isn't already running
### it shouldn't matter, but even then the focus changed event
### won't come through until the window loads so we'll be
### golden anyway

class Mandimus(object):
    def __init__(self, opts, eventQ):
        self.opts = opts
        self.eventQ = eventQ
        self.wordBuffer = {}
        self.recognizer = None

        # These are the phrases that have meaning in ALL modes
        self.globalPhrases = { "pop" : (lambda : self.popModeOrSelect()) }

        self.recognizers = {}

        # generate master corpus
        self.genMasterCorpus()

        #whichever user listed first is root unpoppable mode
        self.modeStack = []
        self.pushMode(config.modes[0])

        self.createRecognizer()

    @property
    def activeMode(self):
        if len(self.modeStack):
            return self.modeStack[-1]
        return None

    def popModeOrSelect(self):
        """If we're already at the top level, then select the
        currently focused window, otherwise pop the mode"""
        if self.modeStack.index(self.activeMode) == 0:
            for mode in config.modes:
                if mode().isModeWindow(Window(winId=Window.FOCUSED)) and not isinstance(self.activeMode, mode):
                    self.pushMode(mode)
                    return
        self.popMode()

    def popMode(self):
        # can't pop off the top level navigation mode
        if self.modeStack.index(self.activeMode) == 0:
            return
        self.modeStack.pop()
        self.updateModeData()
        self.onModeChange()

    def genMasterCorpus(self):
        masterCorpus = set()
        for mode in config.modes:
            masterCorpus.update(mode().wordSet)
        masterCorpus.update(flattenList([k.split() for k in self.globalPhrases.keys()]))

        strings = open(strings_file, "w")

        for word in masterCorpus:
            strings.write(word + '\n')

        strings.close()

        subprocess.call("./language_updater.sh", stdout=sys.stdout,
                        stderr=sys.stdout, shell=True)

        self.masterDictionary = parseSphinxDictionaryFile(dic_file)
        self.masterLanguageModel = parseSphinxLanguageModelFile(lang_file)

    def genDictionary(self, wordSet):
        subDic = {}
        for word in wordSet:
            w = word.upper()
            assert w in self.masterDictionary
            subDic[w] = self.masterDictionary[w]
        return subDic

    def genLanguageModel(self, wordSet):
        subModel = {}
        for ngram, phraseEntries in self.masterLanguageModel.items():
            for entry in phraseEntries:
                phrase = entry[1]

                include = False
                if len(phrase) == 1 and phrase[0] in ["<s>", "</s>"]:
                    # some entries are pure silence, those always make
                    # sense to include
                    include = True
                else:
                    # we only want to include entries containing words if
                    # they're actually in the list
                    word = [w for w in phrase if w not in ["<s>", "</s>"]][0]
                    include = word.lower() in wordSet

                if not include:
                    continue

                if ngram not in subModel:
                    subModel[ngram] = []
                subModel[ngram].append(entry)

        return subModel

    def pushMode(self, newMode):
        # don't allow the same type to be pushed twice in a row
        if self.activeMode and isinstance(self.activeMode, newMode):
            return
        print "Pushing new mode: %s" % (newMode.__name__)

        # delete pending words in old mode's buffer
        del self.activeModeBuffer[:]

        self.modeStack.append(newMode())
        self.updateModeData()
        self.onModeChange()

    def onModeChange(self):
        try:
            # delete everything before the transition 'pop'
            # and the pop itself. stuff before the pop was
            # intended for the old mode to parse
            anchorIdx = rindex(self.activeModeBuffer, "pop")
            del self.activeModeBuffer[:anchorIdx+1]
        except ValueError:
            # happens when 'pop' isn't found, empty the whole
            # buffer to be safe
            del self.activeModeBuffer[:]
            pass

    def updateModeData(self):
        self.commands = self.getModeCommandDictionary(self.activeMode)
        self.wordCorpus = self.getModeWordCorpus(self.activeMode)
        self.phraseList = self.commands.keys()

        # sort by number of words descending
        self.phraseList.sort(key=lambda x: len(x.split()), reverse=True)

    def getModeWordCorpus(self, mode):
        wordCorpus = set()
        wordCorpus.update(mode.wordSet)
        wordCorpus.update(flattenList([k.split() for k in self.globalPhrases]))
        return wordCorpus

    def getModeCommandDictionary(self, mode):
        commands = {}
        commands.update(mode.commands)
        commands.update(self.globalPhrases)
        return commands

    def createRecognizerSetup(self, mode):
        # generate new lang_file/dic_file here, since pocketsphinx doesn't
        # provide a way to directly manipulate the word corpus over time
        modeInstance = mode()
        wordCorpus = self.getModeWordCorpus(modeInstance)
        subDic = self.genDictionary(wordCorpus)
        subModel = self.genLanguageModel(wordCorpus)
        with nested(NamedTemporaryFile(delete=False), NamedTemporaryFile(delete=False)) as (dicF, modelF):
            writeSphinxDictionaryFile(subDic, dicF)
            writeSphinxLanguageModelFile(subModel, modelF)
            dicF.flush()
            modelF.flush()

            from Recognizer import RecognizerSetup
            setup = RecognizerSetup(modelF.name, dicF.name, partial(self.utteranceFinished, mode))
        return setup

    def createRecognizer(self):
        setupObjects = []
        for mode in config.modes:
            setupObjects.append(self.createRecognizerSetup(mode))

        from Recognizer import Recognizer
        self.recognizer = Recognizer(setupObjects, self.opts.microphone)

    def utteranceFinished(self, mode, text, finished):
        # print "%s: %s" % (str(mode), text)

        t = text.lower()

        if mode not in self.wordBuffer:
            self.wordBuffer[mode] = []

        # TODO: set max buffer size so unused modes don't grow
        for word in t.split():
            self.wordBuffer[mode].append(word)

        # we've received an interpretation from all the recognizers,
        # now trying parsing in current mode
        if finished:
            self.parseBuffer()
        # TODO: if a pop didn't occur, clear all the buffers?

    @property
    def activeModeBuffer(self):
        mode = type(self.activeMode)
        if mode not in self.wordBuffer:
            self.wordBuffer[mode] = []
        return self.wordBuffer[mode]

    def parseBuffer(self):
        parsed = []

        buf = self.activeModeBuffer

        while len(buf):
            parsed.append(buf[0])
            del buf[0]

            # we look backwards from the most recent word to identify phrases
            # by going in this order we make sure longer phrases don't get
            # confused with shorter phrases containing the same words, e.g.
            # we test for "move left" before just "left"
            for phrase in self.phraseList:
                words = phrase.split()

                if len(parsed) < len(words):
                    continue

                if words == parsed[-len(words):]:
                    print "Executing command for phrase: %s in mode %s" % (phrase, type(self.activeMode))
                    self.commands[phrase]()

                    # reassign buf in case the mode has changed
                    buf = self.activeModeBuffer

                    parsed = []
                    break

    def run(self):
            self.recognizer.listen()

    def quit(self):
            sys.exit()

    def checkEvents(self):
        try:
            # without a timeout, ctrl-c doesn't work because.. python
            ONEYEAR = 365 * 24 * 60 * 60
            event = self.eventQ.get(block=False)
            # if isinstance(event, FocusChangeEvent):
            #     self.popMode()
            # if isinstance(event, FocusChangeEvent):
            #     for mode in config.modes:
            #         if mode().isModeWindow(event.window) and not isinstance(self.activeMode, mode):
            #             self.pushMode(mode)
            #             break
            #     else:
            #         self.popMode()
        except Queue.Empty:
            pass
        return True

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-i", "--interface",  type="string", dest="interface",
        action='store',
        help="Interface to use (if any). 'q' for Qt, 'g' for GTK")
    parser.add_option("-c", "--continuous",
        action="store_true", dest="continuous", default=False,
        help="starts interface with 'continuous' listen enabled")
    parser.add_option("-H", "--history", type="int",
        action="store", dest="history",
        help="number of commands to store in history file")
    parser.add_option("-m", "--microphone", type="int",
        action="store", dest="microphone", default=None,
        help="Audio input card to use (if other than system default)")

    # queue for event listening threads to push events to the
    # main thread with
    eventQ = Queue.Queue()

    (options, args) = parser.parse_args()
    #make our mandimus object
    mandimus = Mandimus(options, eventQ)
    #init gobject threads
    gobject.threads_init()
    #we want a main loop
    main_loop = gobject.MainLoop()
    eventQTimer = gobject.timeout_add(1000 / 20, mandimus.checkEvents)
    #handle sigint
    # signal.signal(signal.SIGINT, signal.SIG_DFL)
    #run the mandimus
    mandimus.run()
    #start the main loop

    xboxThread = XboxPadSubscription(eventQ)
    windowThread = WindowEventWatcher(eventQ)

    try:
        main_loop.run()
    except:
        xboxThread.stop()
        windowThread.stop()
        main_loop.quit()
        sys.exit()

