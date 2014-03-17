#!/usr/bin/env python2

# -- this code is licensed GPLv3
# Copyright 2013 Jezra

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

from SphinxParser import (
    parseSphinxDictionaryFile, parseSphinxLanguageModelFile,
    writeSphinxDictionaryFile, writeSphinxLanguageModelFile)

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

class Mandimus:
    def __init__(self, opts):        
        self.opts = opts
        self.wordBuffer = {}
        self.recognizer = None
        
        # These are the phrases that have meaning in ALL modes
        self.globalPhrases = { "pop" : (lambda : self.pop) }

        self.recognizers = {}

        # generate master corpus
        self.genMasterCorpus()

        #whichever user listed first is root unpoppable mode
        self.activeMode = None
        self.setMode(config.modes[0])

        self.createRecognizer()

    def pop(self):
        pass

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

    def setMode(self, newMode):
        if self.activeMode and isinstance(self.activeMode, newMode):
            return
        print "Activating new mode: %s" % (newMode.__name__)        

        self.activeMode = newMode()        

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
        print "%s: %s" % (str(mode), text)

        t = text.lower()
        
        if mode not in self.wordBuffer:
            self.wordBuffer[mode] = []        

        for word in t.split():
            if word in self.wordCorpus:
                print "recognized word: %s" % (word,)
                self.wordBuffer[mode].append(word)

        if finished:
            self.parseBuffer(self.wordBuffer[type(self.activeMode)])

    def parseBuffer(self, buf):
        # we look backwards from the most recent word to identify phrases
        # by going in this order we make sure longer phrases don't get
        # confused with shorter phrases containing the same words, e.g.
        # we test for "move left" before just "left"
        for phrase in self.phraseList:
            words = phrase.split()
            
            if len(buf) < len(words):
                continue
            
            if words == buf[-len(words):]:
                print "Executing command for phrase: %s" % (phrase,)
                self.commands[phrase]()
                del buf[:] # erase contents in place
                break                
                
    def run(self):
            self.recognizer.listen()

    def quit(self):
            sys.exit()

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

    (options, args) = parser.parse_args()
    #make our mandimus object
    mandimus = Mandimus(options)
    #init gobject threads
    gobject.threads_init()
    #we want a main loop
    main_loop = gobject.MainLoop()
    #handle sigint
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    #run the mandimus
    mandimus.run()
    #start the main loop

    try:
        main_loop.run()
    except:
        print "time to quit"
        main_loop.quit()
        sys.exit()

