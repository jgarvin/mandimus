import os, sys
import Queue
from dfly_server import DragonflyThread
from Actions import Key, Text, Camel, Underscore, Hyphen, Speak, SelectWindow
from DragonflyNode import ConnectedEvent
from WindowEventWatcher import WindowEventWatcher, FocusChangeEvent, WindowListEvent
from Window import Window
from listHelpers import splitFlatten, deCamelize

from rules.Rule import registerRule, registeredRules
from rules.SeriesMappingRule import SeriesMappingRule
from rules.MappingRule import MappingRule
from rules.Elements import Integer, Dictation
from rules.Always import AlwaysRule
from rules.Emacs import EmacsRule
from rules.XMonad import XMonadRule
from rules.CUA import CUARule
from rules.Chrome import ChromeRule

badWindows = {
    "Desktop",
    ".*Edge Panel.*",
    "gnome-screensaver",
    "Panel",
    "$^", # empty string
}

def filterWindows(w):
    # filter out known bad names
    for bad in badWindows:
        if re.search(bad, w.name) is not None:
            return False

    # filter out windows that don't have
    # icons set. typically these only exist
    # as an artifact of dealing with X
    if not w.hasIcon:
        return False

    return True

spokenWindowRules = set()
def spokenWindowRule(f):
    global spokenWindowRules
    spokenWindowRules.add(f)
    return f

@spokenWindowRule
def weechat(w):
    if "terminal" in w.wmclass.lower() and "weechat" in w.name:
        return ["wee", "chat"]
    return []
    
# so write rules for specific window types
# and then fall back to generic word search
# grammar only when the windows aren't special
# cased?

class RestartEvent(object): pass
class ExitEvent(object): pass

import re, string
def getWindowWords(windowStr):
    words = [windowStr.strip()]
    words = [s.strip() for s in splitFlatten(words, ' ')]
    words = [s.strip() for s in splitFlatten(words, '-')]
    words = [s.strip() for s in splitFlatten(words, '_')]
    
    newWords = []
    for w in words:
        newWords.extend(filter(None, re.split("[" + string.punctuation + "]+", w)))
    words = newWords

    newWords = []
    for w in words:
        newWords.extend(deCamelize(w))
    words = newWords
    
    return [w.lower() for w in words]

class MainThread(object):
    def __init__(self):
        self.eventQ = Queue.Queue()
        self.dfly = DragonflyThread(('', 23133), self.eventQ)
        self.win = WindowEventWatcher(self.eventQ, filterWindows)
        self.run = True

    def determineRules(self, window):
        for r in registeredRules():
            if r.activeForWindow(window):
                self.dfly.loadGrammar(r)
            else:
                self.dfly.unloadGrammar(r)

    def __call__(self):
        class MainRule(SeriesMappingRule):
            mapping = { "restart mandimus" : (lambda x: self.eventQ.put(RestartEvent())),
                        "completely exit mandimus" : (lambda x: self.eventQ.put(ExitEvent())) }


        try:
            while self.run:
                # without a timeout, ctrl-c doesn't work because.. python
                ONEYEAR = 365 * 24 * 60 * 60
                ev = self.eventQ.get(True, ONEYEAR)
                if isinstance(ev, ConnectedEvent):
                    self.dfly.loadGrammar(MainRule)

                    # so that rules apply for whatever is focused on startup
                    self.determineRules(Window(winId=Window.FOCUSED))
                elif isinstance(ev, RestartEvent):
                    self.restart()
                elif isinstance(ev, ExitEvent):
                    self.stop()
                    return
                elif isinstance(ev, FocusChangeEvent):
                    self.determineRules(ev.window)
                elif isinstance(ev, WindowListEvent):
                    self.handleWindowList(ev)
                elif len(ev): # don't print heartbeats
                    print "message: " + str(ev)
        except KeyboardInterrupt:
            self.stop()
            sys.exit()

    def handleWindowList(self, ev):
        # sometimes at startup list is empty
        if not ev.windows:
            return

        spokenWindows = {}
        for w in ev.windows:
            global spokenWindowRules
            spokenForms = []
            for rule in spokenWindowRules:
                spokenForms = rule(w)
                if spokenForms != []:
                    spokenForms = [set(spokenForms)]
                    break

            if spokenForms == []:
                # thought about using name instead of wmclass,
                # but the title tends to contain debris like
                # the name of the currently opened document/page
                nameset = getWindowWords(w.name)
                classset = getWindowWords(w.wmclass)
                spokenForms = [nameset, classset]
                #print spokenForms

            spokenWindows[w] = spokenForms
            # print [w.name, getWindowWords(w.wmclass)]

        # remove empty sets
        for w, spokenForms in spokenWindows.items():
            try:
                spokenForms.remove(set())
            except ValueError:
                pass # python is stupid
            try:
                spokenForms.remove(set(u''))
            except ValueError:
                pass # python is stupid
        # remove windows that map to no forms
        spokenWindows = dict((k, v) for k, v in spokenWindows.iteritems() if v)

        self.buildWindowGrammar(spokenWindows)        

    # TODO: need to cycle through all windows matching a phrase, so if you
    # say "terminal" and a terminal is already focused, pick another one.
    # Also, when you don't have a terminal focused and say 'terminal', pick
    # the one most recently activated rather than always picking the first
    # in the list
    def buildWindowGrammar(self, spokenWindows):
        omapping = {}
        word2Windows = {}
        for w, spokenForms in spokenWindows.items():
            if not spokenForms:
                continue

            grammar = ['win']
            first = True
            grammar += ["("]
            for form in spokenForms:
                if not first:
                    grammar += ["|"]
                for word in form:
                    if word not in word2Windows:
                        word2Windows[word] = set()
                    word2Windows[word].add(w)
                    grammar.append("[%s]" % word)
                first = False
            grammar += [")"]
            grammar = ' '.join(grammar)
            omapping[grammar] = SelectWindow(word2Windows)

        print omapping.keys()
        class WindowRule(MappingRule):
            mapping = omapping
            
        self.dfly.loadGrammar(WindowRule)

    def stop(self):
        self.run = False
        self.dfly.stop()
        self.win.stop()

    def restart(self):
        self.stop()
        sys.stdout.flush()
        sys.stderr.flush()
        python = sys.executable
        os.execl(python, python, *sys.argv)        

if __name__ == "__main__":
    main = MainThread()
    main()
