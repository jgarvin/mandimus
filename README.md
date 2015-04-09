#Mandimus
Mandimus is a program that interfaces with Dragon Naturally Speaking to provide speech recognition based control of your computer, and in particular Emacs. I wrote this because I developed a repetitive strain injury and want to save other programmers from the pain and frustration I experienced. Existing recognition tools are almost all targeted towards writing word documents, not code.

Originally inspired by the work by Travis Rudd in his Python Coding By Voice video, although AFAIK he never published his code, so this is reimplemented from scratch.

"Mandimus" was supposed to be Latin for "we command" but it turns out I misheard and it's Latin for "we are chomping." I find this sufficiently hilarious that I am unmotivated to rename it. 

#Status

Mandimus is VERY rough around the edges. I wrote it as quickly as possible to get productive again after my injury. To make matters worse this is the project I used to learn elisp. Not even all the code is in this repo, this is only the python client/server. joe-etc contains the elisp stuff, which I will move over here eventually. There's blocking I/O, shelling out to external utilities needlessly, things checked on timers instead of using events, etc. I'm slowly fixing it. But if you manage to install it it's very functional. I use it for 50% of my work time.

#Installation

Mandimus isn't very mature yet, better documentation is forthcoming. Rough sketch:

 * Setup Natlink/DNS12 on windows host or VM (check the much more complete aenea installation instructions for this for now)
 * Copy _dfly_client.py over to the Macrosystem folder in Natlink
 * Run mainloop.sh on your linux host
 * Take all the md-*.el files from my joe-etc repository and load them into emacs
 * Change hard coded IPs/ports for your machines
 * Install xdotool
 * Probably more I've forgotten

#Features

 * Saying multiple commands together in a single utterance, "left up larp camel hello world" will work.
 * Seamlessly switch between arbitrary dictation and command mode within an utterance
 * Dynamic grammars for things like window names, words near cursor, etc.
 * Window management
 * Emacs text/buffer navigation
 * Emacs mode specific commands
 * Support for X-Keys foot pedals, can be used to trigger grammar changes
 * Support out of the box for emacs lisp, python, and some C++
