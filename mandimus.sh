#!/bin/bash

if ! which xboxdrv &> /dev/null; then
    echo >&2 "xboxdrv not installed."
    exit 1
fi

if ! ps -ef | grep xboxdrv | grep -v xboxdrv &> /dev/null; then
    if [ "$(stat -c '%a' $(which xboxdrv))" -ne "4755" ]; then
        echo >&2 'xboxdrv needs to be setuid root. Run: sudo chmod 4755 $(which xboxdrv)'
        exit 1
    fi
    # if this isn't working check that it's setuid root
    xboxdrv --detach-kernel-driver --silent &
fi

#python mandimus.py

# SDL has a dumb debug output it prints constantly...
unbuffer python xbox.py | grep -v SDL_JoystickGetAxis
