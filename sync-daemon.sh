#!/bin/zsh

# automagically copy files over to the network share

setopt extended_glob
setopt globdots

while true; do
    sleep 1
    rm -f ~/dragonshare/**/*.\#*
    for f in $(echo *.py | tr ' ' '\n' | grep -v '#'); do
        if tail $f | grep 'DRAGONSHARE RSYNC' > /dev/null; then
            rsync $f ~/dragonshare/NatLink/NatLink/MacroSystem
            # touch so that dragon will reload
            touch ~/dragonshare/NatLink/NatLink/MacroSystem/*.py
        fi
    done
done &> ~/.sync-log &; disown
