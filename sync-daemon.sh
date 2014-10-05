#!/bin/zsh

# automagically copy files over to the network share

while true; do
    sleep 1
    for f in $(echo *.py | tr ' ' '\n' | grep -v '#'); do
        if tail $f | grep 'DRAGONSHARE RSYNC' > /dev/null; then
            rsync $f ~/dragonshare/NatLink/NatLink/MacroSystem
        fi
    done
done &> ~/.sync-log &; disown
