#!/bin/zsh


while true; do
    sleep 3
    kill -9 $(jobs -p)
    rm **/*.pyc
    rm ~/dragonshare/NatLink/NatLink/MacroSystem/*.pyc
    #touch ~/dragonshare/NatLink/NatLink/MacroSystem/*.py

    # Let ctrl-c pass through to python so we can use it
    # to trigger restarts
    TRAPINT() {}
    
    python MainThread.py

    # but only when python is running, this way if we
    # hold control-c we still quit the whole loop
    unfunction TRAPINT
done
