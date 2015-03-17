#!/usr/bin/env zsh

VirtualBox &; disown

sleep 5

xdotool search --name 'Oracle VM VirtualBox Manager' set_desktop_for_window 8
VBoxManage startvm Dragon\ Resized

sleep 5

xdotool search --name 'Dragon Resized' set_desktop_for_window 8
