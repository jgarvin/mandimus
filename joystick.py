#!/usr/bin/env python
# -*- coding: utf-8 -*-

from inputs import devices
from Actions import Key

JOYSTICK_CHOICE=0

joystick_actions = {
    # start
    "ABS_Y" : Key("c-g", delay=0, style="hold"),
    # select
    "BTN_BASE4" : Key("c-g", delay=0, style="hold"),
    # center button
    "BTN_TOP2" : None,
    # left up diagonal
    "BTN_BASE" : Key("ca-h", delay=0),
    # right up diagonal
    "BTN_BASE2" : Key("ca-e", delay=0),
    # left down diagonal
    "BTN_BASE3" : Key("c-slash"),
    # right down diagonal
    "BTN_PINKIE" : None, # turn this into repeat
    # up arrow
    "BTN_TRIGGER" : Key("up", delay=0, style="hold"),
    # down arrow
    "BTN_THUMB" : Key("down", delay=0, style="hold"),
    # left arrow
    "BTN_THUMB2" : None,
    # right arrow
    "BTN_TOP" : None
}

def get_chosen_joystick():
    for i, joystick in enumerate(devices.gamepads):
        if i == JOYSTICK_CHOICE:
            return joystick

def joystick_event_loop():
    joystick = get_chosen_joystick()
    while True:
        events = joystick.read()
        for event in events:
            if event.code in joystick_actions and joystick_actions[event.code]:
                print(type(joystick_actions[event.code]))
                print(dir(joystick_actions[event.code]))
                if joystick_actions[event.code].style == "hold":
                    if event.state == 0 or event.state == 127:  # 127 handles ABS_Y
                        joystick_actions[event.code].up({})
                    if event.state == 1 or event.state == 255: # 255 handles ABS_Y
                        joystick_actions[event.code].down({})
                elif joystick_actions[event.code].style == "press_once":
                    if event.state == 1 or event.state == 255: # 255 handles ABS_Y
                        joystick_actions[event.code]({})
            print(type(event.code))
            print(event.ev_type, event.code, event.state)
    # while joystick.read():
        # continue

joystick_event_loop()