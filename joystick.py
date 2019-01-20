#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

from inputs import DeviceManager
from inputs import devices
from Actions import Key


import time
import sys
from collections import defaultdict

JOYSTICK_CHOICE=0

class JoyButton(object):
    def __init__(self, button_name, button_state=1):
        self.button_name = button_name
        self.button_state = button_state

    def state(self, button_state):
        return JoyButton(self.button_name, button_state)

    def __add__(self, other):
        return JoyCombo(self, other)

    def __eq__(self, other):
        return self.button_name == other.button_name and self.button_state == other.button_state

    def __hash__(self):
        return hash((self.button_name, self.button_state))

class JoyCombo(object):
    def __init__(self, a, b=None):
        self.components = []
        self.extend(a)
        if b is not None:
            self.extend(b)

    def extend(self, a):
        if isinstance(a, JoyCombo):
            self.components.extend(a.components)
            return
        elif isinstance(a, JoyButton):
            self.components.append(a)
            return
        assert(False)

    def __eq__(self, other):
        for componentA, componentB in zip(self.components, other.components):
            if componentA != componentB:
                return False
        return True

    def __hash__(self):
        return hash(frozenset(self.components))

class ButtonContainer(object): pass

buttons = ButtonContainer()
buttons.Start = JoyButton("ABS_Y", 255)
buttons.Select = JoyButton("BTN_BASE4")
buttons.Center = JoyButton("BTN_TOP2")
buttons.LeftUp = JoyButton("BTN_BASE")
buttons.RightUp = JoyButton("BTN_BASE2")
buttons.LeftDown = JoyButton("BTN_BASE3")
buttons.RightDown = JoyButton("BTN_PINKIE")
buttons.Up = JoyButton("BTN_TRIGGER")
buttons.Down = JoyButton("BTN_THUMB")
buttons.Left = JoyButton("BTN_THUMB2")
buttons.Right = JoyButton("BTN_TOP")

joystick_actions = {
    buttons.Start                    : Key("c-g", delay=0, style="hold"),
    buttons.Select                   : Key("c-g", delay=0, style="hold"),

    # too easy to hit by accident
    buttons.Center                   : None,
    # buttons.Left + buttons.Center  : Key("enter", delay=0),
    # buttons.Right + buttons.Center : Key("space", delay=0),

    buttons.Left + buttons.LeftUp    : Key("ca-h", delay=0),
    buttons.Left + buttons.RightUp   : Key("ca-e", delay=0),
    buttons.Right + buttons.LeftUp   : Key("ca-backspace", delay=0),
    buttons.Right + buttons.RightUp  : Key("ca-space", delay=0),

    # undo/redo
    buttons.Right + buttons.LeftDown : Key("c-slash"),
    buttons.Left + buttons.LeftDown  : Key("a-slash"),

    buttons.Left + buttons.RightDown : Key("enter", delay=0), # turn this into repeat
    buttons.Left + buttons.Up        : Key("up", delay=0, style="hold"),
    buttons.Left + buttons.Down      : Key("down", delay=0, style="hold"),
    buttons.Right + buttons.Up       : Key("pgup", delay=0),
    buttons.Right + buttons.Down     : Key("pgdown", delay=0),

    # treat these as modifiers
    buttons.Left                     : None,
    buttons.Right                    : None
}

def get_chosen_joystick(device_manager):
    for i, joystick in enumerate(device_manager.gamepads):
        if i == JOYSTICK_CHOICE:
            return joystick

def joystick_event_loop():
    joystick = None
    while True:
        # device_manager = DeviceManager() # re-create every loop in case
        device_manager = devices # re-create every loop in case
        joystick = get_chosen_joystick(device_manager)
        if joystick is not None:
            break
        print("Can't find joystick, will retry in one second...", file=sys.stderr)
        time.sleep(1)

    joystick_state = defaultdict(lambda: 0)
    combo_state = {}
    print("Joystick ready!", file=sys.stderr)
    while True:
        events = joystick.read()
        for event in events:
            print("Event {},{}".format(event.code, event.state), file=sys.stderr)
            joystick_state[event.code] = event.state
        for condition, action in joystick_actions.items():
            if action is None:
                continue

            # normalize buttons to be combos of one-button, no-op if already a combo
            combo = JoyCombo(condition)
            if combo not in combo_state:
                combo_state[combo] = False

            new_combo_state = True
            for button in combo.components:
                if joystick_state[button.button_name] != button.button_state:
                    new_combo_state = False
                    break

            if new_combo_state == combo_state[combo]:
                combo_state[combo] = new_combo_state
                continue
            combo_state[combo] = new_combo_state

            if action.style == "hold":
                if not new_combo_state:
                    action.up({})
                if new_combo_state:
                    action.down({})
            elif action.style == "press_once":
                if new_combo_state:
                    action({})

joystick_event_loop()