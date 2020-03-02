from requirements.WindowRequirement import WindowRequirement

IsTerminal = WindowRequirement(wmclass=('xfce4-terminal', 'xterm', 'gnome-terminal-server'))
NotTerminal = WindowRequirement(wmclass=('xfce4-terminal', 'xterm', 'gnome-terminal-server'), negate=True)
