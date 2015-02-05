from requirements.WindowRequirement import WindowRequirement

IsEmacs = WindowRequirement(wmclass='emacs')
NotEmacs = WindowRequirement(wmclass='emacs', negate=True)
