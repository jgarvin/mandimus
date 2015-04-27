from protocol import Integer, Dictation, RuleRef, RuleType
import rules.BaseRules as BaseRules

emacsExtras = [
    # Note that repetition counts have a minimum of 3
    # because if you want twice you can just say the
    # command followed by "rep".

    # Small repetition count
    Integer("i", 3, 8),

    # Big repetition count
    Integer("n", 3, 20),
    
    Dictation("text"),
    Dictation("match"),
    Dictation("replace"),
    Integer("big", 0, 2**14),
    RuleRef(BaseRules.CharRule, "charrule"),
    RuleRef(BaseRules.AlphaRule, "alpharule"),
    Dictation("match"),
    Dictation("replace"),
]

emacsDefaults = {
    "n"    : 1,
    "i"    : 1,
    "text" : "",
}

