from protocol import Integer, Dictation, RuleRef, RuleType
import rules.BaseRules as BaseRules

emacsExtras = [
    Integer("i", 2, 8),
    Integer("n", 2, 20),
    Dictation("text"),
    Dictation("match"),
    Dictation("replace"),
    Integer("big", 0, 2**14),
    RuleRef(BaseRules.CharRule, "charrule"),
    RuleRef(BaseRules.AlphaRule, "alpharule"),
    #RuleRef(ModeVerbRule, "mode_verb_rule"),
    Integer("line", 0, 9999),
    Dictation("text"),
    Dictation("match"),
    Dictation("replace"),
]

emacsDefaults = {
    "n"    : 1,
    "i"    : 1,
    "text" : "",
}

