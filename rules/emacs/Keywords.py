from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.Cmd import Cmd
from rules.emacs.Text import EmacsText
from protocol import makeHashedRule, RuleType, RuleRef
from EventLoop import pushEvent
from EventList import RuleRegisterEvent

_mapping = {
    "key" : None,
    "new" : None,
    "prior" : None,
    "future" : None,
}

VerbRule = makeHashedRule("VerbRule", _mapping, ruleType=RuleType.INDEPENDENT)
pushEvent(RuleRegisterEvent(VerbRule))

class KeywordCmd(Cmd):
    def __init__(self, keywords, log=False):
        self.writtenForms = {}
        for i in keywords:
            self.writtenForms[i[1]] = i[0]
        Cmd.__init__(self, None, log)

    def _lisp(self, extras={}):
        command = extras['mode_verb_rule']
        spokenKeyword = extras['keyword']
        writtenKeyword = self.writtenForms[spokenKeyword]
        if command == "key":
            EmacsText("%s" % writtenKeyword, lower=False)()
        elif command == "new":
            return "(md-insert-snippet \"%s\")" % writtenKeyword
        elif command == "prior":
            return "(md-go-to-previous \"%s\")" % writtenKeyword
        elif command == "future":
            return "(md-go-to-next \"%s\")" % writtenKeyword
        else:
            assert False

def normalizeKeywords(keywords):
    return [x if type(x) != str else (x, x) for x in keywords]

def makeKeywordListRule(modeName, keywords, includeDefaultOps):
    if includeDefaultOps:
        defaultOperators = [
            ["+", "plus"],
            ["-", "minus"],
            ["*", "times"],
            ["/", "divide"],
            ["%", "mod"],
        ]
        keywords.extend([x for x in defaultOperators if x not in keywords]) 

    keywords = normalizeKeywords(keywords)
    _mapping = { k[1] : k[0] for k in keywords }
    return makeHashedRule(modeName + "-keyword-list", _mapping, ruleType=RuleType.INDEPENDENT)

def makeKeywordRule(modes, keywords, includeDefaultOps=True):
    if type(modes) in (list, set, tuple):
        # choice is arbitrary, just for naming
        modeName = modes[0]
    else:
        modeName = modes

    keywords = normalizeKeywords(keywords)

    listRule = makeKeywordListRule(modeName, keywords, includeDefaultOps)
    pushEvent(RuleRegisterEvent(listRule))

    mapping = {
        "<mode_verb_rule> <keyword>" : KeywordCmd(keywords),
    }

    extras = [
        RuleRef(VerbRule, "mode_verb_rule"),
        RuleRef(listRule, "keyword"),
    ]

    KeywordRule = makeContextualRule(modeName + "-keyword-rule", mapping, extras)
    KeywordRule.context.addRequirement(IsEmacs)
    KeywordRule.context.addRequirement(ModeRequirement(modes=modes))
    return KeywordRule

# something else will handle the pushing and activating...
