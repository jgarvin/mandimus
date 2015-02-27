from rules.ContextualRule import makeContextualRule
from requirements.Emacs import IsEmacs
from requirements.ModeRequirement import ModeRequirement
from rules.emacs.Cmd import Cmd, runEmacsCmd
from rules.emacs.Text import EmacsText
from protocol import makeHashedRule, RuleType, RuleRef
from EventLoop import pushEvent
from EventList import RuleRegisterEvent

_mapping = {
    "key" : None,
    "come key" : None,
    "go key" : None,
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
        command = " ".join(extras['mode_verb_rule']['words'])
        spokenKeyword = " ".join(extras['keyword']['words'])
        writtenKeyword = self.writtenForms[spokenKeyword]
        if command == "key":
            EmacsText("%s" % writtenKeyword, lower=False)()
        elif command == "come key":
            return "(md-go-to-previous \"%s\")" % writtenKeyword
        elif command == "go key":
            return "(md-go-to-next \"%s\")" % writtenKeyword
        else:
            assert False

def normalizeKeywords(keywords):
    return [x if type(x) not in (str, unicode) else (x, x) for x in keywords]

def makeKeywordListRule(modeName, keywords):
    keywords = normalizeKeywords(keywords)
    _mapping = { k[1] : k[0] for k in keywords }
    return makeHashedRule(modeName + "-keyword-list", _mapping, ruleType=RuleType.INDEPENDENT)

class KeywordRule(object):
    def __init__(self, modes, keywords):
        if type(modes) in (str, unicode):
            modes = [modes]
        self.modes = modes
        self.keywords = normalizeKeywords(keywords)

        self.rule = self._buildRule(self.modes, self.keywords)
        self.tellEmacs()

    def _buildRule(self, modes, keywords):
        # choice is arbitrary, just for naming
        modeName = modes[0]

        listRule = makeKeywordListRule(modeName, keywords)
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

    def tellEmacs(self):
        for m in self.modes:
            keywordString = "'(" + " ".join([("\"%s\"" % x[0]) for x in self.keywords]) + ")"
            runEmacsCmd("(md-register-mode-keywords '%s %s)" % (m, keywordString))

# something else will handle the pushing and activating...
