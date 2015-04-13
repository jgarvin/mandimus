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

def makeKeywordListRule(name, keywords):
    keywords = normalizeKeywords(keywords)
    _mapping = { k[1] : k[0] for k in keywords }
    return makeHashedRule(name, _mapping, ruleType=RuleType.INDEPENDENT)

class KeywordRule(object):
    def __init__(self, requirements, keywords, name=None):
        if type(requirements) in (str, unicode):
            # a string is interpreted to be a mode requirement
            self.requirements = [ModeRequirement([requirements])]
        elif type(requirements) in (list,) and all([type(x) in (str, unicode) for x in requirements]):
            # a list of strings is interpreted to be multiple possible modes 
            self.requirements = [ModeRequirement(requirements)]
        else:
            # otherwise we assume we're being passed a real list of requirement objects
            self.requirements = requirements
        self.name = name
        if not self.name:
            self.name = requirements[0] + "-keyword-rule"
        self.keywords = normalizeKeywords(keywords)

        self.rule = self._buildRule(self.requirements, self.keywords)
        self.tellEmacs()

    def _buildRule(self, requirements, keywords):
        listRule = makeKeywordListRule(self.name + "-list", keywords)
        pushEvent(RuleRegisterEvent(listRule))

        mapping = {
            "<mode_verb_rule> <keyword>" : KeywordCmd(keywords),
        }

        extras = [
            RuleRef(VerbRule, "mode_verb_rule"),
            RuleRef(listRule, "keyword"),
        ]

        KeywordRule = makeContextualRule(self.name, mapping, extras)
        KeywordRule.context.addRequirement(IsEmacs)
        for r in self.requirements:
            KeywordRule.context.addRequirement(r)
        return KeywordRule

    def tellEmacs(self):
        # this isn't ideal, because it only takes into account one type of requirement
        # however that is the most common kind
        keywordString = "'(" + " ".join([("\"%s\"" % x[0]) for x in self.keywords]) + ")"
        for m in self.requirements:
            if isinstance(m, ModeRequirement):
                for mode in m.modes:
                    runEmacsCmd("(md-register-mode-keywords '%s %s)" % (mode, keywordString))

# something else will handle the pushing and activating...
