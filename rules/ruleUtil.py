from rules.MappingRule import MappingRule

def buildRuleClass(name, filterFunction, omapping, odefaults=None, oextras=None,
                   parent=MappingRule, orefOnly=False):
    class LocalMapping(parent):
        mapping = omapping
        defaults = odefaults if odefaults is not None else parent.defaults
        extras = oextras if oextras is not None else parent.extras
        refOnly = orefOnly

        @classmethod
        def activeForWindow(cls, window):
            return filterFunction(window)

    LocalMapping.__name__ = name
    return LocalMapping
