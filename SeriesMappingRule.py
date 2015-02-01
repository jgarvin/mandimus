import mdlog
log = mdlog.getLogger(__name__)


from dragonfly import CompoundRule, MappingRule, RuleRef, Repetition

# taken from dictation-toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts/blob/f639abf167fdd59ca9ad8039f4ff582deeda34e0/dynamics/subversion_grammar.py

class SeriesMappingRule(CompoundRule):
    """Just like a mapping rule except it lets you repeat commands."""
    
    def __init__(self, name=None, mapping=None, extras=None, defaults=None):
        mapping_rule = MappingRule(mapping=mapping, extras=extras,
            defaults=defaults, exported=False)
        single = RuleRef(rule=mapping_rule)
        series = Repetition(single, min=1, max=8, name="series")

        compound_spec = "<series>"
        compound_extras = [series]
        CompoundRule.__init__(self, name=name, spec=compound_spec,
            extras=compound_extras, exported=True)

    def _process_recognition(self, node, extras):  # @UnusedVariable
        series = extras["series"]
        for action in series:
            action.execute()

def combineSeriesMappingRules(series):
    mapping = {}
    extras = []
    defaults = {}
    parts = []
    allowCombining = False
    isMergedSeries = True
        
    name = []

    # TODO: need to rewrite the error checking since
    # dragonfly apparently doesn't have equality
    # comparisons for the element classes

    for a in series:
        commonKeys = set(mapping.keys()) & set(a.mapping.keys()) 
        # if commonKeys:
        #     log.info("MergedSeries and %s have mapping keys in common, skipping. Keys: %s" % (a.name, commonKeys))
        #     continue
        mapping.update(a.mapping.items())
        
        # love me some n^2 on small inputs
        # for i in a.extras:
        #     for j in extras:
        #         if i.name == j.name and i != j:
        #             log.info("MergedSeries and %s disagree on extras (%s) (%s)" % (a.name, i, j))
        extras += a.extras
        
        # for i in a.defaults:
        #     if i in defaults:
        #         if a.defaults[i] != defaults[i]:
        #             log.info("MergedSeries and %s disagree on defaults (%s) (%s)" % (a.name, a.defaults[i], defaults[i]))
        defaults.update(a.defaults.items())

        parts.append(a)

    parts.sort(key=lambda x: x.name)
    name = ','.join([x.name for x in parts])

    newRule = SeriesMappingRule(name=name, mapping=mapping, extras=extras, defaults=defaults)
    setattr(newRule, "parts", parts)
    setattr(newRule, "mandimusFlags", ["ALLOWCOMBINING"])
    setattr(newRule, "isMergedSeries", True)

    # print mapping
    # print extras
    # print defaults

    return newRule


### DRAGONSHARE RSYNC
