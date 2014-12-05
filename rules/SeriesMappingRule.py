import mdlog
log = mdlog.getLogger(__name__)

from MappingRule import MappingRule

class SeriesMappingRule(MappingRule):
    allowCombining = True
    serializedType = "SeriesMappingRule"
