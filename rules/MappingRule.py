from dfly_parser import ARG_DELIMETER, KEY_VALUE_SEPARATOR
from util import EqualityMixin

class MappingRule(EqualityMixin):
    mapping = {}
    extras = []
    defaults = {}
    allowCombining = False
    refOnly = False
    isMergedSeries = False
    serializedType = "MappingRule"

    def __str__(self):
        if self.mapping:
            maxKeyLen = len(max([str(x) for x in self.mapping.keys()], key=lambda x: len(x)))
            maxValLen = len(max([str(x) for x in self.mapping.values()], key=lambda x: len(x)))
        else:
            maxKeyLen = 0
            maxValLen = 0
        result = []
        result.append("%s = {" % type(self))
        for k, v in self.mapping.items():
            result.append("    %*s    :    %*s" % (maxKeyLen, k, maxValLen, v))
        result.append("}")
        return "\n".join(result)

    def __eq__(self, other):
        return (self.name == other.name
                and self.mapping == other.mapping
                and self.extras == other.extras and self.defaults == other.defaults)

    @property
    def name(self):
        return type(self).__name__

    def textSerialize(self):
        serializeType = self.serializedType
        
        msg = []
        msg += [serializeType + ARG_DELIMETER + self.name + ARG_DELIMETER,
                ARG_DELIMETER.join(self.mapping.keys())]
        msg += [ARG_DELIMETER]
        msg += ["EXTRAS"]
        for extra in self.extras:
            msg += [ARG_DELIMETER, str(extra)]
        msg += [ARG_DELIMETER]
        msg += ["DEFAULTS"]
        for key, val in self.defaults.items():
            msg += [ARG_DELIMETER, str(key), KEY_VALUE_SEPARATOR, str(val)]
        msg += [ARG_DELIMETER]
        msg += ["FLAGS"]
        if self.refOnly:
            msg += [ARG_DELIMETER]
            msg += "REFONLY"
        if self.allowCombining:
            msg += [ARG_DELIMETER]
            msg += "ALLOWCOMBINING"
        msg += [ARG_DELIMETER]
        return ''.join(msg)

    def activeForWindow(self, w):
        if not self.refOnly:
            raise NotImplemented(__func__.__name__)
        return False
