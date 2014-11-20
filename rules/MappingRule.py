from dfly_parser import ARG_DELIMETER, KEY_VALUE_SEPARATOR
from util import EqualityMixin

class MappingRule(EqualityMixin):
    mapping = {}
    extras = []
    defaults = {}
    refOnly = False
    serializedType = "MappingRule"

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
        msg += [ARG_DELIMETER]
        return ''.join(msg)

    def activeForWindow(self, w):
        if not self.refOnly:
            raise NotImplemented(__func__.__name__)
        return False
