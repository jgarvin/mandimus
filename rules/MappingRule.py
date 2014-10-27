from dfly_parser import ARG_DELIMETER, KEY_VALUE_SEPARATOR

class MappingRule(object):
    mapping = {}
    extras = []
    defaults = {}
    serializedType = "MappingRule"

    @classmethod
    def textSerialize(cls):
        serializeType = cls.serializedType
        
        msg = []
        msg += [serializeType + ARG_DELIMETER + cls.__name__ + ARG_DELIMETER,
                ARG_DELIMETER.join(cls.mapping.keys())]
        msg += [ARG_DELIMETER]
        msg += ["EXTRAS"]
        for extra in cls.extras:
            msg += [ARG_DELIMETER, str(extra)]
        msg += [ARG_DELIMETER]
        msg += ["DEFAULTS"]
        for key, val in cls.defaults.items():
            msg += [ARG_DELIMETER, str(key), KEY_VALUE_SEPARATOR, str(val)]
        msg += [ARG_DELIMETER]
        return ''.join(msg)

    @classmethod
    def equals(cls, other):
        return (cls.mapping == other.mapping and cls.extras == other.extras and cls.defaults == other.defaults)
