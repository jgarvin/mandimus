'''Wrapper for PieHid32.h

Generated with:
./ctypesgen.py -o piehid32.py -l /home/prophet/opt/lib/libpiehid.so /home/prophet/Downloads/xkeys/piehid/PieHid32.h

Do not modify this file.
'''

__docformat__ =  'restructuredtext'

# Begin preamble

import ctypes, os, sys
from ctypes import *

_int_types = (c_int16, c_int32)
if hasattr(ctypes, 'c_int64'):
    # Some builds of ctypes apparently do not have c_int64
    # defined; it's a pretty good bet that these builds do not
    # have 64-bit pointers.
    _int_types += (c_int64,)
for t in _int_types:
    if sizeof(t) == sizeof(c_size_t):
        c_ptrdiff_t = t
del t
del _int_types

class c_void(Structure):
    # c_void_p is a buggy return type, converting to int, so
    # POINTER(None) == c_void_p is actually written as
    # POINTER(c_void), so it can be treated as a real pointer.
    _fields_ = [('dummy', c_int)]

def POINTER(obj):
    p = ctypes.POINTER(obj)

    # Convert None to a real NULL pointer to work around bugs
    # in how ctypes handles None on 64-bit platforms
    if not isinstance(p.from_param, classmethod):
        def from_param(cls, x):
            if x is None:
                return cls()
            else:
                return x
        p.from_param = classmethod(from_param)

    return p

class UserString:
    def __init__(self, seq):
        if isinstance(seq, basestring):
            self.data = seq
        elif isinstance(seq, UserString):
            self.data = seq.data[:]
        else:
            self.data = str(seq)
    def __str__(self): return str(self.data)
    def __repr__(self): return repr(self.data)
    def __int__(self): return int(self.data)
    def __long__(self): return long(self.data)
    def __float__(self): return float(self.data)
    def __complex__(self): return complex(self.data)
    def __hash__(self): return hash(self.data)

    def __cmp__(self, string):
        if isinstance(string, UserString):
            return cmp(self.data, string.data)
        else:
            return cmp(self.data, string)
    def __contains__(self, char):
        return char in self.data

    def __len__(self): return len(self.data)
    def __getitem__(self, index): return self.__class__(self.data[index])
    def __getslice__(self, start, end):
        start = max(start, 0); end = max(end, 0)
        return self.__class__(self.data[start:end])

    def __add__(self, other):
        if isinstance(other, UserString):
            return self.__class__(self.data + other.data)
        elif isinstance(other, basestring):
            return self.__class__(self.data + other)
        else:
            return self.__class__(self.data + str(other))
    def __radd__(self, other):
        if isinstance(other, basestring):
            return self.__class__(other + self.data)
        else:
            return self.__class__(str(other) + self.data)
    def __mul__(self, n):
        return self.__class__(self.data*n)
    __rmul__ = __mul__
    def __mod__(self, args):
        return self.__class__(self.data % args)

    # the following methods are defined in alphabetical order:
    def capitalize(self): return self.__class__(self.data.capitalize())
    def center(self, width, *args):
        return self.__class__(self.data.center(width, *args))
    def count(self, sub, start=0, end=sys.maxint):
        return self.data.count(sub, start, end)
    def decode(self, encoding=None, errors=None): # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.decode(encoding, errors))
            else:
                return self.__class__(self.data.decode(encoding))
        else:
            return self.__class__(self.data.decode())
    def encode(self, encoding=None, errors=None): # XXX improve this?
        if encoding:
            if errors:
                return self.__class__(self.data.encode(encoding, errors))
            else:
                return self.__class__(self.data.encode(encoding))
        else:
            return self.__class__(self.data.encode())
    def endswith(self, suffix, start=0, end=sys.maxint):
        return self.data.endswith(suffix, start, end)
    def expandtabs(self, tabsize=8):
        return self.__class__(self.data.expandtabs(tabsize))
    def find(self, sub, start=0, end=sys.maxint):
        return self.data.find(sub, start, end)
    def index(self, sub, start=0, end=sys.maxint):
        return self.data.index(sub, start, end)
    def isalpha(self): return self.data.isalpha()
    def isalnum(self): return self.data.isalnum()
    def isdecimal(self): return self.data.isdecimal()
    def isdigit(self): return self.data.isdigit()
    def islower(self): return self.data.islower()
    def isnumeric(self): return self.data.isnumeric()
    def isspace(self): return self.data.isspace()
    def istitle(self): return self.data.istitle()
    def isupper(self): return self.data.isupper()
    def join(self, seq): return self.data.join(seq)
    def ljust(self, width, *args):
        return self.__class__(self.data.ljust(width, *args))
    def lower(self): return self.__class__(self.data.lower())
    def lstrip(self, chars=None): return self.__class__(self.data.lstrip(chars))
    def partition(self, sep):
        return self.data.partition(sep)
    def replace(self, old, new, maxsplit=-1):
        return self.__class__(self.data.replace(old, new, maxsplit))
    def rfind(self, sub, start=0, end=sys.maxint):
        return self.data.rfind(sub, start, end)
    def rindex(self, sub, start=0, end=sys.maxint):
        return self.data.rindex(sub, start, end)
    def rjust(self, width, *args):
        return self.__class__(self.data.rjust(width, *args))
    def rpartition(self, sep):
        return self.data.rpartition(sep)
    def rstrip(self, chars=None): return self.__class__(self.data.rstrip(chars))
    def split(self, sep=None, maxsplit=-1):
        return self.data.split(sep, maxsplit)
    def rsplit(self, sep=None, maxsplit=-1):
        return self.data.rsplit(sep, maxsplit)
    def splitlines(self, keepends=0): return self.data.splitlines(keepends)
    def startswith(self, prefix, start=0, end=sys.maxint):
        return self.data.startswith(prefix, start, end)
    def strip(self, chars=None): return self.__class__(self.data.strip(chars))
    def swapcase(self): return self.__class__(self.data.swapcase())
    def title(self): return self.__class__(self.data.title())
    def translate(self, *args):
        return self.__class__(self.data.translate(*args))
    def upper(self): return self.__class__(self.data.upper())
    def zfill(self, width): return self.__class__(self.data.zfill(width))

class MutableString(UserString):
    """mutable string objects

    Python strings are immutable objects.  This has the advantage, that
    strings may be used as dictionary keys.  If this property isn't needed
    and you insist on changing string values in place instead, you may cheat
    and use MutableString.

    But the purpose of this class is an educational one: to prevent
    people from inventing their own mutable string class derived
    from UserString and than forget thereby to remove (override) the
    __hash__ method inherited from UserString.  This would lead to
    errors that would be very hard to track down.

    A faster and better solution is to rewrite your program using lists."""
    def __init__(self, string=""):
        self.data = string
    def __hash__(self):
        raise TypeError("unhashable type (it is mutable)")
    def __setitem__(self, index, sub):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data): raise IndexError
        self.data = self.data[:index] + sub + self.data[index+1:]
    def __delitem__(self, index):
        if index < 0:
            index += len(self.data)
        if index < 0 or index >= len(self.data): raise IndexError
        self.data = self.data[:index] + self.data[index+1:]
    def __setslice__(self, start, end, sub):
        start = max(start, 0); end = max(end, 0)
        if isinstance(sub, UserString):
            self.data = self.data[:start]+sub.data+self.data[end:]
        elif isinstance(sub, basestring):
            self.data = self.data[:start]+sub+self.data[end:]
        else:
            self.data =  self.data[:start]+str(sub)+self.data[end:]
    def __delslice__(self, start, end):
        start = max(start, 0); end = max(end, 0)
        self.data = self.data[:start] + self.data[end:]
    def immutable(self):
        return UserString(self.data)
    def __iadd__(self, other):
        if isinstance(other, UserString):
            self.data += other.data
        elif isinstance(other, basestring):
            self.data += other
        else:
            self.data += str(other)
        return self
    def __imul__(self, n):
        self.data *= n
        return self

class String(MutableString, Union):

    _fields_ = [('raw', POINTER(c_char)),
                ('data', c_char_p)]

    def __init__(self, obj=""):
        if isinstance(obj, (str, unicode, UserString)):
            self.data = str(obj)
        else:
            self.raw = obj

    def __len__(self):
        return self.data and len(self.data) or 0

    def from_param(cls, obj):
        # Convert None or 0
        if obj is None or obj == 0:
            return cls(POINTER(c_char)())

        # Convert from String
        elif isinstance(obj, String):
            return obj

        # Convert from str
        elif isinstance(obj, str):
            return cls(obj)

        # Convert from c_char_p
        elif isinstance(obj, c_char_p):
            return obj

        # Convert from POINTER(c_char)
        elif isinstance(obj, POINTER(c_char)):
            return obj

        # Convert from raw pointer
        elif isinstance(obj, int):
            return cls(cast(obj, POINTER(c_char)))

        # Convert from object
        else:
            return String.from_param(obj._as_parameter_)
    from_param = classmethod(from_param)

def ReturnString(obj, func=None, arguments=None):
    return String.from_param(obj)

# As of ctypes 1.0, ctypes does not support custom error-checking
# functions on callbacks, nor does it support custom datatypes on
# callbacks, so we must ensure that all callbacks return
# primitive datatypes.
#
# Non-primitive return values wrapped with UNCHECKED won't be
# typechecked, and will be converted to c_void_p.
def UNCHECKED(type):
    if (hasattr(type, "_type_") and isinstance(type._type_, str)
        and type._type_ != "P"):
        return type
    else:
        return c_void_p

# ctypes doesn't have direct support for variadic functions, so we have to write
# our own wrapper class
class _variadic_function(object):
    def __init__(self,func,restype,argtypes):
        self.func=func
        self.func.restype=restype
        self.argtypes=argtypes
    def _as_parameter_(self):
        # So we can pass this variadic function as a function pointer
        return self.func
    def __call__(self,*args):
        fixed_args=[]
        i=0
        for argtype in self.argtypes:
            # Typecheck what we can
            fixed_args.append(argtype.from_param(args[i]))
            i+=1
        return self.func(*fixed_args+list(args[i:]))

# End preamble

_libs = {}
_libdirs = []

# Begin loader

# ----------------------------------------------------------------------------
# Copyright (c) 2008 David James
# Copyright (c) 2006-2008 Alex Holkner
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#  * Neither the name of pyglet nor the names of its
#    contributors may be used to endorse or promote products
#    derived from this software without specific prior written
#    permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ----------------------------------------------------------------------------

import os.path, re, sys, glob
import platform
import ctypes
import ctypes.util

def _environ_path(name):
    if name in os.environ:
        return os.environ[name].split(":")
    else:
        return []

class LibraryLoader(object):
    def __init__(self):
        self.other_dirs=[]

    def load_library(self,libname):
        """Given the name of a library, load it."""
        paths = self.getpaths(libname)

        for path in paths:
            if os.path.exists(path):
                return self.load(path)

        raise ImportError("%s not found." % libname)

    def load(self,path):
        """Given a path to a library, load it."""
        try:
            # Darwin requires dlopen to be called with mode RTLD_GLOBAL instead
            # of the default RTLD_LOCAL.  Without this, you end up with
            # libraries not being loadable, resulting in "Symbol not found"
            # errors
            if sys.platform == 'darwin':
                return ctypes.CDLL(path, ctypes.RTLD_GLOBAL)
            else:
                return ctypes.cdll.LoadLibrary(path)
        except OSError,e:
            raise ImportError(e)

    def getpaths(self,libname):
        """Return a list of paths where the library might be found."""
        if os.path.isabs(libname):
            yield libname
        else:
            # FIXME / TODO return '.' and os.path.dirname(__file__)
            for path in self.getplatformpaths(libname):
                yield path

            path = ctypes.util.find_library(libname)
            if path: yield path

    def getplatformpaths(self, libname):
        return []

# Darwin (Mac OS X)

class DarwinLibraryLoader(LibraryLoader):
    name_formats = ["lib%s.dylib", "lib%s.so", "lib%s.bundle", "%s.dylib",
                "%s.so", "%s.bundle", "%s"]

    def getplatformpaths(self,libname):
        if os.path.pathsep in libname:
            names = [libname]
        else:
            names = [format % libname for format in self.name_formats]

        for dir in self.getdirs(libname):
            for name in names:
                yield os.path.join(dir,name)

    def getdirs(self,libname):
        '''Implements the dylib search as specified in Apple documentation:

        http://developer.apple.com/documentation/DeveloperTools/Conceptual/
            DynamicLibraries/Articles/DynamicLibraryUsageGuidelines.html

        Before commencing the standard search, the method first checks
        the bundle's ``Frameworks`` directory if the application is running
        within a bundle (OS X .app).
        '''

        dyld_fallback_library_path = _environ_path("DYLD_FALLBACK_LIBRARY_PATH")
        if not dyld_fallback_library_path:
            dyld_fallback_library_path = [os.path.expanduser('~/lib'),
                                          '/usr/local/lib', '/usr/lib']

        dirs = []

        if '/' in libname:
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))
        else:
            dirs.extend(_environ_path("LD_LIBRARY_PATH"))
            dirs.extend(_environ_path("DYLD_LIBRARY_PATH"))

        dirs.extend(self.other_dirs)
        dirs.append(".")
        dirs.append(os.path.dirname(__file__))

        if hasattr(sys, 'frozen') and sys.frozen == 'macosx_app':
            dirs.append(os.path.join(
                os.environ['RESOURCEPATH'],
                '..',
                'Frameworks'))

        dirs.extend(dyld_fallback_library_path)

        return dirs

# Posix

class PosixLibraryLoader(LibraryLoader):
    _ld_so_cache = None

    def _create_ld_so_cache(self):
        # Recreate search path followed by ld.so.  This is going to be
        # slow to build, and incorrect (ld.so uses ld.so.cache, which may
        # not be up-to-date).  Used only as fallback for distros without
        # /sbin/ldconfig.
        #
        # We assume the DT_RPATH and DT_RUNPATH binary sections are omitted.

        directories = []
        for name in ("LD_LIBRARY_PATH",
                     "SHLIB_PATH", # HPUX
                     "LIBPATH", # OS/2, AIX
                     "LIBRARY_PATH", # BE/OS
                    ):
            if name in os.environ:
                directories.extend(os.environ[name].split(os.pathsep))
        directories.extend(self.other_dirs)
        directories.append(".")
        directories.append(os.path.dirname(__file__))

        try: directories.extend([dir.strip() for dir in open('/etc/ld.so.conf')])
        except IOError: pass

        unix_lib_dirs_list = ['/lib', '/usr/lib', '/lib64', '/usr/lib64']
        if sys.platform.startswith('linux'):
            # Try and support multiarch work in Ubuntu
            # https://wiki.ubuntu.com/MultiarchSpec
            bitage = platform.architecture()[0]
            if bitage.startswith('32'):
                # Assume Intel/AMD x86 compat
                unix_lib_dirs_list += ['/lib/i386-linux-gnu', '/usr/lib/i386-linux-gnu']
            elif bitage.startswith('64'):
                # Assume Intel/AMD x86 compat
                unix_lib_dirs_list += ['/lib/x86_64-linux-gnu', '/usr/lib/x86_64-linux-gnu']
            else:
                # guess...
                unix_lib_dirs_list += glob.glob('/lib/*linux-gnu')
        directories.extend(unix_lib_dirs_list)

        cache = {}
        lib_re = re.compile(r'lib(.*)\.s[ol]')
        ext_re = re.compile(r'\.s[ol]$')
        for dir in directories:
            try:
                for path in glob.glob("%s/*.s[ol]*" % dir):
                    file = os.path.basename(path)

                    # Index by filename
                    if file not in cache:
                        cache[file] = path

                    # Index by library name
                    match = lib_re.match(file)
                    if match:
                        library = match.group(1)
                        if library not in cache:
                            cache[library] = path
            except OSError:
                pass

        self._ld_so_cache = cache

    def getplatformpaths(self, libname):
        if self._ld_so_cache is None:
            self._create_ld_so_cache()

        result = self._ld_so_cache.get(libname)
        if result: yield result

        path = ctypes.util.find_library(libname)
        if path: yield os.path.join("/lib",path)

# Windows

class _WindowsLibrary(object):
    def __init__(self, path):
        self.cdll = ctypes.cdll.LoadLibrary(path)
        self.windll = ctypes.windll.LoadLibrary(path)

    def __getattr__(self, name):
        try: return getattr(self.cdll,name)
        except AttributeError:
            try: return getattr(self.windll,name)
            except AttributeError:
                raise

class WindowsLibraryLoader(LibraryLoader):
    name_formats = ["%s.dll", "lib%s.dll", "%slib.dll"]

    def load_library(self, libname):
        try:
            result = LibraryLoader.load_library(self, libname)
        except ImportError:
            result = None
            if os.path.sep not in libname:
                for name in self.name_formats:
                    try:
                        result = getattr(ctypes.cdll, name % libname)
                        if result:
                            break
                    except WindowsError:
                        result = None
            if result is None:
                try:
                    result = getattr(ctypes.cdll, libname)
                except WindowsError:
                    result = None
            if result is None:
                raise ImportError("%s not found." % libname)
        return result

    def load(self, path):
        return _WindowsLibrary(path)

    def getplatformpaths(self, libname):
        if os.path.sep not in libname:
            for name in self.name_formats:
                dll_in_current_dir = os.path.abspath(name % libname)
                if os.path.exists(dll_in_current_dir):
                    yield dll_in_current_dir
                path = ctypes.util.find_library(name % libname)
                if path:
                    yield path

# Platform switching

# If your value of sys.platform does not appear in this dict, please contact
# the Ctypesgen maintainers.

loaderclass = {
    "darwin":   DarwinLibraryLoader,
    "cygwin":   WindowsLibraryLoader,
    "win32":    WindowsLibraryLoader
}

loader = loaderclass.get(sys.platform, PosixLibraryLoader)()

def add_library_search_dirs(other_dirs):
    loader.other_dirs = other_dirs

load_library = loader.load_library

del loaderclass

# End loader

add_library_search_dirs([])

# Begin libraries

_libs["/home/prophet/opt/lib/libpiehid.so"] = load_library("/home/prophet/opt/lib/libpiehid.so")

# 1 libraries
# End libraries

# No modules

enum_anon_1 = c_int # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 42

piNone = 0 # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 42

piNewData = 1 # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 42

piDataChange = 2 # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 42

EEventPI = enum_anon_1 # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 42

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 143
class struct__HID_ENUM_INFO(Structure):
    pass

struct__HID_ENUM_INFO.__slots__ = [
    'PID',
    'Usage',
    'UP',
    'readSize',
    'writeSize',
    'DevicePath',
    'Handle',
    'Version',
    'ManufacturerString',
    'ProductString',
]
struct__HID_ENUM_INFO._fields_ = [
    ('PID', c_uint),
    ('Usage', c_uint),
    ('UP', c_uint),
    ('readSize', c_long),
    ('writeSize', c_long),
    ('DevicePath', c_char * 256),
    ('Handle', c_uint),
    ('Version', c_uint),
    ('ManufacturerString', c_char * 128),
    ('ProductString', c_char * 128),
]

TEnumHIDInfo = struct__HID_ENUM_INFO # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 143

PHIDDataEvent = CFUNCTYPE(UNCHECKED(c_uint), POINTER(c_ubyte), c_uint, c_uint) # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 148

PHIDErrorEvent = CFUNCTYPE(UNCHECKED(c_uint), c_uint, c_uint) # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 149

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 151
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'GetErrorString'):
    GetErrorString = _libs['/home/prophet/opt/lib/libpiehid.so'].GetErrorString
    GetErrorString.argtypes = [c_int, String, c_int]
    GetErrorString.restype = None

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 152
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'EnumeratePIE'):
    EnumeratePIE = _libs['/home/prophet/opt/lib/libpiehid.so'].EnumeratePIE
    EnumeratePIE.argtypes = [c_long, POINTER(TEnumHIDInfo), POINTER(c_long)]
    EnumeratePIE.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 153
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'GetXKeyVersion'):
    GetXKeyVersion = _libs['/home/prophet/opt/lib/libpiehid.so'].GetXKeyVersion
    GetXKeyVersion.argtypes = [c_long]
    GetXKeyVersion.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 154
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'SetupInterfaceEx'):
    SetupInterfaceEx = _libs['/home/prophet/opt/lib/libpiehid.so'].SetupInterfaceEx
    SetupInterfaceEx.argtypes = [c_long]
    SetupInterfaceEx.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 155
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'CloseInterface'):
    CloseInterface = _libs['/home/prophet/opt/lib/libpiehid.so'].CloseInterface
    CloseInterface.argtypes = [c_long]
    CloseInterface.restype = None

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 156
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'CleanupInterface'):
    CleanupInterface = _libs['/home/prophet/opt/lib/libpiehid.so'].CleanupInterface
    CleanupInterface.argtypes = [c_long]
    CleanupInterface.restype = None

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 157
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'ReadData'):
    ReadData = _libs['/home/prophet/opt/lib/libpiehid.so'].ReadData
    ReadData.argtypes = [c_long, POINTER(c_ubyte)]
    ReadData.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 158
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'BlockingReadData'):
    BlockingReadData = _libs['/home/prophet/opt/lib/libpiehid.so'].BlockingReadData
    BlockingReadData.argtypes = [c_long, POINTER(c_ubyte), c_int]
    BlockingReadData.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 159
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'WriteData'):
    WriteData = _libs['/home/prophet/opt/lib/libpiehid.so'].WriteData
    WriteData.argtypes = [c_long, POINTER(c_ubyte)]
    WriteData.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 160
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'FastWrite'):
    FastWrite = _libs['/home/prophet/opt/lib/libpiehid.so'].FastWrite
    FastWrite.argtypes = [c_long, POINTER(c_ubyte)]
    FastWrite.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 161
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'ReadLast'):
    ReadLast = _libs['/home/prophet/opt/lib/libpiehid.so'].ReadLast
    ReadLast.argtypes = [c_long, POINTER(c_ubyte)]
    ReadLast.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 162
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'ClearBuffer'):
    ClearBuffer = _libs['/home/prophet/opt/lib/libpiehid.so'].ClearBuffer
    ClearBuffer.argtypes = [c_long]
    ClearBuffer.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 163
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'GetReadLength'):
    GetReadLength = _libs['/home/prophet/opt/lib/libpiehid.so'].GetReadLength
    GetReadLength.argtypes = [c_long]
    GetReadLength.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 164
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'GetWriteLength'):
    GetWriteLength = _libs['/home/prophet/opt/lib/libpiehid.so'].GetWriteLength
    GetWriteLength.argtypes = [c_long]
    GetWriteLength.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 165
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'SetDataCallback'):
    SetDataCallback = _libs['/home/prophet/opt/lib/libpiehid.so'].SetDataCallback
    SetDataCallback.argtypes = [c_long, PHIDDataEvent]
    SetDataCallback.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 166
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'SetErrorCallback'):
    SetErrorCallback = _libs['/home/prophet/opt/lib/libpiehid.so'].SetErrorCallback
    SetErrorCallback.argtypes = [c_long, PHIDErrorEvent]
    SetErrorCallback.restype = c_uint

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 170
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'SuppressDuplicateReports'):
    SuppressDuplicateReports = _libs['/home/prophet/opt/lib/libpiehid.so'].SuppressDuplicateReports
    SuppressDuplicateReports.argtypes = [c_long, c_bool]
    SuppressDuplicateReports.restype = None

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 171
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'DisableDataCallback'):
    DisableDataCallback = _libs['/home/prophet/opt/lib/libpiehid.so'].DisableDataCallback
    DisableDataCallback.argtypes = [c_long, c_bool]
    DisableDataCallback.restype = None

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 172
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'IsDataCallbackDisabled'):
    IsDataCallbackDisabled = _libs['/home/prophet/opt/lib/libpiehid.so'].IsDataCallbackDisabled
    IsDataCallbackDisabled.argtypes = [c_long]
    IsDataCallbackDisabled.restype = c_bool

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 173
if hasattr(_libs['/home/prophet/opt/lib/libpiehid.so'], 'GetSuppressDuplicateReports'):
    GetSuppressDuplicateReports = _libs['/home/prophet/opt/lib/libpiehid.so'].GetSuppressDuplicateReports
    GetSuppressDuplicateReports.argtypes = [c_long]
    GetSuppressDuplicateReports.restype = c_bool

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 44
try:
    MAX_XKEY_DEVICES = 128
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 45
try:
    PI_VID = 1523
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 50
try:
    PIE_HID_ENUMERATE_BAD_HID_DEVICE = 101
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 51
try:
    PIE_HID_ENUMERATE_NO_DEVICES_FOUND = 102
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 52
try:
    PIE_HID_ENUMERATE_OTHER_ENUM_ERROR = 103
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 53
try:
    PIE_HID_ENUMERATE_ERROR_GETTING_DEVICE_DETAIL = 104
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 54
try:
    PIE_HID_ENUMERATE_ERROR_GETTING_DEVICE_DETATIL2 = 105
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 55
try:
    PIE_HID_ENUMERATE_UNABLE_TO_OPEN_HANDLE = 106
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 56
try:
    PIE_HID_ENUMERATE_GET_ATTRIBUTES_ERROR = 107
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 57
try:
    PIE_HID_ENUMERATE_VENDOR_ID_ERROR = 108
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 58
try:
    PIE_HID_ENUMERATE_GET_PREPARSED_DATA_ERROR = 109
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 59
try:
    PIE_HID_ENUMERATE_GET_CAPS = 110
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 60
try:
    PIE_HID_ENUMERATE_GET_MANUFACTURER_STRING = 111
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 61
try:
    PIE_HID_ENUMERATE_GET_PRODUCT_STRING = 112
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 64
try:
    PIE_HID_SETUP_BAD_HANDLE = 201
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 65
try:
    PIE_HID_SETUP_CANNOT_ALLOCATE_MEM_FOR_RING = 202
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 66
try:
    PIE_HID_SETUP_CANNOT_CREATE_MUTEX = 203
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 67
try:
    PIE_HID_SETUP_CANNOT_CREATE_READ_THREAD = 204
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 68
try:
    PIE_HID_SETUP_CANNOT_OPEN_READ_HANDLE = 205
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 69
try:
    PIE_HID_SETUP_CANNOT_OPEN_READ_HANDLE_ACCESS_DENIED = 206
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 70
try:
    PIE_HID_SETUP_CANNOT_OPEN_READ_HANDLE_BAD_PATH = 207
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 71
try:
    PIE_HID_SETUP_CANNOT_OPEN_WRITE_HANDLE = 208
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 72
try:
    PIE_HID_SETUP_CANNOT_OPEN_WRITE_HANDLE_ACCESS_DENIED = 209
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 73
try:
    PIE_HID_SETUP_CANNOT_OPEN_WRITE_HANDLE_BAD_PATH = 210
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 76
try:
    PIE_HID_READ_BAD_INTERFACE_HANDLE = 301
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 77
try:
    PIE_HID_READ_LENGTH_ZERO = 302
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 78
try:
    PIE_HID_READ_CANNOT_ACQUIRE_MUTEX = 303
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 79
try:
    PIE_HID_READ_INSUFFICIENT_DATA = 304
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 80
try:
    PIE_HID_READ_CANNOT_RELEASE_MUTEX = 305
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 81
try:
    PIE_HID_READ_CANNOT_RELEASE_MUTEX2 = 306
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 82
try:
    PIE_HID_READ_INVALID_HANDLE = 307
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 83
try:
    PIE_HID_READ_DEVICE_DISCONNECTED = 308
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 84
try:
    PIE_HID_READ_READ_ERROR = 309
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 85
try:
    PIE_HID_READ_BYTES_NOT_EQUAL_READSIZE = 310
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 86
try:
    PIE_HID_READ_BLOCKING_READ_DATA_TIMED_OUT = 311
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 89
try:
    PIE_HID_WRITE_BAD_HANDLE = 401
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 90
try:
    PIE_HID_WRITE_LENGTH_ZERO = 402
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 91
try:
    PIE_HID_WRITE_FAILED = 403
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 92
try:
    PIE_HID_WRITE_INCOMPLETE = 404
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 93
try:
    PIE_HID_WRITE_UNABLE_TO_ACQUIRE_MUTEX = 405
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 94
try:
    PIE_HID_WRITE_UNABLE_TO_RELEASE_MUTEX = 406
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 95
try:
    PIE_HID_WRITE_HANDLE_INVALID = 407
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 96
try:
    PIE_HID_WRITE_BUFFER_FULL = 408
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 97
try:
    PIE_HID_WRITE_PREV_WRITE_FAILED = 409
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 98
try:
    PIE_HID_WRITE_PREV_WRITE_WRONG_NUMBER = 410
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 99
try:
    PIE_HID_WRITE_TIMER_FAILED = 411
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 100
try:
    PIE_HID_WRITE_PREV_WRITE_UNABLE_TO_RELEASE_MUTEX = 412
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 101
try:
    PIE_HID_WRITE_BUFFER_FULL2 = 413
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 102
try:
    PIE_HID_WRITE_FAST_WRITE_ERROR = 414
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 105
try:
    PIE_HID_READLAST_BAD_HANDLE = 501
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 106
try:
    PIE_HID_READLAST_LENGTH_ZERO = 502
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 107
try:
    PIE_HID_READLAST_UNABLE_TO_ACQUIRE_MUTEX = 503
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 108
try:
    PIE_HID_READLAST_INSUFFICIENT_DATA = 504
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 109
try:
    PIE_HID_READLAST_UNABLE_TO_RELEASE_MUTEX = 505
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 110
try:
    PIE_HID_READLAST_UNABLE_TO_RELEASE_MUTEX2 = 506
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 111
try:
    PIE_HID_READLAST_INVALID_HANDLE = 507
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 114
try:
    PIE_HID_CLEARBUFFER_BAD_HANDLE = 601
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 115
try:
    PIE_HID_CLEARBUFFER_UNABLE_TO_RELEASE_MUTEX = 602
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 116
try:
    PIE_HID_CLEARBUFFER_UNABLE_TO_ACQUIRE_MUTEX = 603
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 119
try:
    PIE_HID_DATACALLBACK_BAD_HANDLE = 701
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 120
try:
    PIE_HID_DATACALLBACK_INVALID_INTERFACE = 702
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 121
try:
    PIE_HID_DATACALLBACK_CANNOT_CREATE_CALLBACK_THREAD = 703
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 122
try:
    PIE_HID_DATACALLBACK_CALLBACK_ALREADY_SET = 704
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 125
try:
    PIE_HID_ERRORCALLBACK_BAD_HANDLE = 801
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 126
try:
    PIE_HID_ERRORCALLBACK_INVALID_INTERFACE = 802
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 127
try:
    PIE_HID_ERRORCALLBACK_CANNOT_CREATE_ERROR_THREAD = 803
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 128
try:
    PIE_HID_ERRORCALLBACK_ERROR_THREAD_ALREADY_CREATED = 1804
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 145
try:
    MAX_XKEY_DEVICES = 128
except:
    pass

# /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 146
try:
    PI_VID = 1523
except:
    pass

_HID_ENUM_INFO = struct__HID_ENUM_INFO # /home/prophet/Downloads/xkeys/piehid/PieHid32.h: 143

# No inserted files

