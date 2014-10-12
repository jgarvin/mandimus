import sys, inspect, random

def topy(path):
    if path.endswith == ".pyc":
        return path[:-1]

    return path

def resetImportState():
    """We tag modules with a random value to indicate
    that we've imported them since the last reset.
    This way if you import x and y, and y happens to
    also import x, we don't end up reloading x again,
    even if in both cases importOrReload('x') was called."""
    global __importState
    __importState = random.randint(0, sys.maxint) 

resetImportState()        

def getCallingModule():
    #print inspect.stack()
    for frm in inspect.stack():
        frmfile = topy(frm[1])
        mod = inspect.getmodulename(topy(frmfile))
        # print dir(inspect.currentframe())
        if frmfile != topy(__file__):
            return sys.modules[mod]
    raise "Couldn't find calling module in stack trace"

# Natlink reloads modules when the mic is woken up
# if the file has changed, so we need to support
# code reloading.
def importOrReload(module_name, *names):
    global __importState
    
    if module_name in sys.modules:
        if (not hasattr(sys.modules[module_name], "__importState") or
            sys.modules[module_name].__importState != __importState):

            if hasattr(sys.modules[module_name], "unload") and callable(sys.modules[module_name].unload):
                sys.modules[module_name].unload()
            print 'calling reload: ' + module_name
            reload(sys.modules[module_name])
        else:
            print 'already loaded: ' + module_name
            # we have already loaded this module since last reset
    else:
        print 'calling import: ' + module_name
        __import__(module_name, fromlist=names)
        
    setattr(sys.modules[module_name], "__importState", __importState)
    callingModule = getCallingModule()
    for name in names:
        print 'setting %s on %s' % (name, str(callingModule))
        setattr(callingModule, name, getattr(sys.modules[module_name], name))
        # we only imported so we could assign, this 'unimports'
        try:
            del globals()[name]
        except KeyError:
            # could be we imported before with different
            # from list
            pass

    # absence of a from list means you did a normal 'import x', so
    # you need the module name put in your globals
    if not names:
        setattr(callingModule, module_name, sys.modules[module_name])
        # we only imported so we could assign, this 'unimports'
        try:
            del globals()[module_name]
        except KeyError:
            # don't think this should ever happen?
            raise

# modified from aenea, taken from:
# https://raw.githubusercontent.com/calmofthestorm/aenea/4b0f91ca82aa994cd4912b17cdb4ae700adc65fe/client/_aenea.py
def unloadCode():
    import natlinkmain, sys, os

    # Do not reload anything in these directories or their subdirectories.
    dir_reload_blacklist = set(["core"])

    # TODO: should only care about path ending in Natlink/Natlink/MacroSystem
    macro_dir = "E:\\NatLink\\NatLink\\MacroSystem"

    # Unload all grammars.
    natlinkmain.unloadEverything()

    # Unload all modules in macro_dir except for those in directories on the
    # blacklist.

    for name, module in sys.modules.items():
        if module and hasattr(module, "__file__"):
            # Some builtin modules only have a name so module is None or
            # do not have a __file__ attribute.  We skip these.
            path = module.__file__

            # Convert .pyc paths to .py paths.
            path = topy(path)

            # Do not unimport this module!  This will cause major problems!
            if (path.startswith(macro_dir) and
                not bool(set(path.split(os.path.sep)) & dir_reload_blacklist)
                and path != topy(os.path.abspath(__file__))):

                # if hasattr(sys.modules[name], "unload") and callable(sys.modules[name].unload):
                #     sys.modules[name].unload()
                
                # Remove the module from the cache so that it will be reloaded
                # the next time # that it is imported.  The paths for packages
                # end with __init__.pyc so this # takes care of them as well.
                del sys.modules[name]        

def reloadCode():
    import natlinkmain
    print 'Reloading code'
    unload_code()
    natlinkmain.findAndLoadFiles()
    print 'Finished reloading'
                

### DRAGONSHARE RSYNC
