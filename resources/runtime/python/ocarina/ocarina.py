#! /usr/bin/python
'''
:mod:`ocarina` -- Python binding to the Ocarina AADL processor
==============================================================

.. moduleauthor:: Jerome Hugues, Arnaud Schach

This module provides direct access to top-level functions of Ocarina
to load, parse, instantiate AADL models, and to invoke backends.

'''

################################################################################

try:
    import libocarina_python # Ocarina bindings
    import ocarina_me_aadl_aadl_instances_nodes as AIN
    import ocarina_me_aadl_aadl_tree_nodes as ATN
    from ocarina_common_tools import *
    import io
except ImportError:
    pass

class Enum(tuple): __getattr__ = tuple.index

################################################################################
def version ():
    '''Print Ocarina version'''
    libocarina_python.version()

################################################################################
def status ():
    '''Print Ocarina status'''
    libocarina_python.status()

################################################################################
def reset ():
    '''Reset Ocarina internal state

    **Note:** this function must be called before processing a new set of
    models.'''

    libocarina_python.reset()

################################################################################
def load (filename):
    '''Load a file

    :param filename: name of the file to be loaded, using Ocarina search path
    :type filename: string

    E.g. to load "foo.aadl":

    >>> load("foo.aadl")

    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            libocarina_python.load (filename)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################
def analyze ():
    '''Analyze models'''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.analyze ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################
def instantiate (root_system):
    '''Instantiate model, starting from root_system

    :param root_system: name of the root system to instantiate
    :type root_system: string

    '''

    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.instantiate (root_system)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################
Backends = Enum ([ "polyorb_hi_ada", "polyorb_hi_c"])
'''List of supported backends, used by :data:`generate`'''
# Note, this list should match backend names as specified by Ocarina CLI

def generate (generator):
    '''Generate code

    :param generator: one supported backends, from :data:`Backends`

    For instance, to use the PolyORB-HI/Ada backend, you may use the following

    >>> generate (Backends.polyorb_hi_ada)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            libocarina_python.generate (Backends[generator])
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getPackages ():
    '''Return the list of all the packages defined in the current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getPackages()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getImportDeclarations ():
    '''Return the list of all the import declaration used in the
    current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getImportDeclarations()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getAliasDeclarations ():
    '''Return the list of all the alias declaration defined in the
    current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getAliasDeclarations ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getComponentTypes (category):
    '''Return a list of component types defined in the current AADL project

    :param category: one of the AADL category defined in the standard

    For instance, to retrieve all the system types from the current project,
    you may use the following

    >>> getComponentTypes (System)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getComponentTypes (category)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getComponentImplementations (category):
    '''Return a list of component implementations defined in the
    current AADL project

    :param category: one of the AADL category defined in the standard

    For instance, to retrieve all the system implementations from the
    current project, you may use the following

    >>> getComponentImplementations (System)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getComponentImplementations (category)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getAnnexes ():
    '''Return the list of all the annexes defined in the current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getAnnexes ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getPrototypes ():
    '''Return the list of all the prototypes defined in the current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getPrototypes ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getPrototypeBindings ():
    '''Return the list of all the prototype bindings defined in the
    current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getPrototypeBindings ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getFlowSpecifications ():
    '''Return the list of all the flow specification defined in the
    current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getFlowSpecifications ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getFlowImplementations ():
    '''Return the list of all the flow implementation defined in the
    current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getFlowImplementations ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getModes ():
    '''Return the list of all the modes defined in the current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getModes ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getModeTransitions ():
    '''Return the list of all the mode transition defined in the
    current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getModeTransitions ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getInModes ():
    '''Return the list of all the in mode used in the current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getInModes ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getPropertySets ():
    '''Return the list of all the property set defined in the
    current AADL project
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getPropertySets ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getPropertyTypes (propertySetId):
    '''Return the list of all the property types defined in the
    provided property set

    :param propertySetId: the nodeId of the property set in the
    current AADL project to serach in

    For instance, to retrieve all the property types from property
    set propertySet, retrieve its id (propertySetId) and use the following

    >>> getPropertyTypes (propertySetId)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getPropertyTypes (propertySetId)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getPropertyDefinitions (propertySetId):
    '''Return the list of all the property declaration defined in the
    provided property set

    :param propertySetId: the nodeId of the property set in the
    current AADL project to serach in

    For instance, to retrieve all the property declaration from
    property set propertySet, retrieve its id (propertySetId)
    and use the following

    >>> getPropertyDefinitions (propertySetId)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getPropertyDefinitions (propertySetId)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getPropertyConstants (propertySetId):
    '''Return the list of all the constant property defined in the
    provided property set

    :param propertySetId: the nodeId of the property set in the
    current AADL project to serach in

    For instance, to retrieve all the constant property from property
    set propertySet, retrieve its id (propertySetId) and use the following

    >>> getPropertyConstants (propertySetId)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getPropertyConstants (propertySetId)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getInstances (category):
    '''Return a list of instances defined in the current AADL project

    :param category: one of the AADL category defined in the standard

    For instance, to retrieve all the system instances from the current project,
    you may use the following

    >>> getInstances (System)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getInstances (category)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getComponentName (nodeId):
    '''Get the name of an AADL component

    :param nodeId: the id of the component whose name is searched

    For instance, to retrieve the name of MyComponent,
    retrieve its id (nodeId) and use the following

    >>> getComponentName (nodeId)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getComponentName (nodeId)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getComponentFullname (nodeId):
    '''Get the full qualified name of an AADL component

    :param nodeId: the id of the component whose
    full qualified name is searched

    For instance, to retrieve the full qualified name of MyComponent,
    retrieve its id (nodeId) and use the following

    >>> getComponentFullname (nodeId)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getComponentFullname (nodeId)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getInstanceName (nodeId):
    '''Get the name of an AADL instance

    :param nodeId: the id of the instance whose name is searched

    For instance, to retrieve the name of MyInstance,
    retrieve its id (nodeId) and use the following

    >>> getInstanceName (nodeId)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getInstanceName (nodeId)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getNodeId (name):
    '''Get the Id of a component from its name

    :param name: the AADL name of the node whose id is queried

    For instance, to retrieve the id of MyHome, you may use the following

    >>> getNodeId (MyHome)
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getNodeId (name)
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]

################################################################################

def getRoot ():
    '''Get the Id of the current root instantiated model
    '''
    info = io.BytesIO()
    error = io.BytesIO()
    raisedError = []
    res = ''
    with std_redirector(info,error):
        try:
            res = libocarina_python.getRoot ()
        except:
            raisedError.append(getErrorMessage())
    stderrMsg = sortStderrMessages(error.getvalue().decode('utf-8'))
    if stderrMsg[1]!=[]:
        raisedError.append(stderrMsg[1])
    return [ res , info.getvalue().decode('utf-8'), stderrMsg[0] ,
        raisedError ]
