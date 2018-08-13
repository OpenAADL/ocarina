#! /usr/bin/python
'''
:mod:`lmp` -- Port of Ellidiss LMP to Ocarina Python API
========================================================

.. moduleauthor:: Jerome Hugues, Arnaud Schach


This module is an adaptation of Ellidiss LMP "Logical Model Processing"
to Python.

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

################################################################################

def getPackages ():
    '''Return the list of all the packages defined in the current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getPackages)

################################################################################

def getImportDeclarations ():
    '''Return the list of all the import declarations used in the
    current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getImportDeclarations)

################################################################################

def getAliasDeclarations ():
    '''Return the list of all the alias declaration defined in the
    current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getAliasDeclarations)

################################################################################

def getComponentTypes (category):
    '''Return a list of component types defined in the current AADL project

    :param category: one of the AADL category defined in the standard

    For instance, to retrieve all the system types from the current project,
    you may use the following

    >>> getComponentTypes (System)
    '''

    return runOcarinaFunction (libocarina_python.getComponentTypes, category)

################################################################################

def getComponentImplementations (category):
    '''Return a list of component implementations defined in the
    current AADL project

    :param category: one of the AADL category defined in the standard

    For instance, to retrieve all the system implementations from the
    current project, you may use the following

    >>> getComponentImplementations (System)
    '''
    return runOcarinaFunction (libocarina_python.getComponentImplementations, category)

################################################################################

def getAnnexes ():
    '''Return the list of all the annexes defined in the current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getAnnexes)

################################################################################

def getPrototypes ():
    '''Return the list of all the prototypes defined in the current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getPrototypes)

################################################################################

def getPrototypeBindings ():
    '''Return the list of all the prototype bindings defined in the
    current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getPrototypeBindings)

################################################################################

def getFlowSpecifications ():
    '''Return the list of all the flow specification defined in the
    current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getFlowSpecifications)

################################################################################

def getFlowImplementations ():
    '''Return the list of all the flow implementation defined in the
    current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getFlowImplementations)

################################################################################

def getModes ():
    '''Return the list of all the modes defined in the current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getModes)

################################################################################

def getModeTransitions ():
    '''Return the list of all the mode transition defined in the
    current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getModeTransitions)

################################################################################

def getInModes ():
    '''Return the list of all the in mode used in the current AADL project
    '''


################################################################################

def getPropertySets ():
    '''Return the list of all the property set defined in the
    current AADL project
    '''

    return runOcarinaFunction (libocarina_python.getPropertySets)

################################################################################

def getPropertyTypes (propertySetId):
    '''Return the list of all the property types defined in the
    provided property set

    :param propertySetId: the nodeId of the property set in the
                          current AADL project to search in

    For instance, to retrieve all the property types from property
    set propertySet, retrieve its id (propertySetId) and use the following

    >>> getPropertyTypes (propertySetId)
    '''

    return runOcarinaFunction (libocarina_python.getPropertyTypes, propertySetId)

################################################################################

def getPropertyDefinitions (propertySetId):
    '''Return the list of all the property declaration defined in the
    provided property set

    :param propertySetId: the nodeId of the property set in the
                          current AADL project to search in

    For instance, to retrieve all the property declaration from
    property set propertySet, retrieve its id (propertySetId)
    and use the following

    >>> getPropertyDefinitions (propertySetId)
    '''

    return runOcarinaFunction (libocarina_python.getPropertyDefinitions, propertySetId)

################################################################################

def getPropertyConstants (propertySetId):
    '''Return the list of all the constant property defined in the
    provided property set

    :param propertySetId: the nodeId of the property set in the
                          current AADL project to search in

    For instance, to retrieve all the constant property from property
    set propertySet, retrieve its id (propertySetId) and use the following

    >>> getPropertyConstants (propertySetId)

    '''

    return runOcarinaFunction (libocarina_python.getPropertyConstants,propertySetId)

################################################################################

def getInstances (category):
    '''Return a list of instances defined in the current AADL project

    :param category: one of the AADL category defined in the standard

    For instance, to retrieve all the system instances from the current project,
    you may use the following

    >>> getInstances (System)
    '''

    return runOcarinaFunction (libocarina_python.getInstances, category)

################################################################################

def getComponentName (nodeId):
    '''Get the name of an AADL component

    :param nodeId: the id of the component whose name is searched

    For instance, to retrieve the name of MyComponent,
    retrieve its id (nodeId) and use the following

    >>> getComponentName (nodeId)
    '''

    return runOcarinaFunction (libocarina_python.getComponentName, nodeId)

################################################################################

def getComponentFullname (nodeId):
    '''Get the full qualified name of an AADL component

    :param nodeId: the id of the component whose full qualified name is searched

    For instance, to retrieve the full qualified name of MyComponent,
    retrieve its id (nodeId) and use the following

    >>> getComponentFullname (nodeId)
    '''

    return runOcarinaFunction (libocarina_python.getComponentFullname, nodeId)

################################################################################

def getInstanceName (nodeId):
    '''Get the name of an AADL instance

    :param nodeId: the id of the instance whose name is searched

    For instance, to retrieve the name of MyInstance,
    retrieve its id (nodeId) and use the following

    >>> getInstanceName (nodeId)
    '''

    return runOcarinaFunction (libocarina_python.getInstanceName, nodeId)

################################################################################

def getNodeId (name):
    '''Get the Id of a component from its name

    :param name: the AADL name of the node whose id is queried

    For instance, to retrieve the id of MyHome, you may use the following

    >>> getNodeId (MyHome)
    '''

    return runOcarinaFunction (libocarina_python.getNodeId, name)

################################################################################

def getRoot ():
    '''Get the Id of the current root instantiated model
    '''

    return runOcarinaFunction (libocarina_python.getRoot)
