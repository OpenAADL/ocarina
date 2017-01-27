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
    return runOcarinaFunction (libocarina_python.load, filename)

################################################################################
def analyze ():
    '''Analyze models'''

    return runOcarinaFunction (libocarina_python.analyze)

################################################################################
def instantiate (root_system):
    '''Instantiate model, starting from root_system

    :param root_system: name of the root system to instantiate
    :type root_system: string

    '''

    return runOcarinaFunction (libocarina_python.instantiate, root_system)

################################################################################
def set_real_theorem (theorem_name):
    '''Set main REAL theorem

    :param theorem_name: name of the theorem
    :type theorem_name: string

    '''

    return runOcarinaFunction (libocarina_python.set_real_theorem, theorem_name)

################################################################################
def add_real_library (libraryname):
    '''

    :param libraryname: name of the REAL library file to include
    :type libraryname: string

    '''

    return runOcarinaFunction (libocarina_python.add_real_library, libraryname)

################################################################################
Backends = Enum ([ "polyorb_hi_ada", "polyorb_hi_c", "real_theorem"])
'''List of supported backends, used by :data:`generate`'''
# Note, this list should match backend names as specified by Ocarina CLI

def generate (generator):
    '''Generate code

    :param generator: one supported backends, from :data:`Backends`

    For instance, to use the PolyORB-HI/Ada backend, you may use the following

    >>> generate (Backends.polyorb_hi_ada)
    '''

    return runOcarinaFunction (libocarina_python.generate, Backends[generator])

################################################################################

def getPropertyValue (nodeId,propertyId):
    '''Get the value of the property
    '''
    return runOcarinaFunction (libocarina_python.getPropertyValue, nodeId,propertyId)

################################################################################

def getPropertyValueByName (nodeId,propertyString):
    '''Get the value of the property propertyString applied to model
       element nodeId.
    '''
    return runOcarinaFunction (libocarina_python.getPropertyValueByName, nodeId, propertyString)

################################################################################

def getSourcePorts (feature_nodeId):
    '''Get the source port associated to the feature_nodeId passed as
       parameter, in the case feature_nodeId participates in a
       connection.
    '''
    return runOcarinaFunction (libocarina_python.getSourcePorts, feature_nodeId)

################################################################################

def getDestinationPorts (nodeId):
    '''Get the destination port associated to the feature_nodeId passed as
       parameter, in the case feature_nodeId participates in a
       connection.
    '''

    return runOcarinaFunction (libocarina_python.getDestinationPorts, nodeId)
