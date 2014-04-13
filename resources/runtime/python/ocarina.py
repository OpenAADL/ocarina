#! /usr/bin/python
'''Python binding to the Ocarina AADL processor'''

################################################################################
import libocarina_python; # Ocarina bindings

class Enum(tuple): __getattr__ = tuple.index

################################################################################
def version ():
    '''Print Ocarina version'''
    libocarina_python.version();

################################################################################
def status ():
    '''Print Ocarina status'''
    libocarina_python.status();

################################################################################
def load (filename):
    '''Load a file

    :param filename: name of the file to be loaded, following Ocarina search path
    :type filename: string

    E.g. to load "foo.aadl":

    >>> load("foo.aadl");

    '''
    libocarina_python.load (filename);

################################################################################
def analyze ():
    '''Analyze models'''
    libocarina_python.analyze ();

################################################################################
def instantiate (root_system):
    '''Instantiate models'''
    libocarina_python.instantiate (root_system);

################################################################################
Backends = Enum ([ "polyorb_hi_ada", "polyorb_hi_c"]);
'''Supported backends'''

def generate (generator):
    '''Generate code

    :param generator: one supported backends, from :data:`Backends`

    For instance, to use the PolyORB-HI/Ada backend, you may use the following

    >>> generate (Backends.polyorb_hi_ada);
    '''
    libocarina_python.generate (Backends[generator]);

################################################################################

