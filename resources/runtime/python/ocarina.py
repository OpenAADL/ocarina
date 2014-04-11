#! /usr/bin/python
'''Python binding to the Ocarina AADL processor'''

################################################################################
import libocarina_python; # Ocarina bindings

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
    '''Load a file'''
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
def generate (generator):
    '''Generate code'''
    libocarina_python.generate (generator);

################################################################################
def main ():
    '''Test function'''
    load("rma.aadl");             # load a file
    load("deployment.aadl");      # load a file
    analyze();                    # analyze models
    instantiate("rma.erc32");     # instantiate
    generate("polyorb_hi_ada");   # generate code

if __name__ == "__main__":
    main ()
    sys.exit (0);                         # exit

################################################################################

