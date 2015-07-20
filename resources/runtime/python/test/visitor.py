#!/usr/bin/env python

# visitor.py: A simple script to visit all nodes of an AADL instance tree
#
# Note: this scripts require the docopt package

"""visitor.py: A simple script to visit all nodes of an AADL instance tree

Usage: visitor.py FILE
       visitor.py ( -h | --help )
       visitor.py --version

Arguments:
    FILE  AADL file to process

Options:
    -h, --help  Help information
    --version   Version information

"""

from docopt import docopt
import sys

###################################
args = sys.argv[1:]
# XXX: we have to do a back-up of the command-line arguments prior to
# importing Ocarina, as its initialization phase will erase sys.argv
# To be investigated, see github issue #45 for details
###################################

import ocarina
import lmp

def visitor (component, level):
    """
    This function visits an AADL component, and prints information
    about its features, subcomponents and properties.

    Args:
    component (str):  the NodeId of the component
    level (int): indentation level

    """

    print ' ' * level,'Visiting ',lmp.getInstanceName(component)[0]

    features=ocarina.AIN.Features(component)[0];
    if features is not None :
        print ' ' * level,' -> features:',features
        for feature in features :
            print ' ' * level,'   -> ',feature,",",lmp.getInstanceName(feature)[0]

    properties = ocarina.AIN.Properties(component)[0];
    if properties is not None :
        print ' ' * level,' -> properties:'
        for property in properties:
            print ' ' * level,'   ',ocarina.getPropertyValue(component,property)[0]
            print '[looking for priority] ' * level,'   ',ocarina.getPropertyValueByName(component,u'priority')[0]

    subcomponents=ocarina.AIN.Subcomponents(component)[0];
    if subcomponents is not None :
        print ' ' * level,' -> subcomponents:',subcomponents
        for subcomponent in subcomponents :
            print ' ' * level,'   -> ',subcomponent,",",lmp.getInstanceName(subcomponent)[0]
            visitor(str(ocarina.AIN.Corresponding_Instance(subcomponent)[0]),level+3)

    print ' ' * level,'end of visit of ',component

def main ():
    # read  command line arguments

    arguments = docopt(__doc__, args, version="visitor.py 0.1")

    # build the repository path
    repo = arguments['FILE']

    err = ocarina.load(repo);       # load a file
    err = ocarina.analyze();        # analyze models
    err = ocarina.instantiate("");  # instantiate system

    print '----------------------------------------------------'
    print 'Visit AADL Instance tree'
    print '----------------------------------------------------'

    root=lmp.getRoot()[0]
    visitor(root,0)

if __name__ == "__main__":
    main ()
