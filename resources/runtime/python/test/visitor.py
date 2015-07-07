#! /usr/bin/python

import ocarina;
import sys;

def visitor (component, level):
    print ' ' * level,'Visiting ',ocarina.getInstanceName(component)[0]

    features=ocarina.AIN.Features(component)[0];
    if features is not None :
        print ' ' * level,' -> features:',features
        for feature in features :
            print ' ' * level,'   -> ',feature,",",ocarina.getInstanceName(feature)[0]

    properties = ocarina.AIN.Properties(component)[0];
    if properties is not None :
        print ' ' * level,' -> properties:'
        for property in properties:
            print ' ' * level,'   ',ocarina.getPropertyValue(component,property)[0]

    subcomponents=ocarina.AIN.Subcomponents(component)[0];
    if subcomponents is not None :
        print ' ' * level,' -> subcomponents:',subcomponents
        for subcomponent in subcomponents :
            print ' ' * level,'   -> ',subcomponent,",",ocarina.getInstanceName(subcomponent)[0]
            visitor(str(ocarina.AIN.Corresponding_Instance(subcomponent)[0]),level+3)

    print ' ' * level,'end of visit of ',component

def main ():
    '''Test function'''

    err=ocarina.load("rma.aadl");                   # load a file
    err=ocarina.analyze();                          # analyze models
    err=ocarina.instantiate("rma.erc32");           # instantiate system

    print '----------------------------------------------------'
    print 'Visit AADL Instance tree'
    print '----------------------------------------------------'

    root=ocarina.getRoot()[0]
    visitor(root,0)

if __name__ == "__main__":
    main ()
    sys.exit (0);                       # exit
