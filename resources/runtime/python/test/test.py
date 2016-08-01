#! /usr/bin/python

import ocarina
import lmp

def main ():
    '''Test function'''

    err=ocarina.load("rma.aadl");                   # load a file
    print 'load("rma.aadl"),:', err

    err=ocarina.load("deployment.aadl");            # load a file
    print 'load("deployment.aadl"):', err

    err=ocarina.analyze();                          # analyze models
    print 'ocarina.analyze():', err

    err=ocarina.instantiate("rma.impl");           # instantiate system
    print 'ocarina.instantiate("rma.impl"):', err

    err=ocarina.add_real_library("rma.real");
    print err;

    err=ocarina.set_real_theorem("check_scheduling");
    print err;

    err=ocarina.generate (ocarina.Backends.real_theorem);
    print err;

if __name__ == "__main__":
    main ()
    sys.exit (0);                       # exit
