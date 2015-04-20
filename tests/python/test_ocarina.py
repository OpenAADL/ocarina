#! /usr/bin/python

from ocarina import *;

def main ():
    '''Test function'''

    for backends in Backends:
        print(backends);

    load("rma.aadl");             # load a file
    load("deployment.aadl");      # load a file
    result = analyze();                    # analyze models
    print "Analysis result %r" % (result);
    instantiate("rma.erc32");     # instantiate
    generate (Backends.polyorb_hi_ada);

    reset();

    load("rma.aadl");             # load a file
    load("deployment.aadl");      # load a file
    analyze();                    # analyze models
    instantiate("rma.erc32");     # instantiate
    generate (Backends.polyorb_hi_ada);

if __name__ == "__main__":
    main ()
    sys.exit (0);                         # exit
