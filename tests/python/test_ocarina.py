#! /usr/bin/python

from ocarina import *;

def main ():
    '''Test function'''

    result = add_real_library("rma.real")      # load a file
    print "%r" % (result)
    result = load("rma.aadl")             # load a file
    print "%r" % (result)
    result = load("deployment.aadl")      # load a file
    print "%r" % (result)
    result = analyze()                    # analyze models
    print "Analysis result %r" % (result)

    result = set_real_theorem ("rma_prop_def")
    result = generate (Backends.real_theorem)
    print "REAL execution %r" % (result)
    print ""

    result = set_real_theorem ("rma_prop_def_2")
    result = generate (Backends.real_theorem)
    print "REAL execution %r" % (result)

if __name__ == "__main__":
    main ()
    sys.exit (0)                         # exit
