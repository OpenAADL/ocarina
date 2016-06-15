#! /usr/bin/python

import ocarina
import lmp

def main ():
    '''Test function'''

    err=ocarina.load("rma.aadl");                   # load a file
    print 'load("rma.aadl")'
    print err
    if err[1] != None:
      print 'info message: ', err[1]
    if err[2]!=[]:
      print 'warning message: ', err[2]
    if err[3]!=[]:
      print 'error message: ', err[3]
      sys.exit(2)

    err=ocarina.load("deployment.aadl");            # load a file
    print 'load("deployment.aadl")'
    if err[1] != None:
      print 'info message: ', err[1]
    if err[2]!=[]:
      print 'warning message: ', err[2]
    if err[3]!=[]:
      print 'error message: ', err[3]
      sys.exit(2)

    err=ocarina.analyze();                          # analyze models
    print 'ocarina.analyze()'
    if err[1] != None:
      print 'info message: ', err[1]
    if err[2]!=[]:
      print 'warning message: ', err[2]
    if err[3]!=[]:
      print 'error message: ', err[3]
      sys.exit(2)

    err=ocarina.instantiate("rma.impl");           # instantiate system
    print 'ocarina.instantiate("rma.impl")'
    if err[1] != None:
      print 'info message: ', err[1]
    if err[2]!=[]:
      print 'warning message: ', err[2]
    if err[3]!=[]:
      print 'error message: ', err[3]
      sys.exit(2)

if __name__ == "__main__":
    main ()
    sys.exit (0);                       # exit
