#! /usr/bin/python

import OcarinaPython as ocarina;

def visitor (component,level) :
    subcomponents=ocarina.AIN.Subcomponents(component);
    if subcomponents is not None :
       prefix=''
       while ( len(prefix)<level ) :
          prefix=prefix+'.'
       for child in subcomponents :
          print prefix,ocarina.getInstanceName(child)[0]
          visitor(str(ocarina.AIN.Corresponding_Instance(child)),level+1)

def main ():
    '''Test function'''

    err=ocarina.load("rma.aadl");                   # load a file
    print 'load("rma.aadl")'
    if err[1].strip()!='':
      print 'info message: \n', err[1]
    if err[2]!=[]:
      print 'warning message: \n', err[2]
    if err[3]!=[]:
      print 'error message: \n', err[3]
      sys.exit(2)
      
    err=ocarina.load("deployment.aadl");            # load a file
    print 'load("deployment.aadl")'
    if err[1].strip()!='':
      print 'info message: \n', err[1]
    if err[2]!=[]:
      print 'warning message: \n', err[2]
    if err[3]!=[]:
      print 'error message: \n', err[3]
      sys.exit(2)
      
    err=ocarina.analyze();                          # analyze models
    print 'ocarina.analyze()'
    if err[1].strip()!='':
      print 'info message: \n', err[1]
    if err[2]!=[]:
      print 'warning message: \n', err[2]
    if err[3]!=[]:
      print 'error message: \n', err[3]
      sys.exit(2)
      
    err=ocarina.instantiate("rma.erc32");           # instantiate system
    print 'ocarina.instantiate("rma.erc32")'
    if err[1].strip()!='':
      print 'info message: \n', err[1]
    if err[2]!=[]:
      print 'warning message: \n', err[2]
    if err[3]!=[]:
      print 'error message: \n', err[3]
      sys.exit(2)
    
    print '----------------------------------------------------'
    print 'Number of Component Instances:'
    print '----------------------------------------------------'
    root=ocarina.getInstances('all')[0][0]
    # root=ocarina.getRoot()
    print ocarina.getInstanceName(root)[0]
    visitor(root,1)

if __name__ == "__main__":
    main ()
    sys.exit (0);                       # exit
