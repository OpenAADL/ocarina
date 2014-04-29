Python bindings for Ocarina
===========================

Ocarina Python bindings
#######################

Ocarina proposes Python bindings to its internal APIs. This binding is
available if configured properly, first at compile-time, then at
run-time.

* At compile time, Ocarina must be configured with shared libraries
  support. Refer to the :ref:`installation`;
* At run-time, the following environment variables must be set up::

  % export OCARINA_PATH=`ocarina-config --prefix`
  % export LD_LIBRARY_PATH=$OCARINA_PATH/lib:$LD_LIBRARY_PATH
  % export PYTHONPATH=$OCARINA_PATH/include/ocarina/runtime/python:$OCARINA_PATH/lib:$PYTHONPATH

Example
#######

Here is a small examples illustrating Python API capabilities::

     #! /usr/bin/python

     from ocarina import *;

     def main ():
         '''Test function'''

         # Print all registered backends
         for backends in Backends:
             print(backends);

         load("rma.aadl");                   # load a file
         load("deployment.aadl");            # load a file
         analyze();                          # analyze models
         instantiate("rma.erc32");           # instantiate system
         generate (Backends.polyorb_hi_ada); # generate code using the PolyORB/HI-Ada backend

     if __name__ == "__main__":
         main ()
         sys.exit (0);                       # exit

Python API description
######################

The following lists all functions defined in the `ocarina` module

.. automodule:: ocarina
   :members:
