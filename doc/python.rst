Python bindings for Ocarina
===========================

Ocarina Python bindings
#######################

Ocarina proposes Python bindings to its internal APIs. This binding is
available if configured properly, first at compile-time, then at
run-time.

At compile time, Ocarina must be configured with shared libraries
support. Refer to the :ref:`installation`;

At run-time, the following environment variables must be set up::

  % export PATH=`ocarina-config --prefix`/bin:$PATH
  % export OCARINA_PATH=`ocarina-config --prefix`
  % export LD_LIBRARY_PATH=$OCARINA_PATH/lib:$LD_LIBRARY_PATH
  % export PYTHONPATH=$OCARINA_PATH/include/ocarina/runtime/python:$OCARINA_PATH/lib:$PYTHONPATH

Example
#######

The following example illustrates the capabilities of the Python API,
it implements a visitor that iterates of the AADL model elements:

.. literalinclude:: ../resources/runtime/python/test/visitor.py
   :language: python

Python API description
######################

The following lists all functions defined in the `ocarina` module

.. automodule:: ocarina.ocarina
   :members:

.. automodule:: ocarina.lmp
   :members:
