.. _scenariofiles:

==============
Scenario files
==============

AADL scenario files are a very simple way to set build options when
using Ocarina. AADL scenario may consist of more than one AADL file.

Scenario files rely on the system component category to configure all
elements of the system to be processed. The following scenario file
illustrates this feature. It extends an existing scenario file (see
below) with project-specific configuration data:

.. literalinclude:: source/scenario.aadl
   :language: aadl

Scenario files rely on specific properties:

* property :code:`AADL_Files` lists all files that are part of the system;

* property :code:`Root_System_Name` is the name of the Root System;

* property :code:`Generator` is the name of the generator (or back-end) to use

.. note:: The definition of scenario-specific properties may be found in section :ref:`ocarina_config`.

:command:`ocarina -x <scenario_file.aadl>` will cause the scenario
file to be processed. In addition, the flag :code:`-b` will
compile generated source files.

:file:`ocarina_library.aadl`
############################

.. literalinclude:: ../resources/AADLv2/ocarina_library.aadl
   :language: aadl
