Ocarina AADL toolset  |docs| 
====================

About: 
------

Ocarina is an AADL model processor, it acts as a "compiler" for AADL models.

As a front-end, it supports

* AADLv2 language from `AS5506C standard <http://standards.sae.org/as5506c/>`_, 
* the ARINC653 and the EMV2 Annex from `AS5506/1A standard <http://standards.sae.org/as5506/1a/>`_

As a back-end, it supports

* Code generation towards the AADL runtime `PolyORB-HI/Ada <https://github.com/OpenAADL/polyorb-hi-ada>`_ and `PolyORB-HI/C <https://github.com/OpenAADL/polyorb-hi-c>`_, and ARINC653 compliant APEX.
* WCET analysis, using `Bound-T <http://bound-t.com>`_
* Scheduling analysis, using `Cheddar <http://beru.univ-brest.fr/~singhoff/cheddar/>`_ and `MAST <http://mast.unican.es>`_
* Model checking, using Petri Net `Tina <http://projects.laas.fr/tina//>`_, and `LNT <http://cadp.inria.fr>`
* Constraint analysis, using the REAL annex language

It can be integrated with the `AADLib <https://github.com/OpenAADL/AADLib>`_ library of AADL components, and through a `OSATE2 plugin <https://github.com/OpenAADL/osate2-ocarina>`_

Installation:
-------------

Refer to the ocarina-build companion project for `details <https://github.com/openaadl/ocarina-build>`_

Q&A:
----

Build status for Linux and OS X: |build-status|

Code coverage: |coverage|

.. |build-status| image:: https://travis-ci.org/OpenAADL/ocarina.svg?branch=master 
  :target: https://travis-ci.org/OpenAADL/ocarina

.. |docs| image:: https://readthedocs.org/projects/docs/badge/?version=latest
    :alt: Documentation Status
    :scale: 100%
    :target: http://ocarina.readthedocs.org/

.. |coverage| image:: https://codecov.io/github/OpenAADL/ocarina/coverage.svg?branch=master
  :target: https://codecov.io/gh/OpenAADL/ocarina
  :alt: Code Coverage
