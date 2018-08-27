.. _usage:

=====
Usage
=====

.. _ocarinacli:

Ocarina command-line
####################

Ocarina has a rich command-line interface, covering all required steps
to parse, instantiate, analyze or generate code from AADL models.

.. program:: ocarina

.. option::  -h, --help

             Display help and exit

.. option:: --version

            Display version and exit

.. option:: -v, --verbose

            Output extra verbose information

.. option:: -q

            Quiet mode (default)

.. option:: -d

            Debug mode

.. option:: -s

            Output default search directory, then exit

.. option:: -aadlv[ARG]

            AADL version, ARG = 1 for AADL 1.0, 2 for AADL 2.x

.. option:: -f

            Parse predefined non-standard property sets

.. option:: -disable-annexes=ARG

            Deactivate annex ARG

.. option:: -r ARG

            Use ARG as root system

.. option:: -o ARG

            Specify output file/directory

.. option:: -y

            Automatically load AADL files

.. option:: -I ARG

            Add ARG to the directory search list

.. option:: -p

            Parse and instantiate the model

.. option:: -i

            Instantiate the model

.. option:: -x

            Parse AADL file as an AADL scenario file

.. option:: -g ARG

            Generate code using Ocarina backend 'ARG'

.. option:: --list-backends

            List available backends

.. option:: --spark2014

            Generate SPARK2014 annotations

.. option:: -b

            Compile generated code

.. option:: -z

            Clean code generated

.. option:: -k ARG

            Set POK flavor (arinc653/deos/pok/vxworks)

.. option:: -t

            Run Ocarina in terminal interactive mode

.. option:: -real_theorem ARG

            Name of the main REAL theorem to evaluate

.. option:: -real_lib ARG

            Add external library of REAL theorems

.. option:: -real_continue_eval

            Continue evaluation of REAL theorems after first failure (REAL backend)

.. option:: -boundt_process ARG

            Generate .tpo file for process ARG (Bound-T backend)

.. option:: -ec

            Compute coverage metrics

.. option:: -er

            Execute system

.. option:: -asn1

            Generate ASN1 deployment file (PolyORB-HI-C only)

.. option:: -perf

            Enable profiling with gprof (PolyORB-HI-C only)


.. note:: A man page is also installed in Ocarina installation path, in :file:`$OCARINA_PATH/share/man/man1/`.

ocarina-config
##############

ocarina-config returns path and library information on Ocarina
installation. This script can be used to compile user program that
uses Ocarina's API.

.. include:: ocarina_config.txt
   :literal:
