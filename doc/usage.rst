
.. _usage:

=====
Usage
=====

Ocarina command-line
####################

Ocarina has a rich command-line interface, covering all required steps
to parse, instantiate, analyze or generate code from AADL models::

   Usage:
      ocarina [options] files
      OR
      ocarina -help
  files are a non null sequence of AADL files

  General purpose options:
   -V  Output Ocarina version, then exit
   -s  Output Ocarina search directory, then exit

  Scenario file options:
   -b  build the generated application code
   -z  clean the generated application code
   -ec execute the generated application code and
       retrieve coverage information
   -er execute the generated application code and
       verify that there is no regression
   -p  only parse and instantiate the application model
   -c  only perform schedulability analysis

  Advanced user options:
   -d  Debug mode for developpers
   -q  Quiet mode (default)
   -t  [script] Run Ocarina in terminal interactive mode.
       If a script is given, interpret it then exit.
   -v  Verbose mode for users
   -x  Parse AADL file as an AADL scenario file
   -f  Parse predefined non standard property sets
   -i  Instantiate the AADL model
   -r  <name> The name of the instance tree root
   -o  Specify output file
   -I  Specify the inclusion paths
   -aadlv1  Use AADL v1 standard (default)
   -aadlv2  Use AADL v2 standard
   -real_lib Add a REAL file to be used as a theorem libraries by REAL annexes
   -g  Generate code from the AADL instance tree
       Registered backends:
        petri_nets
        boundt
        polyorb_hi_ada
        polyorb_qos_ada
        polyorb_hi_c
        polyorb_hi_rtsj
        pok_c
        stats
        subprograms
        real_theorem
        carts
        cheddar
        aadl
        aadl_min
        aadl_annex
        behavior_specification
        real_specification
   -arinc653  Generate code for ARINC653 API (POK backend only)
   -b  Generate and build code from the AADL model
   -z  Clean code generated from the AADL model
   -disable-annexes=@{annexes@}  Desactive one or all annexes
       Annexes :
        all
        behavior
        real

ocarina-config
##############

ocarina-config returns path and library information on Ocarina
installation. This script can be used to compile user program that
uses Ocarina's API.::

 Usage: ocarina-config [OPTIONS]
 Options:
        No option:
            Output all the flags (compiler and linker) required
            to compile your program.
        [--prefix[=DIR]]
            Output the directory in which Ocarina architecture-independent
           files are installed, or set this directory to DIR.
        [--exec-prefix[=DIR]]
            Output the directory in which Ocarina architecture-dependent
           files are installed, or set this directory to DIR.
        [--version|-v]
            Output the version of Ocarina.
        [--config]
            Output Ocarina's configuration parameters.
        [--runtime[=<Runtime_Name>]]
            Checks the validity and the presence of the given runtime and
            then, outputs its path. Only one runtime can be requested at
            a time. If no runtime name is given, outputs the root directory
            of all runtimes.
        [--libs]
            Output the linker flags to use for Ocarina.
        [--properties]
            Output the location of the standard property file.
        [--resources]
            Output the location of resource files
            (typically the standard properties)
        [--cflags]
            Output the compiler flags to use for Ocarina.
        [--help]
            Output this message



