.. _polyorb-hi-c:

############
PolyORB-HI/C
############

About
#####

PolyORB-HI/C is a middleware for High-Integrity Systems, it inherits
most concepts of the schizophrenic middleware *PolyORB* while being
based on a complete new source code base, compatible with the
Ravenscar profile and the restrictions for High-Integrity systems.

PolyORB-HI/C acts as an execution runtime for the AADL language. In
this context, Ocarina acts as a compiler, turning an AADL model into C
code that uses low-level constructs provided by PolyORB-HI/C.

The generated code is in charge of allocating all required ressources
(threads, buffers, message queue), configure communication stacks,
marshallers and concurrency structures.

Supported Platforms
###################

PolyORB-HI-C has been compiled and sucessfully tested on

Native platforms

- Linux
- Mac OS X
- FreeBSD
- Windows

Embedded platforms

- AIR Hypervisor
- FreeRTOS (alpha stage)
- RTEMS
- Xenomai
- XtratuM

.. note:: when using RTEMS operating system, you have to define the :command:`RTEMS_MAKEFILE_PATH` environment variable. See RTEMS documentation for more details.

Tree structure
##############

PolyORB-HI-C source directory has the following tree structure:

- :file:`doc/`: documentation,
- :file:`examples/`: set of examples to test PolyORB-HI-C
- :file:`share/`: common files (aadl files used by Ocarina, makefiles, ...)
- :file:`src/`: core of PolyORB-HI
- :file:`src/drivers`: device drivers supported by PolyORB-HI-C
- :file:`tools/`: some script to handle the packaging and a verification tool to check if the binaries are compliant with the POSIX restrictions
- :file:`COPYING3` and :file:`COPYING.RUNTIME`: licence information
- :file:`README`: short description of the distribution

When installed with Ocarina, in :file:`$OCARINA_PATH` directory:

- documentation is in :file:`$OCARINA_PATH/share/doc/ocarina`
- examples are in :file:`$OCARINA_PATH/examples/ocarina/polyorb-hi-c/`
- runtime files are in :file:`$OCARINA_PATH/include/ocarina/runtime/polyorb-hi-c/`

Generating code from an AADL model
##################################

To build your own system, you have to select the PolyORB-HI/C backend,
either using a scenario file or the command line.

- To use a scenario file, refer to :ref:`scenariofiles`

- To use command line, you have to select the :file:`polyorb_hi_c` backend, e.g. by using the command :command:`ocarina -g polyorb_hi_c <list-of-aadl-files>`. Refer to :ref:`ocarinacli` for more details .

Code generation towards PolyORB-HI/C
####################################

The :code:`ping` example
------------------------

In the following, and for each component of the distributed
application, we give the AADL entities that are used to model this
component, and then the transformation rules used by the code
generator to generate C code from these entities.

The mapping rules will be illustrated using the following simple
example of a distributed application:

@image{fig/ping, 12cm}

The figure above shows the architecture of the :code:`ping` example: a
client, which is a process containing one single *periodic* thread,
sends a message to the server which is a process containing one
*sporadic* thread that handles incoming ping messages from the
client. Each node of the :code:`ping` application runs on a different
machine.

In this chapter, we first present the AADL modeling patterns used to
define a distributed application. Then, we give the rules applied to
map AADL entities onto instances of PolyORB-HI/C elements.

In the following, we detail only the rules that are directly related
to the distributed application as a whole system. The rules that are
specific to the components of the distributed application are
explained in the sections that deals with these respective components.

Mapping AADL :code:`system`
---------------------------

A full system is captured using a root system. The system
implementation shown on the following example models such system.

.. literalinclude:: source/ping/ping.aadl
   :language: aadl

For each node (process) of the system, we instantiate a subcomponent
in the system implementation, and bind it to a :code:`processor`
component. This processor will configure the build target.

We use the :code:`properties` section of the AADL :code:`system` (see
@ref{Hosts` for more details) to map the different nodes on the
different platforms of the distributed application. The
:code:`connections` section of the system implementation models the
connections between the different nodes of the application.

An AADL :code:`system` is mapped into a hierarchy of directories:

* the root directory of the distributed application has the same name
  as the root system implementation, in lower case, all dot being
  converted into underscores. This directory is the root of the
  directory hierarchy of the generated C code.

* for each node of the distributed application, a child directory
  having the same name as the corresponding process subcomponent (in
  lower case) is created inside the root directory. This child
  directory will contain all the code generated for the particular
  node it was created for (see @ref{Distributed application nodes} for
  more details).

Mapping AADL :code:`process`
----------------------------

We use the :code:`process` component category to model an application
node (e.g. an element that would ultimately become a Unix process, an
embedded application or an ARINC653 partition). The process
implementation shown in the listing below shows such system.

.. literalinclude:: source/ping/node_a_model.aadl
   :language: aadl

For each thread that belongs to a node of the distributed application,
we instantiate a subcomponent in the process implementation. For each
connection between a node and another, a :code:`port` feature has to be
added to both nodes with the direction :code:`out` for the source and
:code:`in` for the destination (see @ref{Connections} for more details
on connections mapping).

Elements associated to this process (threads, subprograms, data types,
etc) are generated in a child directory of the root system directory.
This directory has the same name as the process *subcomponent*
instance relative to the handled node in the system implementation
that model the distributed application, in lower case.

For example, all the entities relative to the process :code:`A.Impl`
of the :code:`Ping` example are generated in the directory
:code:`ping_impl/node_a`.

The following paragraphs list the C compilation units that are
created for each node of the distributed application.

Marshallers functions
^^^^^^^^^^^^^^^^^^^^^

The marshallers functions are used to put all request and types values
in a message in order to send them through a network connections. All
marshalling functions are declared in the file :code:`marshallers.c`.

However, PolyORB-HI-C can also use third-party marshallers. It can
rely on the marshallers generated for ASN1 encoding. Details about
ASN1 marshallers are provided in the next section.

@subsubsection Using ASN1 marshallers

With the ASN1 tools from Semantix
(see. @url{http://www.semantix.gr/assert/}), you can convert ASN1
declarations into AADL models. Then, these models can be used with
AADL components and PolyORB-HI-C relies on Semantix tools to
automatically generates C code that implements the ASN1 types.

For that purpose, you need to install the program :code:`asn2aadlPlus`
and :code:`asn1cc`. These programs are freely available on
@url{http://www.semantix.gr/assert/}. Then, when you use ASN1 types
with your AADL model (with the AADL files generated with
:code:`asn2aadlPlus`), PolyORB-HI-C uses the generated code from ASN1
descriptions and integrate it to marshall data.

Node activity
^^^^^^^^^^^^^
We denote *activity* the set of the actions performed by one
particular node which are not triggered by other nodes. All the
periodic threads of a node are part of the node activity.

The code related to the node activity is generated in an C file
with the name :file:`activity.c`. An example is shown below :

.. literalinclude:: source/ping/activity.c
   :language: c

All the naming rules explained in @ref{Whole distributed application}
are also applied to map the package name. This file contains all the
routines mapped from the periodic threads that belong to the handled
node (see @ref{Threads} for more details on thread mapping). This
package contains also the instances of shared objects used in this
node (see @ref{Data} for more details). If the node does not contain
any *periodic* thread nor shared objects, there is no
:file:`activity.c` file generated for this node. Thus, the node
:code:`B` in the :code:`Ping` example does not have a
:file:`activity.c` package.

Data types
^^^^^^^^^^

All the data types mapped from AADL data components and used by a
particular node of a distributed application are gathered in a
separate C file called :file:`types.h`.

For more detail on the mapping of data components, see @ref{Data}.

Subprograms
^^^^^^^^^^^

The mapping of all AADL subprogram components used by a particular
node is generated in a separate file called :file:`subprograms.c`.
The content of the file is shown in the following example:

For more detail on the mapping of subprogram components, see
@ref{Subprograms}.

Deployment information
^^^^^^^^^^^^^^^^^^^^^^

The deployment information is the information each node has on the
other nodes in the distributed applications. This information is used,
to send a request to another node or to receive a request from
another node. The deployment information is generated for each node in
two C files : :file:`deployment.h` and :file:`deployment.c`.

The file :file:`deployment.h` contains the following types

* a first type called :code:`__po_hi_node_t`. For each node in the
  application we create an enum whose name is mapped from the node
  *instance* declared in the system implementation to which we
  concatenate the string *_k*. All the naming rules listed in
  @ref{Whole distributed application} have to be respected.

* a second type called :code:`__po_hi_entity_t`. For each thread in the
  the application, we declare an enum.

* a third type called :code:`__po_hi_task_id`. For each thread that
  run on the current node.

* a fourth type called :code:`__po_hi_entity_server_t`. For each node
  that may communicate with the current node, we add a value in this
  enum. It will be used by the transport layer. Please note that at least
  one server is declared : the value :code:`invalid_server`.

* a fifth type called :code:`__po_hi_port_t` that contains all global port
  identifier.

More, this file contains the following maccros :

* :code:`__PO_HI_NB_ENTITIES` is the number of entities in
  the whole distributed system.

* :code:`__PO_HI_NB_TASKS` is the number of the tasks that will
  be started on the current node

* :code:`__PO_HI_NB_NODES` is the number of nodes in the
  distributed system.

* :code:`__PO_HI_PROTECTED` is the number of protected objects
  use on the current node.

* :code:`__PO_HI_NB_PORTS` that represent the total number of ports
  in the whole distributed system.

* :code:`__PO_HI_NB_DEVICES` that represent the total number of devices
  in the whole distributed system.

The file :file:`deployment.c` contains the following variables :

* :code:`mynode` variable which has the value of the
  handled node.

* :code:`__po_hi_entity_table` variable is used to know on
  which node an entity runs.

* :code:`__po_hi_port_global_to_local` variable is used
  to convert a global port identifier to a local port identifier

* :code:`__po_hi_port_global_to_entity` variable is used
  to know on which entity a given port is. This table is used
  convert a global port identifier to an entity identifier.

* :code:`__po_hi_uint8_t __po_hi_deployment_endiannesses`
  variable details which the endianess of each node. It is an array
  which size is :code:`__PO_HI_NB_NODES`.

* :code:`__po_hi_port_to_device` is an array which size is
  :code:`__PO_HI_NB_PORTS`. For each port, it indicates the
  value of the device identifier that handles it.

* :code:`__po_hi_port_global_model_names` is an array which size is
  :code:`__PO_HI_NB_PORTS`. For each port, it contains the name of the
  port.

* :code:`__po_hi_port_global_names` is an array which size is
  :code:`__PO_HI_NB_PORTS`.  For each port, it contains the name
  generated by the code generator.

* :code:`__po_hi_devices_naming` is an array which size is
  :code:`__PO_HI_NB_DEVICES`.  For each deivce, it contains all relevant
  information for their configuration. The configuration string is
  deduced from the :code:`Configuration` property associated with the
  device.

The following example shows the :code:`Deployment` package relative to
the node :code:`A` of the :code:`Ping` example:

.. literalinclude:: source/ping/deployment.h
   :language: c

OS Configuration
^^^^^^^^^^^^^^^^

A host is the set formed by a processor and an operating system (or
real-time kernel).

To model both the processor and the OS, we use the :code:`processor`
AADL component. The characteristics of the processor are defined using
AADL properties. For example, if our distributed application uses
an IP based network to make its node communicate, then each host must
have an IP address. Each host must also precise its platform (native,
LEON...). The listing following example shows how to express this
using a custom property set.

.. literalinclude:: source/ping/hardware.aadl
   :language: aadl

To map an application node (processor) to a particular host, we use
the :code:`Actual_Processor_Binding` property. The following example
shows how the node :code:`Node_A` is mapped to the processor
:code:`Proc_A` in the :code:`Ping` example.

.. literalinclude:: source/ping/processor_binding.aadl
   :language: aadl

.. :lines: 3,5

The C generated code concerning the code generation to model host
mapping is located in the :file:`naming.c` file. More precisely, the
:code:`node_addr` and :code:`node_port` contains, for each node, the
information related to its host. These information are dependant on
the transport mechanism used in the distributed application.

Mapping AADL :code:`threads`
----------------------------

The threads are the active part of the distributed application. A node
must contain at least one thread and may contain more than one
thread. In this section, we give the AADL entities used to model
threads. Then, we give the mapping rule to generate C code
corresponding to the periodic and aperiodic threads.

The rules are listed relatively to the packages generated for the
nodes and for the distributed application (see @ref{Distributed
application nodes} and @ref{Whole distributed application}). Only
rules that are related directly to a thread as a whole subsystem are
listed here.

AADL entities
^^^^^^^^^^^^^

AADL :code:`thread` components are used to model execution flows.

The :code:`features` subclause of the thread component declaration
describes the thread interface. The ports that may be connected to the
ports of other threads, enclosing process, etc.

The :code:`properties` subclause of the thread implementation lists
the properties of the thread such as its priority, its nature
(periodic, sporadic) and many other properties ares expressed using
AADL properties.

The :code:`calls` subclause of the thread implementation contains the
sequences of subprograms the thread may call during its job (see
@ref{Subprograms} for more details on the subprogram mapping). If the
thread job consist of calling more than one subprogram, it is
**mandatory** to encapsulate these calls inside a single subprogram
which will consist the thread job.

The :code:`connections` section of a thread implementation connects
the parameters of the subprograms called by the thread to the ports of
the threads or to the parameters of other called subprograms in the
same thread.

.. literalinclude:: source/ping/thread_a.aadl
   :language: aadl

The listing above shows the thread :code:`P` which belongs to the
process :code:`A.impl` in the :code:`Ping` example. We can see that
:code:`P` is a periodic thread with a period of 1000ms, that this
thread has a unique :code:`out event data port` and that at each
period, the thread performs a call to the :code:`Do_Ping_Spg`
subprogram whose :code:`out parameter` is connected to the thread port.

Mapping rules for periodic threads
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Periodic threads are cyclic threads that are triggered by and only by
a periodic time event. between two time events the periodic threads do
a non blocking job and then they sleep waiting for the next time
event.

The majority of the code generated for the periodic threads is put in
the :file:`activity.c` file generated for the application node
containing the handled thread. Each periodic thread is created in the
main function (:file:`main.c` file) with the
:code:`__po_hi_create_periodic_task` function-call.

The generated code in the :file:`activity.c` file is a parameterless
function that represents the thread job. The defining identifier of
the function is mapped from the thread instance name in the process
that models the node, to which we append the string
:code:`_job`. All the naming rules listed in @ref{Whole distributed
application} have to be respected. The body of this subprogram calls
the subprograms mapped from the subprogram calls the thread
performs. Then, it sends the request to the remote threads it may be
connected to. Finally, at the end of the function, we make a call to
the :code:`__po_hi_wait_next_period()` with the task identifier as
parameter.  This call ensure that we wait the next period before we
start the function again.

The generated code in :file:`main.c` file is a function call that creates
a periodic task. The task is created with the function
:code:`__po_hi_create_periodic_task`. This creates a periodic task with
the wanted properties at the elaboration time of the node. The package
instantiation name is mapped from the thread instance name in the process
that model the node, to which we append the string :code:`_k`. All the
naming rules listed in @ref{Whole distributed application} have to be
respected. The function-call takes the following parameters:

* the enumerator corresponding to the thread

* the task period,

* the task priority. If the user did not specify a priority, then
  :code:`__PO_HI_DEFAULT_PRIORITY` is used,

* the task job which corresponds to the subprogram :code:`<Thread_Name>_job`.

The following example shows the generated code for the periodic thread
:code:`Pinger` from the node :code:`Node_A` of the :code:`Ping` example:

.. literalinclude:: source/ping/activity.c
   :language: c

Mapping rules for sporadic threads
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sporadic threads are cyclic threads that are triggered by the arrival
of a sporadic event. The minimum inter-arrival time between two
sporadic events is called the period of the sporadic thread.

The majority of the code generated for the sporadic threads is put in
the :file:`activity.c` file generated for the application node
containing the handled thread. Each periodic thread is created in the
main function (:file:`main.c` file) with the
:code:`__po_hi_create_sporadic_task` function-call.

The generated code in the :file:`activity.c` file is a parameterless function
that represents the thread job. The defining identifier of the function
is mapped from the thread instance name in the process that models the node,
to which we append the string :code:`_job`. All the naming rules listed
in @ref{Whole distributed application} have to be respected. In the body
of the function, the thread will wait for an event (most of the time : a
message from another entity).

The generated code in :file:`main.c` file is a function-call that creates
the sporadic task. The task is created with the function
:code:`__po_hi_create_sporadic_task`. This creates a sporadic task with
the wanted properties at the elaboration time of the node. The package
instantiation name is mapped from the thread instance name in the process
that model the node, to which we append the string :code:`_k`. All the
naming rules listed in @ref{Whole distributed application} have to be
respected. The function-call takes the following parameters:

* the enumerator corresponding to the thread

* the task priority. If the user did not specify a priority, then
  :code:`__PO_HI_DEFAULT_PRIORITY` is used,

* the task job which corresponds to the subprogram
  :code:`<Thread_Name>_job`.

The following example shows the generated code for the sporadic thread
:code:`Ping_Me` from the node :code:`Node_B` of the :code:`Ping` example.

.. literalinclude:: source/ping/node_b_activity.c
   :language: c

Deployment information
----------------------

As said in @ref{Distributed application nodes}, the files
:file:`deployment.h` and :file:`deployment.c` are generated for each
node in the distributed application. For each thread port in the whole
distributed application, we declare an enumerator in this type. The
defining identifier of the enumerator is mapped from the process
subcomponent name and the thread subcomponent name as follows:
:code:`<Node_Name>_<Thread_Name>_K`.

For each that that may communicate, we generate the following elements

* A variable called :code:`__po_hi_<thread_name>_local_to_global` (in
  :code:`deployment.c`) that is used to convert a local port identifier
  of the thread to a global one.

* A type :code:`__po_hi_<thread_name>_t` that will contain
  on local port identifier.

* A macro :code:`__po_hi_<thread_name>_nb_ports` that will contain
  the number of ports for the thread.

For these elements, all the naming rules listed in @ref{Whole
distributed application} must be respected.

.. literalinclude:: source/ping/deployment.h
   :language: c

.. literalinclude:: source/ping/deployment_c.c
   :language: c

The listing above shows the generated :code:`__po_hi_entity_server_t` and
:code:`entity_table` for the nodes :code:`B` from the
:code:`Ping` example.

Mapping of AADL :code:`ports`
-----------------------------

Threads can contain one or several ports. To handle them, we declared several
arrays in the :file:`activity.c`

* :code:`__po_hi_<port_name>_destinations` : array for each port of
   the thread which contains all destinations of the port.

* :code:`__po_hi_<thread_name>_woffsets` : array (size = number of
   ports in the thread) used by \pohic for the global queue of the
   thread.

* :code:`__po_hi_<thread_name>_offsets` : array (size = number of
  ports in the thread) used by \pohic for the global queue of the
  thread.

* :code:`__po_hi_<thread_name>_used_size` : array (size = number of
  ports in the thread) used by \pohic for the global queue of the
  thread.

* :code:`__po_hi_<thread_name>_empties` : array (size = number of
  ports in the thread) used by \pohic for the global queue of the
  thread.

* :code:`__po_hi_<thread_name>_first` : array (size = number of ports
  in the thread) used by \pohic for the global queue of the
  thread.

* :code:`__po_hi_<thread_name>_recent` : array (size = number of ports
  in the thread) used by \pohic for the global queue of the
  thread.

* :code:`__po_hi_<thread_name>_queue` : array (size = size of the
  global queue for the thread) used by \pohic to handle the global
  queue.

* :code:`__po_hi_<thread_name>_total_fifo_size` : variable that
  contains the size of the global queue. It is the sum of all port
  size for the thread.

* :code:`__po_hi_<thread_name>_history` : array (size = number of
  ports in the thread) used by \pohic for the global queue of the
  thread.

* :code:`__po_hi_<thread_name>_n_dest` : array (size = number of ports
  in the thread) used by \pohic for the global queue of the
  thread. It contains the number of destinations for each port of
  the thread.

* :code:`__po_hi_<thread_name>_fifo_size` : array (size = number of
  ports in the thread) used by \pohic for the global queue of the
  thread.

* :code:`__po_hi_<thread_name>_destinations` : array (size = number of ports
  in the thread) that contains all destinations for each port.

Mapping of AADL :code:`Connections`
-----------------------------------

The connections are entities that support communication between the
application nodes. In this section, we present the AADL entities used
to model connection between nodes.

A connection between two nodes of the system is modeled by:

* The :code:`ports` features that exist on each one of the nodes. Ports
  can be declared inside processes or threads. The direction of the
  port (:code:`in`, :code:`out` or :code:`in out`) indicates the
  direction of the information flow.

* The :code:`connections` section in the system implementation
  relative to the distributed application and in the process and
  thread implementations.

.. literalinclude:: source/ping/port_cnx.aadl
   :language: aadl

The listing above shows the connection between the node :code:`A` and
:code:`B` in the system implementation.

The nature of the :code:`port` (*event port*, *data port* or *event
data port*) depends on the nature of the connection between the two
nodes:

* if the message sent from one node to another node is only a
  triggering event and contains no data, we create an *event* port.

* if the message sent from one node to another node is a data message
  but it does not trigger the receiver thread, we create a *data*
  port.

* if the message sent from one node to another node is a data message
  that triggers the receiver thread, we create an @i{event data} port.

In a distributed system, when we send any data to a node, we need to
put them in a stream. We call that the marshall operation. On the
other hand, find data in a stream is called the unmarshall
operation. In each distributed application, we generate marshallers
for each types and request. These functions will marshall/unmarshall
data in/from a message.

All marshallers functions are generated in a file called
:file:`marshallers.c`.  The marshall (or unmarshall) functions for
request are prefixed by the string :code:`__po_hi_marshall_request_`
(or :code:`__po_hi_unmarshall_request_`). Marshall (or unmarshall)
functions for types are prefixed by the string
:code:`__po_hi_marshall_type_` (or
:code:`__po_hi_unmarshall_type_`). Each function has the name of the
type or the request it marshalls.

Finally, a function :code:`__po_hi_marshall_request` and
:code:`__po_hi_unmarshall_request` is generated to handle all
requests. Then, is called the appropriate function to call to marshall
or unmarshall the data.

.. literalinclude:: source/ping/marshallers.c
   :language: c

Mapping of AADL :code:`Subprograms`
-----------------------------------

Subprograms are used to encapsulate behavioural aspects of the
application.

To model a subprogram, we use the :code:`subprogram` AADL component
category. The parameters of the subprogram are specified in the
:code:`features` subclausen of the component declaration. If the
subprogram does only the job of calling other declared subprograms,
then the :code:`calls` subclause of the subprogram implementation has
to contain such calls. To point to the actual implementation of the
subprogram, we use the AADL properties.

The following example shows the AADL model for the :code:`Do_Ping_Spg`
from the :code:`Ping` example. It precises that the C implementation of
the subprogram is located in the function :code:`user_ping`. The file
which contains this function must be stored with the aadl model.

Subprograms are generally called by threads or by other subprograms.
To express this, we use the :code:`calls` subclause of a component
implementation. Then we perform all the connections between the called
subprograms parameters and the caller components ports (or parameters
if the caller is a subprogram).

The following listing shows the calls and connections sections of the
periodic thread :code:`P` in the :code:`Ping` example.

.. literalinclude:: source/ping/do_ping_spg.aadl
   :language: aadl

Each subprogram instance model a hand-written function. In the
:file:`subprograms.c` file, we declare the definition of this function
and we generate a new one that will call the one provided by the user.

The following listing shows the calls and connections sections of the
subprogram :code:`ping_spg` in the :code:`Ping` example.

.. literalinclude:: source/ping/subprograms.c
   :language: c

For each subprogram call in a thread, we generate an C subprogram call
to the subprogram implementing the thread and given by mean of the
AADL properties.

On the client side, the function :code:`sth_Job` begins by calling the
subprogram in its call sequence. then it calls the stubs of all the
subprogram it is connected to.

On the server side, and in the function of the
:code:`process_request`, the subprogram implementation corresponding
to the operation (coded in the message) is called.

Mapping of AADL :code:`data`
----------------------------

The data are the messages exchanged amongst the nodes of the
application.

AADL :code:`data` components are used to model data exchanged in the
distributed application. Properties are used to precise the nature of
the data. To model a data structure (which contains fields of others
data types) we use data component implementation and we add a
subcomponent for each field of the structure.

The simple data types that can be modeled using AADL are

* boolean,
* integer,
* fixed point types,
* characters,
* wide characters

.. literalinclude:: source/ping/simple_types.aadl
   :language: aadl

The complex data types that can be modeled using AADL are

* Bounded strings
* Bounded wide strings
* Bounded arrays of a type that can be modeled
* Structure where the fields types are types that can be
  modeled

.. literalinclude:: source/ping/complex_types.aadl
   :language: aadl

Data components may also contain subprogram features. Depending on
the AADL properties given by the user. These component may denote a
protected object or a non protected object. In either case, they are
used to model a data structure that can be handled only by the
subprograms it exports (which are the feature of the data structure).

@include protected_object_types.texi

The example above shows an example of
a protected data component (@code{Protected_Object.Impl}).
The object has a single field (subcomponent) which is a simple data
component. Note that the description of the feature subprograms of
these data component is a little bit different from the description of
classic subprograms: each feature subprogram must have a full access
to the internal structure of the object type. To achieve this, we use
the @code{require data access} facility of AADL. To model a non
protected data component, user should simply change the
@code{ARAO::Object_Kind => Protected;} into
@code{ARAO::Object_Kind => Non_Protected;} in the implementation
of data component.

@subsection C mapping rules

Data component declaration are mapped into C type declaration in the
file @code{types.h}.
In the following we give the C type corresponding to each data component
type that could be modeled.

@subsubsection Simple types

Simple data components are mapped into an C type definition whose
defining identifier is mapped from the component declaration
identifier (with respect to the naming rules listed
in @ref{Whole distributed application}) and whose parent subtypes is:
@itemize @bullet
  @item @code{int} for boolean data types
  @item @code{int} for integer data types
  @item @code{float} for fixed point types
  @item @code{chat} for character data types
@end itemize

@subsubsection Bounded strings and wide strings

Bounded strings and wide strings are not supported in the C generator at this
time.


@subsubsection Bounded arrays

Bounded arrays and wide strings are not supported in the C generator at this
time.


@subsubsection Data structures

Data structures are mapped into a C structure defined in the file @file{types.h}.
The identifier of the record type is mapped from the data component name
with respect to the naming rules given in @ref{Whole distributed
application}. Each field defining identifier is mapped from the
subcomponent name given in the data component implementation with the
same naming rules. The type of the field is the C type mapped from
the data corresponding component. The following example shows the C
mapping of the data structure defined given earlier in this part.

@include data_struct.h.texi

@subsubsection Object types

Protected object types are mapped into an a C structure. We add automatically a
member in the structure with the type @code{__po_hi_protected_id} and the name
@code{protected_id}. This member
will identify the protected type in the distributed system. All other members of
the object are declared as in Data Structures (see previous subsection). The
features subprograms of the object types are declared in the @file{types.h}
file, whereas the body of these functions are defined in the @file{types.c}
file. Moreover, the value of the @code{protected_id} must be initialized. This
is done in the main function (@file{main.c}), before the initialization.
All the naming conventions given in @ref{Whole distributed application} have to
be respected. The following example shows the specification of the protected type
mapped from the @code{Protected_Object.Impl} shown earlier in this part. We show
the files @file{types.h}, @file{types.c} and @file{main.c} (that initialize the
@code{protected_id} member of the structure.

@include toy_types.h.texi
@include toy_types.c.texi
@include toy_main.c.texi

Non protected object types are mapped similarly to protected object
types. The only difference, is that instead of creating a protected
type, we create a generic parameterless nested package.
