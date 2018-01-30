
------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . C H E D D A R . M A P P I N G      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2010-2018 ESA & ISAE.                    --
--                                                                          --
-- Ocarina  is free software; you can redistribute it and/or modify under   --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. Ocarina is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY; without even the implied warranty of               --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Helper;
with Ocarina.Backends.Utils;

package Ocarina.Backends.Cheddar.Mapping is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Helper;
   use Ocarina.Backends.Utils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   function Map_HI_Node (E : Node_Id) return Node_Id;
   function Map_HI_Unit (E : Node_Id) return Node_Id;

   function Map_Processor (E : Node_Id) return Node_Id with
     Pre => (True and then
               --  1/ Typing
               AINU.Is_Processor (E) and then

               --  2/ Property requirements
               --  a) Scheduling_Protocol policy is specified
               (Get_Scheduling_Protocol (E) /= Unknown_Scheduler)
            );

   function Map_Process (E : Node_Id) return Node_Id with
     Pre => (
             --  1/ Typing
             AINU.Is_Process (E)
            );

   function Map_Thread (E : Node_Id) return Node_Id with
     Pre => (True and then
               --  1/ Typing
               AINU.Is_Thread (E) and then

               --  2/ Property requirements
               --  The thread a) has dispatch protocol specified, b)
               --  has compute_execution_time specified, if it is
               --  either periodic or sporadic, then it has a period

               (Get_Thread_Dispatch_Protocol (E) /= Thread_None) and then
               (Get_Execution_Time (E) /= Empty_Time_Array) and then
               (if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic or else
                  Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic then
                    Get_Thread_Period (E) /= Null_Time) and then

              --  3/ Architecture requirements
              --  a) There is a linked processor P for E
              (for some P of Processors (Get_Root_Component (E)) =>
                AINU.Is_Processor (P) and then
                P = Get_Bound_Processor
                (Corresponding_Instance
                   (Get_Container_Process (Parent_Subcomponent (E)))))
            );

   function Map_Data (E : Node_Id) return Node_Id with
     Pre => (True and then
               --  1/ Typing
               AINU.Is_Data (E) and then

               --  2/ Architecture requirements
               --  a) There is a linked processor P for E
               (for some P of Processors (Get_Root_Component (E)) =>
                  AINU.Is_Processor (P) and then
                  P = Get_Bound_Processor
                  (Corresponding_Instance
                     (Get_Container_Process (Parent_Subcomponent (E)))))
               and then

               --  b) There is at least one thread accessing the data

               (for some T of Threads (Get_Root_Component (E)) =>
                  (for some C of Connections_Of
                     (Corresponding_Instance
                        (Get_Container_Process
                           (Parent_Subcomponent (E))))
                     => T = Corresponding_Instance
                     (Item (AIN.First_Node
                              (Path (Destination (C)))))))

            );

   function Map_Buffer (E : Node_Id; P : Node_Id) return Node_Id with
     Pre => (True and then
               --  1/ Typing

               --  a) E is a thread
               AINU.Is_Thread (E) and then

               --  b) P is an in event (data) port of E
               AINU.Is_Port (P) and then
               Is_Event (P) and then
               Is_In (P) and then
               (for some F of Features_Of (E) => P = F) and then

               --  2/ Architecture requirements

               --  a) There is a linked processor P for E
               (for some CPU of Processors (Get_Root_Component (E)) =>
                  AINU.Is_Processor (CPU) and then
                  CPU = Get_Bound_Processor
                  (Corresponding_Instance
                     (Get_Container_Process (Parent_Subcomponent (E)))))
               and then

               --  b) P is connected to some threads
               not AINU.Is_Empty (Get_Source_Ports (P))
            );

   function Map_Dependency (E : Node_Id; P : Node_Id) return Node_Id;

end Ocarina.Backends.Cheddar.Mapping;
