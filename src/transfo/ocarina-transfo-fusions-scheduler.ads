------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . T R A N S F O . F U S I O N S . S C H E D U L E R     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

--  Generate the scheduler for a given thread, and output it to a file

package Ocarina.Transfo.Fusions.Scheduler is

   Min_Priority : Int;
   Max_Priority : Int;
   --  Minimum and maximum priorities amongst the threads call sequences'

   procedure Initialize (New_Thread, Old_Thread_1, Old_Thread_2 : Node_Id);
   --  By looking at both threads' call sequences, initialize
   --  hyperperiod, quantum and mode array dimensions
   --  FIXME : AADL : add call sequences to period property validity list

   procedure Generate_Schedule;
   --  Compute the actual mode scheduler for the current thread

   procedure Print_Schedule (Package_Name, Thread_Inst_Name : Name_Id);
   --  Create a file which contains the scheduler generic
   --  package instance with the current thread scheduling

   function Find_Initial_Mode return Node_Id;
   --  Returns the thread's initial mode

   procedure Free;
   --  Free all dynamic structures

   function Get_Quantum return Natural;
   --  Return the GCD of all call sequences periods of the tread

   function Get_Call_Sequence_Period (Call_Sequence : Node_Id) return Natural;
   --  Returns the period of the call sequence

   function Get_Call_Sequence_Priority
     (Call_Sequence : Node_Id) return Natural;
   --  Returns the priority of the call sequence

end Ocarina.Transfo.Fusions.Scheduler;
