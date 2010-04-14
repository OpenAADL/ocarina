------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . T R A N S F O . F U S I O N S . S C H E D U L E R     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

--  Generate the scheduler for a given thread, and output it to a file

package Ocarina.Transfo.Fusions.Scheduler is

   Min_Priority : Int;
   Max_Priority : Int;
   --  Minimum and maximum priorities amongst the threads call sequences'

   procedure Initialize
     (New_Thread, Old_Thread_1, Old_Thread_2 : Node_Id);
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
