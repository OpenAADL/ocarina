------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              O C A R I N A . T R A N S F O . F U S I O N S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2008-2010, GET-Telecom Paris.                --
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

--  Perform thread merging

package Ocarina.Transfo.Fusions is

   procedure Init;
   --  Fix the name allocation for features, components and subprograms
   --  Sporadic threads

   procedure Reset;
   --  To be called between each execution of fusion_threads

   procedure Fusion_Threads
     (Root       : Node_Id;
      Owner_Process, Thread_1, Thread_2 : Name_Id;
      New_Thread : out Node_Id;
      Success    : out Boolean);
   --  Perfom the fusion of the two threads passed as arguments

   Transfo_Original_Name : Name_Id;
   Raw_Original_Name    : Name_Id;
   Transfo_Scheduler_Name : Name_Id;
   Raw_Scheduler_Name   : Name_Id;
   Dispatch_Protocol    : Name_Id;
   Deadline             : Name_Id;
   WCET                 : Name_Id;
   Deployment_Priority  : Name_Id;
   Cheddar_Priority     : Name_Id;
   Transfo_Priority     : Name_Id;
   Transfo_Occurred     : Name_Id;
   Scheduler_Call       : Name_Id;
   Iterator_Call        : Name_Id;
   Compute_Entrypoint   : Name_Id;
   CS_Period            : Name_Id;
   Period               : Name_Id;
   Priority_Shifter     : Name_Id;
   Raw_Priority_Shifter : Name_Id;
   Cheddar_Pkg          : Name_Id;
   Transfo_Pkg          : Name_Id;
   Raw_Priority         : Name_Id;
   Raw_Fixed_Priority   : Name_Id;
   Raw_Occurred         : Name_Id;
   Raw_Data_Representation : Name_Id;
   Data_Representation  : Name_Id;
   Data_Model_Pkg       : Name_Id;
   --  name of corresponding AADL properties

   function Is_Defined_Property
     (Component : Node_Id;
      Property  : Name_Id) return Boolean;
   --  Search a property in component implementation and declaration

   function Get_Integer_Property
     (Component : Node_Id;
      Property  : Name_Id) return Unsigned_Long_Long;
   --  Search a property in component implementation and declaration

   function Get_Boolean_Property
     (Component : Node_Id;
      Property  : Name_Id) return Boolean;
   --  Search a property in component implementation and declaration

private

   type Node_Only is record
      List_Node : List_Id;
   end record;

   type Node_Association is record
      Old_Node : Node_Id;
      New_Node : Node_Id;
   end record;

   type Node_Counter is record
      Node : Node_Id;
      Cnt  : Int;
   end record;

   type Node_Priority is record
      Node   : Node_Id;
      Cnt    : Int;
      Object : Node_Id;
   end record;

end Ocarina.Transfo.Fusions;
