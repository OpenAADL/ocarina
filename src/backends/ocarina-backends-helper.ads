------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              O C A R I N A . B A C K E N D S . H E L P E R               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2018-2019 ESA & ISAE.                    --
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

---  This package provides a set of helper routines to access AADL models
---
---  Each accessor relies on Node_Array type, allowing to use Ada
---  2012-type of iterations:
---
---     for S of Subcomponents_Of (My_N) loop
---       --  do something
---     end loop;

with Ocarina.Instances.Finder;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

package Ocarina.Backends.Helper is

   use Ocarina.Instances.Finder;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   --------------------------
   -- Component subclauses --
   --------------------------

   --  For a component instance, the following accessors return the
   --  elements from the corresponding subclause as a Node_Array.

   function Features_Of (E : Node_Id) return Node_Array is
     (if No (Features (E)) then No_Nodes
     else To_Node_Array (Features (E)))
     with Pre => (Kind (E) = K_Component_Instance),
          Post => (Features_Of'Result = No_Nodes or else
                   (for all E of Features_Of'Result =>
                    Kind (E) in K_Feature_Instance |
                                K_Parameter_Instance |
                                K_Port_Spec_Instance |
                                K_Subcomponent_Access_Instance));

   function Properties_Of (E : Node_Id) return Node_Array is
     (if No (Properties (E)) then No_Nodes
      else To_Node_Array (Properties (E)))
      with Pre => (Kind (E) = K_Component_Instance),
           Post => (for all E of Properties_Of'Result =>
                    Kind (E) = K_Property_Association_Instance);

   function Subcomponents_Of (E : Node_Id) return Node_Array
      with Pre => (Kind (E) = K_Component_Instance),
           Post => (for all E of Subcomponents_Of'Result =>
                    Kind (E) = K_Subcomponent_Instance);

   function Connections_Of (E : Node_Id) return Node_Array is
     (if No (Connections (E)) then No_Nodes
      else To_Node_Array (Connections (E)))
      with Pre => (Kind (E) = K_Component_Instance),
           Post => (for all E of Connections_Of'Result =>
                    Kind (E) = K_Connection_Instance);

   --------------------------
   -- Component categories --
   --------------------------

   --  Note: the list of categories is defined in Ocarina.ME_AADL

   function Abstracts (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Abstract));

   function Data (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Data));

   function Subprograms (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Subprogram));

   function Threads (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Thread));

   function Processes (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Process));

   function Memories (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Memory));

   function Processors (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Processor));

   function Virtual_Processors (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Virtual_Processor));

   function Buses (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Bus));

   function Virtual_Buses (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Virtual_Bus));

   function Devices (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_Device));

   function Systems (Root : Node_Id) return Node_Array is
      (Filter_Instance_By_Category
         (Find_All_Component_Instances (Root), CC_System));

   ---------------------
   -- Pretty printing --
   ---------------------

   function Category_Name (C : Component_Category) return String is
      (case C is
         when CC_Abstract          => "abstract",
         when CC_Bus               => "bus",
         when CC_Data              => "data",
         when CC_Device            => "device",
         when CC_Memory            => "memory",
         when CC_Process           => "process",
         when CC_Processor         => "processor",
         when CC_Subprogram        => "subprogram",
         when CC_Subprogram_Group  => "subprogram_group",
         when CC_System            => "system",
         when CC_Thread            => "thread",
         when CC_Thread_Group      => "thread_group",
         when CC_Unknown           => raise Program_Error,
         when CC_Virtual_Bus       => "virtual_bus",
         when CC_Virtual_Processor => "virtual_processor");

end Ocarina.Backends.Helper;
