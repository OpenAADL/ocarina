------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.AADL_SPACESTUDIO.MAPPING                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Backends.python.Nodes;
with Ocarina.Backends.python.Nutils;

package body Ocarina.Backends.AADL_SpaceStudio.Mapping is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;

   use Ocarina.Backends.python.Nodes;
   use Ocarina.Backends.python.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package PN renames Ocarina.Backends.python.Nodes;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node (E : Node_Id) return Node_Id is
      pragma Assert
        (AINU.Is_Process (E)
         or else AINU.Is_System (E)
         or else AINU.Is_Processor (E));

      N : constant Node_Id := New_Node (PN.K_HI_Node);

   begin
      if AINU.Is_System (E) then
         Set_Str_To_Name_Buffer ("general");
      else
         Get_Name_String
           (To_python_Name (AIN.Name (Identifier (Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_cheddar"); --  XXX ????
      end if;

      PN.Set_Name (N, Name_Find);
      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list

      Append_Node_To_List (N, HI_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------

   function Map_HI_Unit (E : Node_Id) return Node_Id is
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process (E)
         or else AINU.Is_Processor (E));

      U    : Node_Id;
      N    : Node_Id;
      P    : Node_Id;
      Root : Node_Id;
   begin
      U := New_Node (PN.K_HI_Unit, Identifier (E));

      --  Packages that are common to all nodes

      if AINU.Is_System (E) then
         Get_Name_String (To_python_Name (Display_Name (Identifier (E))));

      else
         Get_Name_String
           (To_python_Name
              (Display_Name (Identifier (Parent_Subcomponent (E)))));
      end if;

      Add_Str_To_Name_Buffer ("_aadl_Python");
      N := Make_Defining_Identifier (Name_Find);

      P := Make_python_File (N);
      Set_Distributed_Application_Unit (P, U);
      PN.Set_python_File (U, P);

      Root := Make_python_Node ("", No_Name, K_Nameid);

      PN.Set_Root_Node (P, Root);

      Append_Node_To_List (U, Units (Current_Entity));
      PN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

end Ocarina.Backends.AADL_SpaceStudio.Mapping;
