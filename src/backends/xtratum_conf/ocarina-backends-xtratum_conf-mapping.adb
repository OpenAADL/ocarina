------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.XTRATUM_CONF.MAPPING                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2018 ESA & ISAE.                    --
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
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

package body Ocarina.Backends.Xtratum_Conf.Mapping is

   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.XML_Tree.Nodes;
   use Ocarina.Backends.XML_Tree.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (XTN.K_HI_Node);
   begin
      pragma Assert
        (AINU.Is_Process (E)
         or else AINU.Is_System (E)
         or else AINU.Is_Processor (E));

      if AINU.Is_System (E) then
         Set_Str_To_Name_Buffer ("general");
      else
         Get_Name_String
           (To_XML_Name
              (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_arinc653");
      end if;

      XTN.Set_Name (N, Name_Find);

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
      U    : Node_Id;
      N    : Node_Id;
      P    : Node_Id;
      Q    : Node_Id;
      R    : Node_Id;
      Root : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process (E)
         or else AINU.Is_Processor (E));

      U := New_Node (XTN.K_HI_Unit, AIN.Identifier (E));

      --  Packages that are common to all nodes
      Get_Name_String (To_XML_Name (Display_Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_xtratum-conf");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_XML_File (N);
      Set_Distributed_Application_Unit (P, U);
      XTN.Set_XML_File (U, P);

      Root := Make_XML_Node ("SystemDescription");

      Set_Str_To_Name_Buffer ("xmlns");
      R := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("http://www.xtratum.org/xm-3.x");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (Root));

      Set_Str_To_Name_Buffer ("version");
      R := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("1.0.0");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (Root));

      Set_Str_To_Name_Buffer ("name");
      R := Make_Defining_Identifier (Name_Find);

      Get_Name_String (To_XML_Name (Display_Name (Identifier (E))));
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (Root));

      XTN.Set_Root_Node (P, Root);

      Append_Node_To_List (U, Units (Current_Entity));
      XTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

end Ocarina.Backends.Xtratum_Conf.Mapping;
