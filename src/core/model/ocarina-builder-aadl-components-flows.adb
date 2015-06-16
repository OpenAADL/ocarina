------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BUILDER.AADL.COMPONENTS.FLOWS                   --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Builder.AADL.Components.Flows is

   use Ocarina.Builder.AADL.Components;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Flow                 : Node_Id;
      Property_Association : Node_Id) return Boolean
   is
      pragma Assert
        (Kind (Flow) = K_Flow_Spec
         or else Kind (Flow) = K_Flow_Implementation
         or else Kind (Flow) = K_End_To_End_Flow_Spec
         or else Kind (Flow) = K_Flow_Implementation_Refinement
         or else Kind (Flow) = K_End_To_End_Flow_Refinement);
      pragma Assert (Present (Property_Association));
   begin
      if Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Flow)) then
         Set_Properties
           (Flow,
            New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List
        (Property_Association,
         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Flow));
      return True;
   end Add_Property_Association;

   -----------------------
   -- Add_New_Flow_Spec --
   -----------------------

   function Add_New_Flow_Spec
     (Loc           : Location;
      Name          : Node_Id;
      Comp_Type     : Node_Id;
      Category      : Flow_Category;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_Id;
      Is_Refinement : Boolean := False) return Node_Id
   is
      pragma Assert (Kind (Comp_Type) = K_Component_Type);

      Node    : constant Node_Id := New_Node (K_Flow_Spec, Loc);
      Success : Boolean;
   begin
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Properties (Node, No_List);
      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);
      Set_Is_Refinement (Node, Is_Refinement);
      Set_Category (Node, Flow_Category'Pos (Category));
      Set_Source_Flow (Node, Source_Flow);
      Set_Sink_Flow (Node, Sink_Flow);

      Success := Add_Flow_Spec (Comp_Type, Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Flow_Spec;

   ---------------------------------
   -- Add_New_Flow_Implementation --
   ---------------------------------

   function Add_New_Flow_Implementation
     (Loc           : Location;
      Container     : Node_Id;
      Name          : Node_Id;
      Category      : Flow_Category;
      In_Modes      : Node_Id;
      Is_Refinement : Boolean;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_Id) return Node_Id
   is
      pragma Assert (Present (Name));
      pragma Assert (Kind (Container) = K_Component_Implementation);
      pragma Assert
        (not Is_Refinement or else (No (Source_Flow) and then No (Sink_Flow)));

      Node : Node_Id;
   begin
      if Is_Refinement then
         Node := New_Node (K_Flow_Implementation_Refinement, Loc);
      else
         Node := New_Node (K_Flow_Implementation, Loc);
      end if;

      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Category (Node, Flow_Category'Pos (Category));
      Set_Properties (Node, No_List);
      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);
      Set_In_Modes (Node, In_Modes);

      if not Is_Refinement then
         Set_Source_Flow (Node, Source_Flow);
         Set_Sink_Flow (Node, Sink_Flow);
      end if;

      if not Add_Flow_Implementation (Container, Node) then
         Node := No_Node;
      end if;

      return Node;
   end Add_New_Flow_Implementation;

   ----------------------------------
   -- Add_New_End_To_End_Flow_Spec --
   ----------------------------------

   function Add_New_End_To_End_Flow_Spec
     (Loc           : Location;
      Container     : Node_Id;
      Name          : Node_Id;
      In_Modes      : Node_Id;
      Is_Refinement : Boolean;
      Source_Flow   : Node_Id;
      Sink_Flow     : Node_Id) return Node_Id
   is
      pragma Assert (Present (Name));
      pragma Assert (Kind (Container) = K_Component_Implementation);
      pragma Assert
        (not Is_Refinement or else (No (Source_Flow) and then No (Sink_Flow)));

      Node : Node_Id;
   begin
      if Is_Refinement then
         Node := New_Node (K_End_To_End_Flow_Refinement, Loc);
      else
         Node := New_Node (K_End_To_End_Flow_Spec, Loc);
      end if;

      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);
      Set_Properties (Node, No_List);
      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);
      Set_In_Modes (Node, In_Modes);

      if not Is_Refinement then
         Set_Source_Flow (Node, Source_Flow);
         Set_Sink_Flow (Node, Sink_Flow);
      end if;

      if not Add_End_To_End_Flow_Spec (Container, Node) then
         Node := No_Node;
      end if;

      return Node;
   end Add_New_End_To_End_Flow_Spec;

end Ocarina.Builder.AADL.Components.Flows;
