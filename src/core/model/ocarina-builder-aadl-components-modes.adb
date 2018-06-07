------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BUILDER.AADL.COMPONENTS.MODES                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.Builder.AADL.Components.Modes is

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Mode                 : Types.Node_Id;
      Property_Association : Types.Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Mode /= No_Node and then Kind (Mode) = K_Mode);
      pragma Assert (Property_Association /= No_Node);
   begin
      if Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Mode)) then
         Set_Properties
           (Mode,
            New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List
        (Property_Association,
         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Mode));
      return True;
   end Add_Property_Association;

   ------------------
   -- Add_New_Mode --
   ------------------

   function Add_New_Mode
     (Loc        : Locations.Location;
      Identifier : Types.Node_Id;
      Component  : Types.Node_Id) return Types.Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Identifier /= No_Node and then Kind (Identifier) = K_Identifier);
      pragma Assert
        (Component /= No_Node
         and then
         (Kind (Component) = K_Component_Implementation
          or else Kind (Component) = K_Component_Type));

      Node          : Node_Id;
      Success       : Boolean          := True;
      Property_Scop : constant Node_Id := New_Node (K_Scope_Definition, Loc);
   begin
      Node := New_Node (K_Mode, Loc);
      Set_Identifier (Node, Identifier);
      Set_Corresponding_Entity (Identifier, Node);

      Set_Property_Scope (Node, Property_Scop);
      Set_Corresponding_Entity (Property_Scope (Node), Node);

      Success := Add_Mode (Component, Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Mode;

   -----------------------------
   -- Add_New_Mode_Transition --
   -----------------------------

   function Add_New_Mode_Transition
     (Loc       : Locations.Location;
      Component : Types.Node_Id) return Types.Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Component /= No_Node
         and then Kind (Component) = K_Component_Implementation);

      Node    : constant Node_Id := New_Node (K_Mode_Transition, Loc);
      Success : Boolean          := True;
   begin
      Success := Add_Mode (Component => Component, Mode => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Mode_Transition;

   -------------------------------------
   -- Add_New_Mode_Transition_Trigger --
   -------------------------------------

   function Add_New_Mode_Transition_Trigger
     (Loc          : Locations.Location;
      Identifier   : Types.Node_Id;
      Is_Self      : Boolean;
      Is_Processor : Boolean) return Types.Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Identifier /= No_Node);

      Node : constant Node_Id := New_Node (K_Mode_Transition_Trigger, Loc);
   begin

      Set_Identifier (Node, Identifier);
      Set_Is_Self (Node, Is_Self);
      Set_Is_Processor (Node, Is_Processor);

      return Node;

   end Add_New_Mode_Transition_Trigger;

end Ocarina.Builder.AADL.Components.Modes;
