------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BUILDER.AADL.COMPONENTS.CONNECTIONS                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

package body Ocarina.Builder.AADL.Components.Connections is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Connection           : Node_Id;
      Property_Association : Node_Id) return Boolean
   is
      pragma Assert
        (Connection /= No_Node and then Kind (Connection) = K_Connection);
      pragma Assert (Property_Association /= No_Node);
   begin
      if Is_Empty
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Connection))
      then
         Set_Properties
           (Connection,
            New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List
        (Property_Association,
         Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Connection));
      return True;
   end Add_Property_Association;

   ------------------------
   -- Add_New_Connection --
   ------------------------

   function Add_New_Connection
     (Loc           : Location;
      Name          : Node_Id;
      Comp_Impl     : Node_Id;
      Category      : Ocarina.ME_AADL.Connection_Type;
      Is_Refinement : Boolean := False;
      Is_Bidirect   : Boolean := False;
      Source        : Node_Id := No_Node;
      Destination   : Node_Id := No_Node;
      In_Modes      : Node_Id := No_Node) return Node_Id
   is
      pragma Assert (Kind (Comp_Impl) = K_Component_Implementation);

      Node    : constant Node_Id
        := New_Connection (Loc, Name, Category, Is_Refinement, Is_Bidirect,
                           Source, Destination, In_Modes);
      Success : Boolean          := True;
   begin
      Success := Add_Connection (Component => Comp_Impl, Connection => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Connection;

   --------------------
   -- New_Connection --
   --------------------

   function New_Connection
     (Loc           : Location;
      Name          : Node_Id;
      Category      : Ocarina.ME_AADL.Connection_Type;
      Is_Refinement : Boolean := False;
      Is_Bidirect   : Boolean := False;
      Source        : Node_Id := No_Node;
      Destination   : Node_Id := No_Node;
      In_Modes      : Node_Id := No_Node) return Node_Id
   is
      Node : constant Node_Id := New_Node (K_Connection, Loc);

   begin
      Set_Identifier (Node, Name);

      if Name /= No_Node then
         Set_Corresponding_Entity (Name, Node);
      end if;

      Set_Is_Refinement (Node, Is_Refinement);
      Set_Is_Bidirectional (Node, Is_Bidirect);
      Set_Source (Node, Source);
      Set_Destination (Node, Destination);
      Set_Category (Node, Connection_Type'Pos (Category));
      Set_In_Modes (Node, In_Modes);
      Set_Properties (Node, No_List);
      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);

      return Node;
   end New_Connection;

end Ocarina.Builder.AADL.Components.Connections;
