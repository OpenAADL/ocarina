------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BUILDER.AADL.COMPONENTS.CONNECTIONS                --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Builder.AADL.Components.Connections is

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Connection           : Node_Id;
      Property_Association : Node_Id)
     return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Connection /= No_Node
                     and then Kind (Connection) = K_Connection);
      pragma Assert (Property_Association /= No_Node);
   begin
      if Is_Empty
        (Ocarina.Me_AADL.AADL_Tree.Nodes.Properties (Connection)) then
         Set_Properties (Connection,
                         New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List (Property_Association,
                           Ocarina.Me_AADL.AADL_Tree.Nodes.Properties
                             (Connection));
      return True;
   end Add_Property_Association;

   ------------------------
   -- Add_New_Connection --
   ------------------------

   function Add_New_Connection
     (Loc           : Location;
      Name          : Node_Id;
      Comp_Impl     : Node_Id;
      Category      : Ocarina.Me_AADL.Connection_Type;
      Is_Refinement : Boolean := False;
      Is_Bidirect   : Boolean := False;
      Source        : Node_Id := No_Node;
      Destination   : Node_Id := No_Node;
      In_Modes      : Node_Id := No_Node)
     return Node_Id
   is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Comp_Impl) = K_Component_Implementation);

      Node : constant Node_Id := New_Node (K_Connection, Loc);
      Success : Boolean := True;
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

      Success := Add_Connection (Component  => Comp_Impl,
                                 Connection => Node);

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
      Category      : Ocarina.Me_AADL.Connection_Type;
      Is_Refinement : Boolean := False;
      Is_Bidirect   : Boolean := False;
      Source        : Node_Id := No_Node;
      Destination   : Node_Id := No_Node;
      In_Modes      : Node_Id := No_Node)
     return Node_Id
   is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

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
