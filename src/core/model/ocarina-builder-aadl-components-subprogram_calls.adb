------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            OCARINA.BUILDER.AADL.COMPONENTS.SUBPROGRAM_CALLS              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2012 ESA & ISAE.        --
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
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.Builder.AADL.Components.Subprogram_Calls is

   ------------------------------
   -- Add_Property_Association --
   ------------------------------

   function Add_Property_Association
     (Subprogram_Call      : Node_Id;
      Property_Association : Node_Id)
     return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Subprogram_Call) = K_Subprogram_Call);
      pragma Assert (Present (Property_Association));
   begin
      if Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
        (Subprogram_Call) = No_List then
         Set_Properties
           (Subprogram_Call,
            New_List (K_List_Id, Loc (Property_Association)));
      end if;

      Append_Node_To_List (Property_Association,
                           Ocarina.ME_AADL.AADL_Tree.Nodes.Properties
                             (Subprogram_Call));
      return True;
   end Add_Property_Association;

   -------------------------
   -- Add_Subprogram_Call --
   -------------------------

   function Add_Subprogram_Call
     (Call_Sequence   : Node_Id;
      Subprogram_Call : Node_Id)
     return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert (Kind (Subprogram_Call) = K_Subprogram_Call);
      pragma Assert (Kind (Call_Sequence) = K_Subprogram_Call_Sequence);
   begin
      if Ocarina.ME_AADL.AADL_Tree.Nodes.Subprogram_Calls
        (Call_Sequence) = No_List then
         Set_Subprogram_Calls
           (Call_Sequence,
            New_List (K_List_Id, Loc (Subprogram_Call)));
      end if;

      Append_Node_To_List (Subprogram_Call,
                           Ocarina.ME_AADL.AADL_Tree.Nodes.Subprogram_Calls
                             (Call_Sequence));
      Set_Parent_Sequence (Subprogram_Call, Call_Sequence);

      return True;
   end Add_Subprogram_Call;

   -----------------------------
   -- Add_New_Subprogram_Call --
   -----------------------------

   function Add_New_Subprogram_Call
     (Loc           : Locations.Location;
      Name          : Node_Id;
      Call_Sequence : Node_Id)
     return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Name) = K_Identifier);
      pragma Assert (Kind (Call_Sequence) = K_Subprogram_Call_Sequence);

      Node : constant Node_Id := New_Node (K_Subprogram_Call, Loc);
      Success : Boolean := True;
   begin
      Set_Identifier (Node, Name);
      Set_Corresponding_Entity (Name, Node);

      Set_Properties (Node, No_List);
      Set_Property_Scope (Node, New_Node (K_Scope_Definition, Loc));
      Set_Corresponding_Entity (Property_Scope (Node), Node);

      Success := Add_Subprogram_Call
        (Call_Sequence   => Call_Sequence,
         Subprogram_Call => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Subprogram_Call;

   --------------------------------------
   -- Add_New_Subprogram_Call_Sequence --
   --------------------------------------

   function Add_New_Subprogram_Call_Sequence
     (Loc       : Locations.Location;
      Name      : Node_Id;
      Comp_Impl : Node_Id;
      In_Modes  : Node_Id := No_Node)
     return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (No (Name) or else Kind (Name) = K_Identifier);
      pragma Assert (Kind (Comp_Impl) = K_Component_Implementation);

      Node : constant Node_Id := New_Node (K_Subprogram_Call_Sequence, Loc);
      Success : Boolean := True;
   begin
      Set_Identifier (Node, Name);

      if Name /= No_Node then
         Set_Corresponding_Entity (Name, Node);
      end if;

      Set_In_Modes (Node, In_Modes);
      Set_Subprogram_Calls (Node, No_List);
      Success := Add_Subprogram_Call_Sequence
        (Component     => Comp_Impl,
         Call_Sequence => Node);

      if Success then
         return Node;
      else
         return No_Node;
      end if;
   end Add_New_Subprogram_Call_Sequence;

end Ocarina.Builder.AADL.Components.Subprogram_Calls;
