------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . M E _ A A D L . A A D L _ T R E E . N U T I L S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with Locations;                        use Locations;
with Types;                            use Types;
with Ocarina.ME_AADL.AADL_Tree.Nodes;  use Ocarina.ME_AADL.AADL_Tree.Nodes;

package Ocarina.ME_AADL.AADL_Tree.Nutils is

   function First_Homonym (N : Node_Id) return Node_Id;
   procedure Set_First_Homonym (N : Node_Id; V : Node_Id);

   procedure Append_List_To_List (S : List_Id; D : in out List_Id);
   pragma Inline (Append_List_To_List);
   --  Append list S to list D, if D does not exist, then D := S

   procedure Push_Node_To_List (E : Node_Id; L : List_Id);
   --  Add node N to the front of list L

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   --  Append node N to list L (to the end of list L)

   procedure Append_Node_To_Node_List
     (Node :        Node_Id;
      List : in out Node_List;
      Once :        Boolean := True);
   --  Append Node to List. If Once true, do not append when Node
   --  already there.

   procedure Replace_Node_To_List
     (List     : List_Id;
      Old_Node : Node_Id;
      New_Node : Node_Id);
   --  Replace Old_Node by New_Node in List.

   procedure Remove_Nodes_From_List (List : in out Node_List);
   --  Clear all links between the nodes of List.

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Remove_Last_Node_From_List (L : List_Id) return Node_Id;
   --  Remove the last node from list L, return the removed node
   --  Return No_Node if L does not exists or is empty

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return true when L is empty

   function Have_Modes (In_Modes : Node_Id) return Boolean;
   --  Return true if the In_Modes node kind 'In_Modes' has at least one
   --  mode.

   function Length (L : List_Id) return Natural;

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   --  Create a new node

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;
   --  Create a new list

   function Make_Identifier
     (Loc          : Location;
      Name         : Name_Id;
      Display_Name : Name_Id;
      Entity       : Node_Id)
     return Node_Id;
   --  Make an identifier

   function Split_Name (N : Node_Id) return List_Id;
   --  Starting from a namespace node N whose name is in the form
   --  n1::n2::..., return a list of identifiers whose names are n1,
   --  n2...Return a null list if N has no name

   function Get_Parent_Package_Name (Pkg : Node_Id) return Name_Id;
   --  Return the name of the parent package of the package
   --  Pkg. Return No_Name if P has no parent package name. The
   --  returned name is in lower case.

   procedure Reset_Nodes;
   --  Reset the table that contains the nodes. Beware that everything
   --  will be lost.

   function Build_Package_Identifier
     (Pack_Name : Types.Node_Id;
      Loc       : Locations.Location := No_Location)
     return Types.Node_Id;
   --  Return a package identifier node from a package name node

end Ocarina.ME_AADL.AADL_Tree.Nutils;
