------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.ME_AADL.AADL_INSTANCES.NUTILS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2008-2009, GET-Telecom Paris.                --
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

with Locations;
with Types;

with Ocarina.ME_AADL.AADL_Instances.Nodes;

package Ocarina.Me_AADL.AADL_Instances.Nutils is

   use Locations;
   use Types;
   use Ocarina.Me_AADL.AADL_Instances.Nodes;

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

   function Length (L : List_Id) return Natural;

   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;
   --  Create a new node

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;
   --  Create a new list

   function Make_Node_Container
     (Item       : Node_Id;
      Extra_Item : Node_Id := No_Node)
     return Node_Id;
   --  Creates a container for the nodes Item and Extra_Item to be able
   --  to put one node in several lists

   procedure Reset_Nodes;
   --  Reset the table that contains the nodes. Beware that everything
   --  will be lost.

   function Compute_Full_Name_Of_Instance
     (Instance         : Node_Id;
      Display_Name     : Boolean := False;
      Keep_Root_System : Boolean := True)
     return Name_Id;
   --  Return the complete name (i.e. containing the names of the
   --  parent components) of a given component instance. If the
   --  Display_Name flag is set, return the display name (any upper
   --  case charecters will be conserved. If Keep_Root_System is set
   --  to false, the heading root system name will be removed from the
   --  full name.

   function Split_Name (N : Node_Id) return List_Id;
   --  Starting from a namespace node N whose name is in the form
   --  n1::n2::..., return a list of identifiers whose names are n1,
   --  n2...Return a null list if N has no name

   function Make_Identifier
     (Loc          : Location;
      Name         : Name_Id;
      Display_Name : Name_Id;
      Entity       : Node_Id)
     return Node_Id;
   --  Make an identifier

   function Copy_Node (Original : Node_Id) return Node_Id;
   --  Copy an instance node. Duplicate an identifier instance tree node.
   --  XXX remove and use Duplicate_Identifier if there is no change
   --  when we instantiate properly feature group.

   function Find_Name_In_List
     (Name_Node : Name_Id;
      List : List_Id)
     return Node_Id;
   --  Find a node with a given name in a list. This function
   --  was introduced to search for a feature located in a
   --  feature group and does not provide advanced search features
   --  at this time.

   --  The routines below are used to check the category of a
   --  component or subcomponent instance.

   function Is_Abstract   (C : Node_Id) return Boolean;
   function Is_Data       (C : Node_Id) return Boolean;
   function Is_Memory     (C : Node_Id) return Boolean;
   function Is_Subprogram (C : Node_Id) return Boolean;
   function Is_Process    (C : Node_Id) return Boolean;
   function Is_Device     (C : Node_Id) return Boolean;
   function Is_Process_Or_Device (C : Node_Id) return Boolean;
   function Is_Thread     (C : Node_Id) return Boolean;
   function Is_System     (C : Node_Id) return Boolean;
   function Is_Processor  (C : Node_Id) return Boolean;
   function Is_Bus        (C : Node_Id) return Boolean;
   function Is_Virtual_Bus (C : Node_Id) return Boolean;

end Ocarina.ME_AADL.AADL_Instances.Nutils;
