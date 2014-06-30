------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . P N . N U T I L S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

with Types;
with Ocarina.Backends.PN.Nodes;

package Ocarina.Backends.PN.Nutils is

   package OPN renames Ocarina.Backends.PN.Nodes;

   function New_Node (Kind : OPN.Node_Kind) return Types.Node_Id;
   --  Create a new node

   function New_List (Kind : OPN.Node_Kind) return Types.List_Id;
   --  Create a new list

   function Is_Empty (L : Types.List_Id) return Boolean;
   --  Return true if the list is empty, else return false

   procedure Append_Node_To_List (E : Types.Node_Id; L : Types.List_Id);
   --  Append the node to the list

   procedure Push_Node_Into_List (E : Types.Node_Id; L : Types.List_Id);

   procedure Delete_Node_From_List (E : Types.Node_Id; L : Types.List_Id);

   function Make_Identifier
     (Pn_Entity  : Types.Node_Id;
      Ident_Name : Types.Name_Id) return Types.Node_Id;
   --  Create a new identifier

end Ocarina.Backends.PN.Nutils;
