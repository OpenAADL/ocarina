------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . P N . N U T I L S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Types;
with Ocarina.Backends.PN.Nodes;

package Ocarina.Backends.PN.Nutils is

   package OPN renames Ocarina.Backends.PN.Nodes;

   function New_Node (Kind : OPN.Node_Kind) return Ocarina.Types.Node_Id;
   --  Create a new node

   function New_List (Kind : OPN.Node_Kind) return Ocarina.Types.List_Id;
   --  Create a new list

   function Is_Empty (L : Ocarina.Types.List_Id) return Boolean;
   --  Return true if the list is empty, else return false

   procedure Append_Node_To_List
     (E : Ocarina.Types.Node_Id; L : Ocarina.Types.List_Id);
   --  Append the node to the list

   procedure Push_Node_Into_List
     (E : Ocarina.Types.Node_Id; L : Ocarina.Types.List_Id);

   procedure Delete_Node_From_List
     (E : Ocarina.Types.Node_Id; L : Ocarina.Types.List_Id);

   function Make_Identifier
     (Pn_Entity  : Ocarina.Types.Node_Id;
      Ident_Name : Ocarina.Types.Name_Id) return Ocarina.Types.Node_Id;
   --  Create a new identifier

end Ocarina.Backends.PN.Nutils;
