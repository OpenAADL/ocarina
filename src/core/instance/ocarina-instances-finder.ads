------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . I N S T A N C E S . F I N D E R              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with Ocarina.ME_AADL.AADL_Instances.Nodes;

package Ocarina.Instances.Finder is

   type Node_Kind_Array is array (Positive range <>)
     of Ocarina.ME_AADL.AADL_Instances.Nodes.Node_Kind;

   function Find_Instance
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Path               : List_Id)
     return Node_Id;
   --  Return the instance pointed by Path, from Reference_Instance

   function Find_Instance_In_Instance
     (Instance_Root      : Node_Id;
      Reference_Instance : Node_Id;
      Path               : List_Id)
     return Node_Id;
   --  Return the instance pointed by Path, from Reference_Instance

   function Find_Local_Instance
     (Reference_Instance  : Node_Id;
      Instance_Identifier : Node_Id)
     return Node_Id;
   --  Return the instance having the same name as
   --  Instance_Identifier, or No_Node if no instance of that name can
   --  be found. Reference_Instance is (1) a component instance in
   --  which case the search is performed inside it or (2) a
   --  connection instance in which case the search is performed in
   --  its parent component.

   procedure Find_All_Instances
     (Instance_Root :        Node_Id;
      Kinds         :        Node_Kind_Array;
      First_Node    : in out Node_Id;
      Last_Node     : in out Node_Id);
   --  Search recursively in the instance hierarchy for instances entity
   --  within a provided set of kinds

   procedure Find_All_Flows
     (Instance_Root :        Node_Id;
      First_Node    : in out Node_Id;
      Last_Node     : in out Node_Id);
   --  Search recursively in the instance hierarchy for instances entity
   --  of end to end flows

end Ocarina.Instances.Finder;
