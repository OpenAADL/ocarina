------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . M E _ R E A L . R E A L _ T R E E . N U T I L S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

--  Offers list manipulation functions

with Ocarina.Types;
with Locations;
with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.Instances.REAL_Checker.Queries;
with GNAT.Dynamic_Tables;

package Ocarina.ME_REAL.REAL_Tree.Nutils is
   use Ocarina.Types;
   use Locations;
   use Ocarina.ME_REAL.REAL_Tree.Nodes;

   type Node is record
      Node : Node_Id;
   end record;

   package Node_List is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,  -- # of elements
      Table_Increment      => 50); -- % increase

   REAL_Root : Node_Id;
   --  Current theorem root node

   AADL_Model_Root : Node_Id;
   --  Current AADL model root node

   Owner_Node : Node_Id;
   --  Node that is owner of the current theorem

   Environment : List_Id;
   Domain      : Ocarina.Instances.REAL_Checker.Queries.Result_Set;
   Is_Domain   : Boolean;
   --  Values inherited from caller theorems

   Library_Theorems : Node_List.Instance;
   --  List of theorem available that must be registered,
   --  in order to be run by others theorems
   --  (ie. found in REAL files)

   To_Run_Theorem_List : Node_List.Instance;
   --  List of theorem to be run (ie. from annexes)

   procedure Init;
   --  Initialize REAL context

   procedure Reset;
   --  Reinitialize REAL context

   procedure Append_Variable_To_Global_Variables (Node : Node_Id);
   function Find_Global_Variable (Key : Name_Id) return Node_Id;
   --  Global variables handling

   function Find_Declared_Theorem (Theorem_Name : Name_Id) return Node_Id;
   --  Return a theorem root from the declared theorems list

   function New_List (Kind : Node_Kind; Loc : Location) return List_Id;
   function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;

   procedure Replace_Node_To_List
     (List     : List_Id;
      Old_Node : Node_Id;
      New_Node : Node_Id);
   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   function Find_Node_By_Name (Key : Name_Id; Target : List_Id) return Node_Id;
   function Is_Empty (L : List_Id) return Boolean;
   --  List manipulation

end Ocarina.ME_REAL.REAL_Tree.Nutils;
