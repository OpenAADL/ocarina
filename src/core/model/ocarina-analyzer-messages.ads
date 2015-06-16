------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . A N A L Y Z E R . M E S S A G E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2004-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Locations;

with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL;

package Ocarina.Analyzer.Messages is

   use Locations;

   procedure Debug_Message (Location : String; Message : String);
   --  Displays Message, specifying it was emitted from Location

   procedure Display_Node_Link (Node1 : Node_Id; Node2 : Node_Id);

   procedure Display_Analyzer_Error
     (Node1    : Node_Id;
      Message1 : String;
      Node2    : Node_Id  := No_Node;
      Message2 : String   := "";
      Message0 : String   := "";
      Loc      : Location := No_Location);

   procedure DAE
     (Node1    : Node_Id;
      Message1 : String;
      Node2    : Node_Id  := No_Node;
      Message2 : String   := "";
      Message0 : String   := "";
      Loc      : Location := No_Location) renames
     Display_Analyzer_Error;

   Impl_Separator    : constant String := ".";
   Package_Separator : constant String := "::";

   procedure Display_Conflict_Declaration
     (Loc1 : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Name : Name_Id;
      Loc2 : Location);
   procedure DCD
     (Loc1 : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Name : Name_Id;
      Loc2 : Location) renames
     Display_Conflict_Declaration;
   --  Display an output error message:
   --     Loc1: ... Name_Id conflicts with declaration at <loc2>

   procedure Display_Conflict_Declaration (Ident1 : Node_Id; Ident2 : Node_Id);
   procedure DCD
     (Node1 : Node_Id;
      Node2 : Node_Id) renames
     Display_Conflict_Declaration;
   --  Display an output error message:
   --     Loc1: <Entity_Type1> <Name1> conflicts with declaration
   --           <Entity_Type2> <Name2> at <Loc2>

   --  NOTE: Ident1 and Ident2 must be identifier nodes

   procedure Display_Undefined_Item
     (Loc  : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Name : Name_Id);
   procedure DUI
     (Loc  : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Name : Name_Id) renames
     Display_Undefined_Item;
   --  Display an output error message:
   --     Loc: <Kind> '<Name>' is undefined

   procedure Display_Undefined_Item
     (Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Item : Node_Id);

   procedure DUI
     (Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Item : Node_Id) renames
     Display_Undefined_Item;
   --  Display an output error message:
   --     Loc (Item): <Kind> Item_Name is undefined
   --  NOTE: Item can be an Identifier, Identifier_With_Package_Name

   procedure Display_Unexpected_Type
     (Expected_Type : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Found_Type    : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Item          : Node_Id;
      Loc           : Location);
   procedure DUT
     (Expected_Type : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Found_Type    : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Item          : Node_Id;
      Loc           : Location) renames
     Display_Unexpected_Type;
   --  Display an output error message:
   --     Loc (Item): expected type <Expected_Type>, found type <Found_Type>
   --                 Item_Name defined at <Loc>

   procedure Display_Unexpected_Component_Category
     (Expected_Cat : Ocarina.ME_AADL.Component_Category;
      Found_Cat    : Ocarina.ME_AADL.Component_Category;
      Item         : Node_Id;
      Loc          : Location);
   procedure DUCC
     (Expected_Cat : Ocarina.ME_AADL.Component_Category;
      Found_Cat    : Ocarina.ME_AADL.Component_Category;
      Item         : Node_Id;
      Loc          : Location) renames
     Display_Unexpected_Component_Category;
   --  Display an output error message:
   --     Loc (Item): expected component category <Expected_Cat>, found
   --                 <Found_Cat> Item_Name defined at <Loc>

   procedure Display_Cyclic_Extension (Cycling_Node : Node_Id);
   procedure Display_Cyclic_Inversion (Cycling_Node : Node_Id);
   procedure Display_Cyclic_Subcomponents (Cycling_Node : Node_Id);

   procedure Display_Conflicting_Initial_Modes
     (Initial_Mode          : Node_Id;
      Existing_Initial_Mode : Node_Id);

   procedure Display_Link_To_Wrong_Node
     (Node         : Node_Id;
      Pointed_Node : Node_Id;
      Warning      : Boolean := False;
      Non_Existent : Boolean := False);
   procedure DLTWN
     (Node         : Node_Id;
      Pointed_Node : Node_Id;
      Warning      : Boolean := False;
      Non_Existent : Boolean := False) renames
     Display_Link_To_Wrong_Node;

   procedure Display_Incompatible_Property_Types
     (Property_Association : Node_Id;
      Property_Value       : Node_Id;
      Property_Name        : Node_Id);

   procedure Display_Inconsistent_Property_Type (Property_Type : Node_Id);

   procedure Display_Inconsistency_In_Property_Values
     (Property_Value1    : Node_Id;
      Property_Value2    : Node_Id;
      Reference_Property : Node_Id);

   procedure Display_Property_List_Discrepancy
     (Property_Association : Node_Id;
      Property_Name        : Node_Id);

   procedure Display_Conversion_To_Property_List
     (Property_Association : Node_Id;
      Property_Name        : Node_Id);

   procedure Display_Property_Not_Applicable
     (Property_Association : Node_Id;
      Entity               : Node_Id);

   function Display_Node_Kind_Error
     (Node : Node_Id) return Boolean renames
     Ocarina.ME_AADL.AADL_Tree.Entities.Display_Node_Kind_Error;

   function DNKE
     (Node : Node_Id) return Boolean renames
     Ocarina.ME_AADL.AADL_Tree.Entities.Display_Node_Kind_Error;

private
   D_Analyzer : constant Boolean := False;

end Ocarina.Analyzer.Messages;
