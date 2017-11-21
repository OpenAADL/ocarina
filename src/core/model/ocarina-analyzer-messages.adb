------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . A N A L Y Z E R . M E S S A G E S             --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet;
with Ocarina.Output;

with Ocarina.ME_AADL.AADL_Tree.Debug;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

package body Ocarina.Analyzer.Messages is

   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

   procedure Display_Location_And_Node_Kind
     (Loc  : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind);

   function Image
     (Category : Ocarina.ME_AADL.Component_Category) return String;
   --  Return component category image

   -----------------------
   -- Display_Node_Link --
   -----------------------

   procedure Display_Node_Link (Node1 : Node_Id; Node2 : Node_Id) is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

   begin
      if D_Analyzer then
         Set_Standard_Error;
         Write_Str ("ANL: ");

         if Present (Node1) then
            Write_Str (Image (Loc (Node1)));
         else
            Write_Str ("no loc");
         end if;

         Write_Str (" -> ");

         if Present (Node2) then
            Write_Line (Image (Loc (Node2)));
         else
            Write_Line ("no loc");
         end if;

         Set_Standard_Output;
      end if;
   end Display_Node_Link;

   ----------------------------
   -- Display_Analyzer_Error --
   ----------------------------

   procedure Display_Analyzer_Error
     (Node1    : Node_Id;
      Message1 : String;
      Node2    : Node_Id  := No_Node;
      Message2 : String   := "";
      Message0 : String   := "";
      Loc      : Location := No_Location)
   is
      use Ocarina.ME_AADL.AADL_Tree.Debug;
      use Ocarina.Namet;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Output;
      use Ocarina.ME_AADL;

   begin
      Set_Standard_Error;

      if Loc /= No_Location then
         Write_Str (Image (Loc) & ": ");
      else
         if Present (Node1) then
            Write_Str
              (Image (Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Node1)) & ": ");
         end if;
      end if;

      if Message0 /= "" then
         Write_Str (" (" & Message0 & ") ");
      end if;

      if Present (Node1) then
         if Get_Entity_Category (Node1) /= EC_Undefined then
            Write_Name (Get_Name_Of_Entity (Node1, Get_Display_Name => True));
         elsif Kind (Node1) = K_Entity_Reference then
            Write_Name
              (Get_Name_Of_Entity_Reference (Node1, Get_Display_Name => True));
         elsif Kind (Node1) = K_Identifier then
            Write_Name (Display_Name (Node1));
         end if;

         Write_Str (" (" & Image (Kind (Node1)) & ") ");
      end if;

      Write_Str (Message1);

      if Present (Node2) then
         if Get_Entity_Category (Node2) /= EC_Undefined then
            Write_Name (Get_Name_Of_Entity (Node2, Get_Display_Name => True));
         elsif Kind (Node2) = K_Entity_Reference then
            Write_Name
              (Get_Name_Of_Entity_Reference (Node2, Get_Display_Name => True));

         elsif Kind (Node2) = K_Identifier then
            Write_Name (Name (Node2));
         end if;

         Write_Str (" (" & Image (Kind (Node2)) & ") ");
      end if;

      Write_Line (Message2);
      Set_Standard_Output;
   end Display_Analyzer_Error;

   ----------------------------------
   -- Display_Conflict_Declaration --
   ----------------------------------

   procedure Display_Conflict_Declaration
     (Loc1 : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Name : Name_Id;
      Loc2 : Location)
   is
      use Ocarina.Namet;
      use Ocarina.Output;

   begin
      Set_Standard_Error;

      Display_Location_And_Node_Kind (Loc1, Kind);
      Write_Str (" '");
      Write_Name (Name);
      Write_Str ("' conflicts with declaration at ");
      Write_Line (Image (Loc2));

      Set_Standard_Output;
   end Display_Conflict_Declaration;

   ----------------------------------
   -- Display_Conflict_Declaration --
   ----------------------------------

   procedure Display_Conflict_Declaration
     (Ident1 : Node_Id;
      Ident2 : Node_Id)
   is
      use Ocarina.Namet;
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

   begin
      Set_Standard_Error;

      Display_Location_And_Node_Kind
        (Loc (Ident1),
         Kind (Corresponding_Entity (Ident1)));
      Write_Str (" '");
      Write_Name (Display_Name (Ident1));
      Write_Str ("' conflicts with declaration ");

      Write_Str
        (Ocarina.ME_AADL.AADL_Tree.Debug.Image
           (Kind (Corresponding_Entity (Ident2))));
      Write_Str (" '");
      Write_Name (Display_Name (Ident2));
      Write_Str ("' at ");

      Write_Line (Image (Loc (Ident2)));

      Set_Standard_Output;
   end Display_Conflict_Declaration;

   ------------------------------------
   -- Display_Location_And_Node_Kind --
   ------------------------------------

   procedure Display_Location_And_Node_Kind
     (Loc  : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind)
   is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Debug;
   begin
      Write_Str (Image (Loc));
      Write_Str (": ");
      Write_Str (Ocarina.ME_AADL.AADL_Tree.Debug.Image (Kind));
   end Display_Location_And_Node_Kind;

   ----------------------------
   -- Display_Undefined_Item --
   ----------------------------

   procedure Display_Undefined_Item
     (Loc  : Location;
      Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Name : Name_Id)
   is
      use Ocarina.Namet;
      use Ocarina.Output;

   begin
      Set_Standard_Error;

      Display_Location_And_Node_Kind (Loc, Kind);
      Write_Str (" '");
      Write_Name (Name);
      Write_Line ("' is undefined or not visible");

      Set_Standard_Output;
   end Display_Undefined_Item;

   ----------------------------
   -- Display_Undefined_Item --
   ----------------------------
   procedure Display_Undefined_Item
     (Kind : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Item : Node_Id)
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Output;
      use Ocarina.Namet;

   begin
      Set_Standard_Error;

      Display_Location_And_Node_Kind (Loc (Item), Kind);
      Write_Str (" '");
      Write_Str (Get_Name_String (Display_Name (Item)));
      Write_Line ("' is undefined or not visible");

      Set_Standard_Output;
   end Display_Undefined_Item;

   -------------------------------------------
   -- Display_Unexpected_Component_Category --
   -------------------------------------------

   procedure Display_Unexpected_Component_Category
     (Expected_Cat : Ocarina.ME_AADL.Component_Category;
      Found_Cat    : Ocarina.ME_AADL.Component_Category;
      Item         : Node_Id;
      Loc          : Location)
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Output;
      use Ocarina.Namet;

   begin
      Set_Standard_Error;

      Write_Str (Image (Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Item)));
      Write_Str (": expected component category ");
      Write_Str (Image (Expected_Cat));
      Write_Str (", found ");
      Write_Str (Image (Found_Cat));
      Write_Str (" '");
      Write_Str (Get_Name_String (Display_Name (Item)));
      Write_Str ("' defined at ");
      Write_Line (Image (Loc));

      Set_Standard_Output;
   end Display_Unexpected_Component_Category;

   -----------------------------
   -- Display_Unexpected_Type --
   -----------------------------

   procedure Display_Unexpected_Type
     (Expected_Type : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Found_Type    : Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;
      Item          : Node_Id;
      Loc           : Location)
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Output;
      use Ocarina.Namet;

   begin
      Set_Standard_Error;

      Write_Str (Image (Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Item)));
      Write_Str (": expected type ");
      Write_Str (Ocarina.ME_AADL.AADL_Tree.Debug.Image (Expected_Type));
      Write_Str (", found type ");
      Write_Str (Ocarina.ME_AADL.AADL_Tree.Debug.Image (Found_Type));
      Write_Str (" '");
      Write_Str (Get_Name_String (Display_Name (Item)));
      Write_Str ("' defined at ");
      Write_Line (Image (Loc));

      Set_Standard_Output;
   end Display_Unexpected_Type;

   -----------
   -- Image --
   -----------

   function Image
     (Category : Ocarina.ME_AADL.Component_Category) return String
   is
      use Ocarina.ME_AADL;
   begin
      case Category is
         when CC_Abstract =>
            return "Abstract";
         when CC_Data =>
            return "Data";
         when CC_Subprogram =>
            return "Subprogram";
         when CC_Subprogram_Group =>
            return "Subprogram Group";
         when CC_Thread =>
            return "Thread";
         when CC_Thread_Group =>
            return "Thread Group";
         when CC_Process =>
            return "Process";
         when CC_Memory =>
            return "Memory";
         when CC_Processor =>
            return "Processor";
         when CC_Virtual_Processor =>
            return "Virtual Processor";
         when CC_Virtual_Bus =>
            return "Virtual Bus";
         when CC_Bus =>
            return "Bus";
         when CC_Device =>
            return "Device";
         when CC_System =>
            return "System";
         when CC_Unknown =>
            return "UNKNOWN";
      end case;
   end Image;

   -------------------
   -- Debug_Message --
   -------------------

   procedure Debug_Message (Location : String; Message : String) is
      use Ocarina.Output;
   begin
      if D_Analyzer then
         Set_Standard_Error;

         Write_Str ("ANL: ");
         Write_Str (Location & ": ");
         Write_Line (Message);

         Set_Standard_Output;
      end if;
   end Debug_Message;

   ------------------------------
   -- Display_Cyclic_Extension --
   ------------------------------

   procedure Display_Cyclic_Extension (Cycling_Node : Node_Id) is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Present (Cycling_Node));
   begin
      Set_Standard_Error;

      Display_Location_And_Node_Kind (Loc (Cycling_Node), Kind (Cycling_Node));
      Write_Line (" creates a circular extension ");

      Set_Standard_Output;
   end Display_Cyclic_Extension;

   ------------------------------
   -- Display_Cyclic_Inversion --
   ------------------------------

   procedure Display_Cyclic_Inversion (Cycling_Node : Node_Id) is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Present (Cycling_Node));
   begin
      Set_Standard_Error;

      Display_Location_And_Node_Kind (Loc (Cycling_Node), Kind (Cycling_Node));
      Write_Line (" creates a cycle in port group inversions");

      Set_Standard_Output;
   end Display_Cyclic_Inversion;

   ----------------------------------
   -- Display_Cyclic_Subcomponents --
   ----------------------------------

   procedure Display_Cyclic_Subcomponents (Cycling_Node : Node_Id) is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Present (Cycling_Node));
   begin
      Set_Standard_Error;

      Display_Location_And_Node_Kind (Loc (Cycling_Node), Kind (Cycling_Node));
      Write_Line (" creates a cycle in subcomponent declarations");

      Set_Standard_Output;
   end Display_Cyclic_Subcomponents;

   ---------------------------------------
   -- Display_Conflicting_Initial_Modes --
   ---------------------------------------

   procedure Display_Conflicting_Initial_Modes
     (Initial_Mode          : Node_Id;
      Existing_Initial_Mode : Node_Id)
   is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Initial_Mode) = K_Mode);
      pragma Assert (Kind (Initial_Mode) = K_Mode);

   begin
      Set_Standard_Error;

      Write_Str (Image (Loc (Initial_Mode)));
      Write_Str (": initial mode conflicts with another one at ");
      Write_Line (Image (Loc (Existing_Initial_Mode)));

      Set_Standard_Output;
   end Display_Conflicting_Initial_Modes;

   --------------------------------
   -- Display_Link_To_Wrong_Node --
   --------------------------------

   procedure Display_Link_To_Wrong_Node
     (Node         : Node_Id;
      Pointed_Node : Node_Id;
      Warning      : Boolean := False;
      Non_Existent : Boolean := False)
   is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Namet;
      use Ocarina.ME_AADL.AADL_Tree.Debug;
      use Ocarina.Output;

      pragma Assert (Present (Node));

   begin
      Set_Standard_Error;
      Write_Str (Image (Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Node)) & ": ");

      if Warning then
         Write_Str ("warning: ");
      end if;

      if Get_Entity_Category (Node) /= EC_Undefined then
         Write_Name (Get_Name_Of_Entity (Node, Get_Display_Name => True));
      elsif Kind (Node) = K_Entity_Reference then
         Write_Name
           (Get_Name_Of_Entity_Reference (Node, Get_Display_Name => True));
      end if;

      Write_Str (" (" & Image (Kind (Node)) & ")");

      if Present (Pointed_Node) then
         Write_Str (" points to ");

         if Get_Entity_Category (Pointed_Node) /= EC_Undefined then
            Write_Name
              (Get_Name_Of_Entity (Pointed_Node, Get_Display_Name => True));
         elsif Kind (Pointed_Node) = K_Entity_Reference then
            Write_Name
              (Get_Name_Of_Entity_Reference
                 (Pointed_Node,
                  Get_Display_Name => True));
         end if;
         Write_Str (" (" & Image (Kind (Pointed_Node)) & ")");

         if Non_Existent then
            Write_Str (", which does not exist. ");
            if Kind (Pointed_Node) = K_Entity_Reference then
               Is_Bad_Spelling
                 (Get_Name_Of_Entity_Reference
                    (Pointed_Node,
                     Get_Display_Name => True));
            end if;
         else
            Write_Str (", which is not of an adequate kind");
         end if;
      else
         Write_Str (" does not point to anything or to something unreachable");
      end if;

      Write_Eol;
      Set_Standard_Output;
   end Display_Link_To_Wrong_Node;

   -----------------------------------------
   -- Display_Incompatible_Property_Types --
   -----------------------------------------

   procedure Display_Incompatible_Property_Types
     (Property_Association : Node_Id;
      Property_Value       : Node_Id;
      Property_Name        : Node_Id)
   is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Property_Name) = K_Property_Definition_Declaration);
      pragma Assert (Present (Property_Value));

   begin
      Set_Standard_Error;

      Write_Str (Image (Loc (Property_Value)));
      Write_Str (": when evaluating the value of ");
      Write_Str (Get_Name_Of_Entity (Property_Association));

      Write_Str (", the value");

      if Kind (Property_Value) /= K_Property_Value then
         Write_Str
           (" (" &
            Property_Type'Image (Get_Type_Of_Property_Value (Property_Value)) &
            ")");
         --  We only display the type if the property value is explicit
      end if;

      Write_Str (" is not conformant with declaration at ");
      Write_Str (Image (Loc (Property_Name)));
      Write_Line
        (" (" &
         Property_Type'Image (Get_Type_Of_Property (Property_Name)) &
         ")");

      Set_Standard_Output;
   end Display_Incompatible_Property_Types;

   ----------------------------------------
   -- Display_Inconsistent_Property_Type --
   ----------------------------------------

   procedure Display_Inconsistent_Property_Type (Property_Type : Node_Id) is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Present (Property_Type));
   begin
      Set_Standard_Error;
      Write_Str (Image (Loc (Property_Type)));
      Write_Line (": property type is inconsistent");
      Set_Standard_Output;
   end Display_Inconsistent_Property_Type;

   ----------------------------------------------
   -- Display_Inconsistency_In_Property_Values --
   ----------------------------------------------

   procedure Display_Inconsistency_In_Property_Values
     (Property_Value1    : Node_Id;
      Property_Value2    : Node_Id;
      Reference_Property : Node_Id)
   is
      use Ocarina.Output;
      use Ocarina.Namet;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Debug;

      pragma Assert (Present (Property_Value1));
      pragma Assert (Present (Property_Value2));
      pragma Assert
        (No (Reference_Property)
         or else Kind (Reference_Property) = K_Property_Association
         or else Kind (Reference_Property) = K_Property_Definition_Declaration
         or else Kind (Reference_Property) = K_Property_Type_Declaration
         or else Kind (Reference_Property) = K_Constant_Property_Declaration);
   begin
      Set_Standard_Error;
      Write_Str (Image (Loc (Reference_Property)));
      Write_Str (": when evaluating the value of ");
      Write_Name (Get_Name_Of_Entity (Reference_Property));
      Write_Str (", the value at ");
      Write_Str (Image (Loc (Property_Value1)));
      Write_Str (" (");

      if Kind (Property_Value1) = K_List_Id then
         if First_Node (List_Id (Property_Value1)) /= No_Node then
            Write_Str ("list of ");
            Write_Str (Image (Kind (First_Node (List_Id (Property_Value1)))));
         else
            Write_Str ("list");
         end if;
      else
         if Next_Node (Property_Value1) /= No_Node then
            Write_Str ("list of ");
         end if;

         Write_Str (Image (Kind (Property_Value1)));
      end if;

      Write_Str (") is inconsistent with " & "the other one declared at ");
      Write_Str (Image (Loc (Property_Value2)));
      Write_Str (" (");

      if Kind (Property_Value2) = K_List_Id then
         if First_Node (List_Id (Property_Value2)) /= No_Node then
            Write_Str ("list of ");
            Write_Str (Image (Kind (First_Node (List_Id (Property_Value2)))));
         else
            Write_Str ("list");
         end if;
      else
         if Next_Node (Property_Value2) /= No_Node then
            Write_Str ("list of ");
         end if;

         Write_Str (Image (Kind (Property_Value2)));
      end if;

      Write_Line (")");
      Set_Standard_Output;
   end Display_Inconsistency_In_Property_Values;

   ---------------------------------------
   -- Display_Property_List_Discrepancy --
   ---------------------------------------

   procedure Display_Property_List_Discrepancy
     (Property_Association : Node_Id;
      Property_Name        : Node_Id)
   is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Property_Name) = K_Property_Definition_Declaration);
      pragma Assert (Kind (Property_Association) = K_Property_Association);

   begin
      Set_Standard_Error;

      Write_Str (Image (Loc (Property_Association)));
      Write_Str (": ");
      Write_Str (Get_Name_Of_Entity (Property_Association));

      if Type_Of_Property_Is_A_List (Property_Association) then
         Write_Str (" is a list while the corresponding property name at ");
      else
         Write_Str
           (" is not a list while the corresponding property name at ");
      end if;

      Write_Str (Image (Loc (Property_Name)));

      if Type_Of_Property_Is_A_List (Property_Name) then
         Write_Line (" is a list.");
      else
         Write_Line (" is not a list.");
      end if;

      Set_Standard_Output;
   end Display_Property_List_Discrepancy;

   -----------------------------------------
   -- Display_Conversion_To_Property_List --
   -----------------------------------------

   procedure Display_Conversion_To_Property_List
     (Property_Association : Node_Id;
      Property_Name        : Node_Id)
   is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Property_Name) = K_Property_Definition_Declaration);
      pragma Assert (Kind (Property_Association) = K_Property_Association);

   begin
      Set_Standard_Error;

      Write_Str (Image (Loc (Property_Association)));
      Write_Str (": Warning: ");
      Write_Str (Get_Name_Of_Entity (Property_Association));
      Write_Str (" is not a list while the corresponding property name at ");
      Write_Str (Image (Loc (Property_Name)));
      Write_Line (" is a list.");
      Write_Str (Image (Loc (Property_Association)));
      Write_Str (": Warning: ");
      Write_Str ("The value of ");
      Write_Str (Get_Name_Of_Entity (Property_Association));
      Write_Line (" has been converted into a list.");

      Set_Standard_Output;
   end Display_Conversion_To_Property_List;

   -------------------------------------
   -- Display_Property_Not_Applicable --
   -------------------------------------

   procedure Display_Property_Not_Applicable
     (Property_Association : Node_Id;
      Entity               : Node_Id)
   is
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Kind (Property_Association) = K_Property_Association);
      pragma Assert (Present (Entity));
   begin
      Set_Standard_Error;

      Write_Str (Image (Loc (Property_Association)));
      Write_Str (": ");
      Write_Str (Get_Name_Of_Entity (Property_Association));
      Write_Str (" cannot apply to ");
      Write_Line (Get_Name_Of_Entity (Entity));

      Set_Standard_Output;
   end Display_Property_Not_Applicable;

end Ocarina.Analyzer.Messages;
