------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . M E _ A A D L . A A D L _ T R E E . E N T I T I E S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Debug;

package body Ocarina.ME_AADL.AADL_Tree.Entities is

   --
   --  This following section is relative to Entities
   --

   -------------------------
   -- Get_Entity_Category --
   -------------------------

   function Get_Entity_Category
     (Node : Ocarina.Types.Node_Id) return Entity_Category
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
   begin
      case Kind (Node) is
         when K_Subcomponent =>
            return EC_Subcomponent;

         when K_Flow_Spec =>
            return EC_Flow_Spec;

         when K_Flow_Implementation         |
           K_End_To_End_Flow_Spec           |
           K_Flow_Implementation_Refinement |
           K_End_To_End_Flow_Refinement     =>
            return EC_Flow_Implementation;

         when K_Mode | K_Mode_Transition =>
            return EC_Mode;

         when K_Connection =>
            return EC_Connection;

         when K_Property_Association =>
            return EC_Property_Association;

         when K_Subprogram_Call_Sequence =>
            return EC_Call_Sequence;

         when K_Subprogram_Call =>
            return EC_Subprogram_Call;

         when K_Port_Spec        |
           K_Feature_Group_Spec  |
           K_Subprogram_Spec     |
           K_Parameter           |
           K_Subcomponent_Access =>
            return EC_Feature;

         when K_Component_Type | K_Component_Implementation =>
            return EC_Component;

         when K_Feature_Group_Type =>
            return EC_Feature_Group_Type;

         when K_Package_Specification =>
            return EC_Package;

         when K_Property_Set =>
            return EC_Property_Set;

         when K_Property_Definition_Declaration =>
            return EC_Property_Name;

         when K_Property_Type_Declaration =>
            return EC_Property_Type;

         when K_Annex_Library | K_Annex_Subclause =>
            return EC_Annex;

         when others =>
            return EC_Undefined;
      end case;
   end Get_Entity_Category;

   ------------------------
   -- Get_Name_Of_Entity --
   ------------------------

   function Get_Name_Of_Entity
     (Entity           : Ocarina.Types.Node_Id;
      Get_Display_Name : Boolean := True;
      Fully_Qualified  : Boolean := False) return Ocarina.Types.Name_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Namet;
      use Ocarina.Types;

      pragma Assert
        (Kind (Entity) = K_Component_Implementation
         or else Kind (Entity) = K_Feature_Group_Spec
         or else Kind (Entity) = K_Component_Type
         or else Kind (Entity) = K_Feature_Group_Type
         or else Kind (Entity) = K_Subcomponent
         or else Kind (Entity) = K_Mode
         or else Kind (Entity) = K_Connection
         or else Kind (Entity) = K_Flow_Spec
         or else Kind (Entity) = K_Flow_Implementation
         or else Kind (Entity) = K_Flow_Implementation_Refinement
         or else Kind (Entity) = K_End_To_End_Flow_Spec
         or else Kind (Entity) = K_End_To_End_Flow_Refinement
         or else Kind (Entity) = K_Subprogram_Call
         or else Kind (Entity) = K_Subprogram_Call_Sequence
         or else Kind (Entity) = K_Port_Spec
         or else Kind (Entity) = K_Parameter
         or else Kind (Entity) = K_Subcomponent_Access
         or else Kind (Entity) = K_Subprogram_Spec
         or else Kind (Entity) = K_Property_Association
         or else Kind (Entity) = K_Property_Type_Declaration
         or else Kind (Entity) = K_Property_Definition_Declaration
         or else Kind (Entity) = K_Package_Specification
         or else Kind (Entity) = K_AADL_Specification
         or else Kind (Entity) = K_Property_Set
         or else Kind (Entity) = K_Annex_Subclause
         or else Kind (Entity) = K_Annex_Library
         or else DNKE (Entity));

      Entity_Name : Name_Id := No_Name;
      Package_Name : Name_Id;

      Name_Of_Entity : Name_Id := No_Name;
   begin
      if Kind (Entity) /= K_AADL_Specification
        and then Identifier (Entity) /= No_Node
      then
         if Get_Display_Name then
            Entity_Name := Display_Name (Identifier (Entity));
         else
            Entity_Name := Name (Identifier (Entity));
         end if;
      else
         return No_Name;
      end if;

      if Fully_Qualified and then AADL_Version = AADL_V2 then
         Package_Name :=
           Display_Name (Identifier (Namespace (Entity)));

         Set_Str_To_Name_Buffer ("");
         Set_Str_To_Name_Buffer
           (Get_Name_String (Package_Name) &
              "::" &
              Get_Name_String (Entity_Name));
         Name_Of_Entity := Name_Find;
      else
         Name_Of_Entity := Entity_Name;
      end if;

      return Name_Of_Entity;
   end Get_Name_Of_Entity;

   function Get_Name_Of_Entity
     (Entity           : Ocarina.Types.Node_Id;
      Get_Display_Name : Boolean := True;
      Fully_Qualified  : Boolean := False) return String
   is
      use Ocarina.Types;
      use Ocarina.Namet;

      Name : constant Name_Id := Get_Name_Of_Entity
        (Entity, Get_Display_Name, Fully_Qualified);
   begin
      if Name /= No_Name then
         return Get_Name_String (Name);
      else
         return "";
      end if;
   end Get_Name_Of_Entity;

   ----------------------------------
   -- Get_Name_Of_Entity_Reference --
   ----------------------------------

   function Get_Name_Of_Entity_Reference
     (Entity_Ref       : Ocarina.Types.Node_Id;
      Get_Display_Name : Boolean := True) return Ocarina.Types.Name_Id
   is
      use Ocarina.Types;
      use Ocarina.Namet;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert
        (Kind (Entity_Ref) = K_Entity_Reference
         or else Kind (Entity_Ref) = K_Property_Term
         or else Kind (Entity_Ref) = K_Reference_Term
         or else DNKE (Entity_Ref));
   begin
      if Identifier (Entity_Ref) /= No_Node then
         if Namespace_Identifier (Entity_Ref) /= No_Node then
            if Get_Display_Name then
               Get_Name_String
                 (Display_Name (Namespace_Identifier (Entity_Ref)));
               Add_Str_To_Name_Buffer ("::");
               Get_Name_String_And_Append
                 (Display_Name (Identifier (Entity_Ref)));
               return Name_Find;
            else
               Get_Name_String (Name (Namespace_Identifier (Entity_Ref)));
               Add_Str_To_Name_Buffer ("::");
               Get_Name_String_And_Append (Name (Identifier (Entity_Ref)));
               return Name_Find;
            end if;
         else
            if Get_Display_Name then
               return Display_Name (Identifier (Entity_Ref));
            else
               return Name (Identifier (Entity_Ref));
            end if;
         end if;
      else
         return No_Name;
      end if;
   end Get_Name_Of_Entity_Reference;

   ----------------------------------
   -- Get_Name_Of_Entity_Reference --
   ----------------------------------

   function Get_Name_Of_Entity_Reference
     (Entity_Ref       : Ocarina.Types.Node_Id;
      Get_Display_Name : Boolean := True) return String
   is
      use Ocarina.Namet;
      use Ocarina.Types;

      Name : constant Name_Id :=
        Get_Name_Of_Entity_Reference (Entity_Ref, Get_Display_Name);
   begin
      if Name /= No_Name then
         return Get_Name_String (Name);
      else
         return "";
      end if;
   end Get_Name_Of_Entity_Reference;

   ---------------------------
   -- Get_Referenced_Entity --
   ---------------------------

   function Get_Referenced_Entity
     (Entity_Ref : Ocarina.Types.Node_Id) return Ocarina.Types.Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Types;

      pragma Assert
        (Kind (Entity_Ref) = K_Entity_Reference
         or else Kind (Entity_Ref) = K_Reference_Term
         or else Kind (Entity_Ref) = K_Enumeration_Term
         or else Kind (Entity_Ref) = K_Contained_Element_Path
         or else Kind (Entity_Ref) = K_Property_Term
         or else Kind (Entity_Ref) = K_Unique_Property_Const_Identifier
         or else Kind (Entity_Ref) = K_Unique_Property_Type_Identifier
         or else Kind (Entity_Ref) = K_Component_Classifier_Term
         or else DNKE (Entity_Ref));
   begin
      if Kind (Entity_Ref) = K_Reference_Term then
         return Entity (Reference_Term (Entity_Ref));

      elsif Present (Entity (Entity_Ref))
        and then Kind (Entity (Entity_Ref)) = K_Subprogram_Spec
        and then Present
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Entity_Ref (Entity (Entity_Ref)))
      then
         return Entity
             (Ocarina.ME_AADL.AADL_Tree.Nodes.Entity_Ref
                (Entity (Entity_Ref)));
      else
         return Entity (Entity_Ref);
      end if;
   end Get_Referenced_Entity;

   ---------------------------
   -- Set_Referenced_Entity --
   ---------------------------

   procedure Set_Referenced_Entity
     (Entity_Ref, Entity : Ocarina.Types.Node_Id) is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Types;

      pragma Assert
        (Entity_Ref /= No_Node
         and then
         (Kind (Entity_Ref) = K_Entity_Reference
          or else Kind (Entity_Ref) = K_Reference_Term
          or else Kind (Entity_Ref) = K_Contained_Element_Path
          or else Kind (Entity_Ref) = K_Enumeration_Term
          or else Kind (Entity_Ref) = K_Property_Term
          or else Kind (Entity_Ref) = K_Unique_Property_Const_Identifier
          or else Kind (Entity_Ref) = K_Unique_Property_Type_Identifier
          or else Kind (Entity_Ref) = K_Component_Classifier_Term
          or else DNKE (Entity_Ref)));
   begin
      Set_Entity (Entity_Ref, Entity);
   end Set_Referenced_Entity;

   ------------------------------------------------
   -- Entity_Reference_Path_Has_Several_Elements --
   ------------------------------------------------

   function Entity_Reference_Path_Has_Several_Elements
     (Entity_Ref : Ocarina.Types.Node_Id) return Boolean
   is
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert
        (Entity_Ref /= No_Node
         and then (Kind (Entity_Ref) = K_Entity_Reference));
   begin
      return Path (Entity_Ref) /= No_List
        and then First_Node (Path (Entity_Ref)) /= No_Node
        and then Next_Node (First_Node (Path (Entity_Ref))) /= No_Node;
   end Entity_Reference_Path_Has_Several_Elements;

   --------------------------
   -- Duplicate_Identifier --
   --------------------------

   function Duplicate_Identifier
     (Identifier : Ocarina.Types.Node_Id) return Ocarina.Types.Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Identifier = No_Node or else Kind (Identifier) = K_Identifier);

      Duplicate : Node_Id;
   begin
      if Identifier = No_Node then
         return No_Node;
      else
         Duplicate := New_Node (K_Identifier, Loc (Identifier));
         Set_Name (Duplicate, Name (Identifier));
         Set_Display_Name (Duplicate, Display_Name (Identifier));
         Set_Corresponding_Entity
           (Duplicate,
            Corresponding_Entity (Identifier));
         return Duplicate;
      end if;
   end Duplicate_Identifier;

   --
   --  This following section is relative to Entities Components
   --

   -------------------------------
   -- Get_Category_Of_Component --
   -------------------------------

   function Get_Category_Of_Component
     (Component : Ocarina.Types.Node_Id) return Component_Category
   is
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type);
   begin
      return Component_Category'Val (Category (Component));
   end Get_Category_Of_Component;

   --
   --  This following section is relative to Entities Components Connections
   --

   --------------------------------
   -- Get_Category_Of_Connection --
   --------------------------------

   function Get_Category_Of_Connection
     (Connection : Ocarina.Types.Node_Id) return Connection_Type
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Types;

      pragma Assert
        (Connection /= No_Node
         and then
         (Kind (Connection) = K_Connection or else DNKE (Connection)));
   begin
      return Connection_Type'Val (Category (Connection));
   end Get_Category_Of_Connection;

   --
   --  This following section is relative to Entities Components Flows
   --

   --------------------------
   -- Get_Category_Of_Flow --
   --------------------------

   function Get_Category_Of_Flow (Flow : Ocarina.Types.Node_Id)
                                 return Flow_Category is
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert
        (Flow /= No_Node
         and then
         (Kind (Flow) = K_Flow_Spec
          or else Kind (Flow) = K_Flow_Implementation));

   begin
      return Flow_Category'Val (Category (Flow));
   end Get_Category_Of_Flow;

   --
   --  This following section is relative to Entities Components Subcomponents
   --

   ----------------------------------
   -- Get_Category_Of_Subcomponent --
   ----------------------------------

   function Get_Category_Of_Subcomponent
     (Subcomponent : Ocarina.Types.Node_Id) return Component_Category
   is
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert
        (Subcomponent /= No_Node
         and then Kind (Subcomponent) = K_Subcomponent);
   begin
      return Component_Category'Val (Category (Subcomponent));
   end Get_Category_Of_Subcomponent;

   ---------------------------------
   -- Get_Corresponding_Component --
   ---------------------------------

   function Get_Corresponding_Component
     (Subcomponent : Ocarina.Types.Node_Id) return Ocarina.Types.Node_Id
   is
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert
        (Subcomponent /= No_Node
         and then Kind (Subcomponent) = K_Subcomponent);
   begin
      if Entity_Ref (Subcomponent) /= No_Node then
         return Get_Referenced_Entity (Entity_Ref (Subcomponent));
      else
         return No_Node;
      end if;
   end Get_Corresponding_Component;

   --
   --  This following section is relative to Entities Components SubprogramCall
   --

   ----------------------------------
   -- Get_Corresponding_Subprogram --
   ----------------------------------

   function Get_Corresponding_Subprogram
     (Call : Ocarina.Types.Node_Id) return Ocarina.Types.Node_Id
   is
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Assert (Call /= No_Node and then Kind (Call) = K_Subprogram_Call);
   begin
      if Entity_Ref (Call) /= No_Node then
         return Get_Referenced_Entity (Entity_Ref (Call));
      else
         return No_Node;
      end if;
   end Get_Corresponding_Subprogram;

   --
   --  This following section is relative to Entities Namespaces
   --

   ---------------------------------------------------
   -- Package_Has_Public_Declarations_Or_Properties --
   ---------------------------------------------------

   function Package_Has_Public_Declarations_Or_Properties
     (Pack : Ocarina.Types.Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Types;

      pragma Assert
        (Pack /= No_Node and then Kind (Pack) = K_Package_Specification);
   begin
      return Has_Public_Part (Pack);
   end Package_Has_Public_Declarations_Or_Properties;

   ----------------------------------------------------
   -- Package_Has_Private_Declarations_Or_Properties --
   ----------------------------------------------------

   function Package_Has_Private_Declarations_Or_Properties
     (Pack : Ocarina.Types.Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Types;

      pragma Assert
        (Pack /= No_Node and then Kind (Pack) = K_Package_Specification);
   begin
      return Has_Private_Part (Pack);
   end Package_Has_Private_Declarations_Or_Properties;

   --
   --  This following section is relative to Entities Messages
   --
   -----------------------------
   -- Display_Node_Kind_Error --
   -----------------------------

   function Display_Node_Kind_Error (Node : Ocarina.Types.Node_Id)
                                    return Boolean is
      use Ocarina.Types;
      use Ocarina.Output;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Debug;
   begin
      Set_Standard_Error;
      W_Str ("Bad node kind: ");

      if Node /= No_Node then
         W_Node_Header (Node);
         W_Line (" " & Node_Kind'Image (Kind (Node)));
      else
         W_Line ("no node");
      end if;

      Set_Standard_Output;
      return False;
   end Display_Node_Kind_Error;

   ------------------------------------------
   -- Add_Path_Element_To_Entity_Reference --
   ------------------------------------------

   procedure Add_Path_Element_To_Entity_Reference
     (Entity_Ref, Item : Ocarina.Types.Node_Id)
   is
      use Ocarina.Types;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      pragma Assert
        (Entity_Ref /= No_Node
         and then
         (Kind (Entity_Ref) = K_Entity_Reference
          or else Kind (Entity_Ref) = K_Reference_Term
          or else DNKE (Entity_Ref)));
      pragma Assert (Item /= No_Node);
   begin
      if Path (Entity_Ref) = No_List then
         Set_Path (Entity_Ref, New_List (K_List_Id, Loc (Item)));
      end if;

      Append_Node_To_List
        (New_Node (K_Node_Container, Loc (Item)),
         Path (Entity_Ref));
      Set_Item (Last_Node (Path (Entity_Ref)), Item);
   end Add_Path_Element_To_Entity_Reference;

end Ocarina.ME_AADL.AADL_Tree.Entities;
