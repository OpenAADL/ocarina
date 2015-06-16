------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                          O C A R I N A . L M P                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2015 ESA & ISAE.                       --
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

with Ocarina.Namet;                      use Ocarina.Namet;

with Ocarina.Analyzer;           use Ocarina.Analyzer;
with Ocarina.Instances;          use Ocarina.Instances;
with Ocarina.Utils;              use Ocarina.Utils;

with Ocarina.Analyzer.AADL.Finder;         use Ocarina.Analyzer.AADL.Finder;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Instances.Finder;

with Ada.Strings.Equal_Case_Insensitive;

package body Ocarina.Lmp is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AIE renames Ocarina.ME_AADL.AADL_Instances.Entities;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   ------------------
   -- Get_Packages --
   ------------------

   function Get_Packages return Node_List is
   begin
      --  K_Package_Name,
      --  K_Package_Specification,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Package_Specification));
   end Get_Packages;

   -----------------------------
   -- Get_Import_Declarations --
   -----------------------------

   function Get_Import_Declarations return Node_List is
   begin
      --  K_Import_Declaration,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Import_Declaration));
   end Get_Import_Declarations;

   ----------------------------
   -- Get_Alias_Declarations --
   ----------------------------

   function Get_Alias_Declarations return Node_List is
   begin
      --  K_Alias_Declaration,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Alias_Declaration));
   end Get_Alias_Declarations;

   -------------------------
   -- Get_Component_Types --
   -------------------------

   function Get_Component_Types (kind : String) return Node_List is
      use Ocarina.ME_AADL;
      EL : Node_List;
   begin
      if Ada.Strings.Equal_Case_Insensitive (kind, "all") then
         return Find_All_Component_Types (Get_AADL_Root);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "abstract") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Abstract);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "data") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Data);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "subprogram") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Subprogram);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "subprogram_group") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Subprogram_Group);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "thread") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Thread);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "thread_group") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Thread_Group);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "process") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Process);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "memory") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Memory);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "processor") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Processor);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "virtual_processor") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Virtual_Processor);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "bus") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Bus);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "virtual_bus") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Virtual_Bus);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "device") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_Device);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "system") then
         return Filter_Component_By_Category (
            Find_All_Component_Types (Get_AADL_Root), CC_System);
      end if;
      EL.First := No_Node;
      EL.Last := No_Node;
      return EL;
   end Get_Component_Types;

   -----------------------------------
   -- Get_Component_Implementations --
   -----------------------------------

   function Get_Component_Implementations (kind : String) return Node_List is
      use Ocarina.ME_AADL;
      EL : Node_List;
   begin
      if Ada.Strings.Equal_Case_Insensitive (kind, "all") then
         return Find_All_Declarations (Get_AADL_Root,
            (1 => ATN.K_Component_Implementation));
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "abstract") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Abstract);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "data") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Data);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "subprogram") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Subprogram);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "subprogram_group") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Subprogram_Group);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "thread") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Thread);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "thread_group") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Thread_Group);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "process") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Process);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "memory") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Memory);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "processor") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Processor);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "virtual_processor") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Virtual_Processor);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "bus") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Bus);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "virtual_bus") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Virtual_Bus);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "device") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_Device);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "system") then
         return Filter_Component_By_Category (
            Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Component_Implementation)), CC_System);
      end if;
      EL.First := No_Node;
      EL.Last := No_Node;
      return EL;
   end Get_Component_Implementations;

   -----------------
   -- Get_Annexes --
   -----------------

   function Get_Annexes return Node_List is
   begin
      --  K_Annex_Content
      --  K_Annex_Subclause
      --  K_Annex_Library
      --  K_Annex_Path
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Annex_Library));
   end Get_Annexes;

   -------------------
   -- Get_Prototype --
   -------------------

   function Get_Prototype return Node_List is
   begin
      --  K_Prototype,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Prototype));
   end Get_Prototype;

   ---------------------------
   -- Get_Prototype_Binding --
   ---------------------------

   function Get_Prototype_Binding return Node_List is
   begin
      --  K_Binding_Prototype,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Binding_Prototype));
   end Get_Prototype_Binding;

   --------------------
   -- Get_Flow_Specs --
   --------------------

   function Get_Flow_Specs return Node_List is
   begin
      --  K_Flow_Spec,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Flow_Spec));
   end Get_Flow_Specs;

   ------------------------------
   -- Get_Flow_Implementations --
   ------------------------------

   function Get_Flow_Implementations return Node_List is
   begin
      --  K_Flow_Implementation,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Flow_Implementation));
   end Get_Flow_Implementations;

   ---------------
   -- Get_Modes --
   ---------------

   function Get_Modes return Node_List is
   begin
      --  K_Mode,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Mode));
   end Get_Modes;

   --------------------------
   -- Get_Mode_Transitions --
   --------------------------

   function Get_Mode_Transitions return Node_List is
   begin
      --  K_Mode_Transition,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Mode_Transition));
   end Get_Mode_Transitions;

   ------------------
   -- Get_In_Modes --
   ------------------

   function Get_In_Modes return Node_List is
   begin
      --  K_In_Modes,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_In_Modes));
   end Get_In_Modes;

   -----------------------
   -- Get_Property_Sets --
   -----------------------

   --  function Get_PropertyBinding return Node_List is

   function Get_Property_Sets return Node_List is
   begin
      --  K_Property_Set,
      return Find_All_Declarations (Get_AADL_Root,
               (1 => ATN.K_Property_Set));
   end Get_Property_Sets;

   ------------------------
   -- Get_Property_Types --
   ------------------------

   function Get_Property_Types (PropertySet : Node_Id) return Node_List is
   begin
      --  K_Property_Type,
      return Filter_Node_By_Kind (ATN.Declarations (PropertySet),
            ATN.K_Property_Type_Declaration);
   end Get_Property_Types;

   ------------------------------
   -- Get_Property_Definitions --
   ------------------------------

   function Get_Property_Definitions (PropertySet : Node_Id)
      return Node_List is
   begin
      --  K_Property_Definition_Declaration,
      return Filter_Node_By_Kind (ATN.Declarations (PropertySet),
            ATN.K_Property_Definition_Declaration);
   end Get_Property_Definitions;

   ----------------------------
   -- Get_Property_Constants --
   ----------------------------

   function Get_Property_Constants (PropertySet : Node_Id) return Node_List is
   begin
      --  K_Constant_Property_Declaration,
      return Filter_Node_By_Kind (ATN.Declarations (PropertySet),
            ATN.K_Constant_Property_Declaration);
   end Get_Property_Constants;

      --  K_Scope_Definition,
      --  K_Identifier,
      --  K_AADL_Entity,
      --  K_Named_AADL_Entity,
      --  K_AADL_Declaration,
      --  K_Entity_Reference,
      --  K_Pair_Of_Entity_References,
      --  K_Identifiers_List,
      --  K_AADL_Specification,
      --  K_AADL_Declarations_List,
      --  K_Name_Visibility_Declaration,
      --  K_Component_Category,
      --  K_Component_Type,
      --  K_Component_Implementation,
      --  K_Contained_Entity,
      --  K_Subclause,
      --  K_Refinable_Feature,
      --  K_Port_Spec,
      --  K_Feature_Group_Spec,
      --  K_Subprogram_Spec,
      --  K_Parameter,
      --  K_Subcomponent_Access,
      --  K_Mode_Transition_Trigger,
      --  K_End_To_End_Flow_Spec,
      --  K_Flow_Implementation_Refinement,
      --  K_End_To_End_Flow_Refinement,
      --  K_Feature_Group_Type,
      --  K_Connection,
      --  K_Contained_Element_Path,
      --  K_Property_Type_Declaration,
      --  K_Single_Valued_Property,
      --  K_Multi_Valued_Property,
      --  K_Constant_Property_Declaration,
      --  K_Property_Value,
      --  K_Property_Definition_Declaration,
      --  K_Property_List_Value,
      --  K_In_Binding,
      --  K_Property_Association,
      --  K_Named_Element,
      --  K_Literal,
      --  K_Signed_AADLNumber,
      --  K_Not_Boolean_Term,
      --  K_And_Boolean_Term,
      --  K_Or_Boolean_Term,
      --  K_Parenthesis_Boolean_Term,
      --  K_Minus_Numeric_Term,
      --  K_Property_Term,
      --  K_Enumeration_Term,
      --  K_Unit_Term,
      --  K_Number_Range_Term,
      --  K_Component_Classifier_Term,
      --  K_Reference_Term,
      --  K_Record_Term,
      --  K_Record_Term_Element,
      --  K_Computed_Term,
      --  K_Boolean_Type,
      --  K_String_Type,
      --  K_Real_Type,
      --  K_Integer_Type,
      --  K_Enumeration_Type,
      --  K_Number_Range,
      --  K_Unit_Definition,
      --  K_Units_Type,
      --  K_Range_Type,
      --  K_Classifier_Type,
      --  K_Classifier_Category_Ref,
      --  K_Referable_Element_Category,
      --  K_Reference_Type,
      --  K_Reference_Category,
      --  K_Record_Type,
      --  K_Record_Type_Element,
      --  K_Unique_Property_Type_Identifier,
      --  K_Applies_To,
      --  K_Unique_Property_Const_Identifier,
      --  K_Annex_Content,
      --  K_Annex_Subclause,
      --  K_Annex_Library,
      --  K_Annex_Path,
      --  K_Array_Dimensions,
      --  K_Array_Dimension_Size,
      --  K_Array_Selection,
      --  K_Range_Selection,
      --  K_Node_Container

   ----------------------------------
   -- Filter_Component_By_Category --
   ----------------------------------

   function Filter_Component_By_Category (components : Node_List;
      category : Ocarina.ME_AADL.Component_Category) return Node_List is
      use Ocarina.ME_AADL;
      List_Node : Node_Id;
      List_Node2 : Node_Id;
      EL : Node_List;
   begin
      EL.First := No_Node;
      EL.Last := No_Node;
      List_Node := components.first;
      while Present (List_Node) loop
         List_Node2 := ATN.Next_Entity (List_Node);
         if ATE.Get_Category_Of_Component (List_Node) = category
         then
            ATNU.Append_Node_To_Node_List (List_Node, EL);
         end if;
         List_Node := List_Node2;
      end loop;
      return EL;
   end Filter_Component_By_Category;

   -------------------------
   -- Filter_Node_By_Kind --
   -------------------------

   function Filter_Node_By_Kind (components : List_Id;
      category : ATN.Node_Kind) return Node_List is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      List_Node : Node_Id;
      List_Node2 : Node_Id;
      EL : Node_List;
   begin
      EL.First := No_Node;
      EL.Last := No_Node;
      List_Node := ATN.First_Node (components);
      while Present (List_Node) loop
         List_Node2 := ATN.Next_Node (List_Node);
         if ATN.Kind (List_Node) = category
         then
            ATNU.Append_Node_To_Node_List (List_Node, EL);
         end if;
         List_Node := List_Node2;
      end loop;
      return EL;
   end Filter_Node_By_Kind;

   -------------------
   -- Get_Instances --
   -------------------

   function Get_Instances (kind : String) return Node_List is
      use Ocarina.ME_AADL;
      EL : Node_List;
   begin
      if Ada.Strings.Equal_Case_Insensitive (kind, "all") then
         return Find_All_Component_Instances (Get_AADL_Root);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "abstract") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Abstract);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "data") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Data);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "subprogram") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Subprogram);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "subprogram_group") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Subprogram_Group);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "thread") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Thread);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "thread_group") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Thread_Group);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "process") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Process);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "memory") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Memory);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "processor") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Processor);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "virtual_processor") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root),
               CC_Virtual_Processor);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "bus") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Bus);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "virtual_bus") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Virtual_Bus);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "device") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_Device);
      elsif Ada.Strings.Equal_Case_Insensitive (kind, "system") then
         return Filter_Instance_By_Category (
            Find_All_Component_Instances (Get_AADL_Root), CC_System);
      end if;
      EL.First := No_Node;
      EL.Last := No_Node;
      return EL;
   end Get_Instances;

   ---------------------------------
   -- Filter_Instance_By_Category --
   ---------------------------------

   function Filter_Instance_By_Category (components : Node_List;
      category : Ocarina.ME_AADL.Component_Category) return Node_List is
      use Ocarina.ME_AADL;
      List_Node : Node_Id;
      List_Node2 : Node_Id;
      EL : Node_List;
   begin
      EL.First := No_Node;
      EL.Last := No_Node;
      List_Node := components.first;
      while Present (List_Node) loop
         List_Node2 := AIN.Next_Entity (List_Node);
         if AIE.Get_Category_Of_Component (List_Node) = category
         then
            AINU.Append_Node_To_Node_List (List_Node, EL);
         end if;
         List_Node := List_Node2;
      end loop;
      return EL;
   end Filter_Instance_By_Category;

   ------------------------
   -- Get_Component_Name --
   ------------------------

   procedure Get_Component_Name (Data : in out Callback_Data'Class;
      N : Node_Id) is
   begin
      Set_Return_Value (Data, ATE.Get_Name_Of_Entity (N, True));
   end Get_Component_Name;

   ----------------------------
   -- Get_Component_Fullname --
   ----------------------------

   procedure Get_Component_Fullname (Data : in out Callback_Data'Class;
      N : Node_Id) is
   begin
      Set_Return_Value (Data, ATE.Get_Name_Of_Entity (
           ATN.Namespace (N), False) & "::"
           & ATE.Get_Name_Of_Entity (N, True));
   end Get_Component_Fullname;

   -----------------------
   -- Get_Instance_Name --
   -----------------------

   procedure Get_Instance_Name (Data : in out Callback_Data'Class;
      N : Node_Id) is
   begin
      Set_Return_Value (Data, Namet.Get_Name_String (
         AINU.Compute_Full_Name_Of_Instance (N)));
   end Get_Instance_Name;

   ----------------------------------------
   -- Find_All_Component_Implementations --
   ----------------------------------------

   function Find_All_Component_Implementations
     (Root      : Node_Id;
      Namespace : Node_Id := No_Node) return Node_List
   is
   begin
      return Find_All_Declarations (Root,
         (1 => ATN.K_Component_Implementation), Namespace);
   end Find_All_Component_Implementations;

   ----------------------------------
   -- Find_All_Component_Instances --
   ----------------------------------

   function Find_All_Component_Instances
     (Root      : Node_Id) return Node_List
   is
      EL      : Node_List;
   begin
      Ocarina.Instances.Finder.Find_All_Instances
        (Root,
         (1 => AIN.K_Component_Instance),
         EL.First,
         EL.Last);
      return EL;
   end Find_All_Component_Instances;

end Ocarina.Lmp;
