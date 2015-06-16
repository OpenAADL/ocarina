------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . F E _ A A D L . P A R S E R _ E R R O R S         --
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

with Ocarina.Types; use Ocarina.Types;

with Ocarina.ME_AADL.Tokens; use Ocarina.ME_AADL.Tokens;

package Ocarina.FE_AADL.Parser_Errors is

   --  Parsing Codes

   type Parsing_Code is
     (PC_None_Statement,
      PC_Items_List,
      PC_Classifier_Reference,
      PC_Identifiers,
      PC_Defining_Identifier,
      PC_Defining_Name,

      PC_AADL_Specification,
      PC_AADL_Declaration,
      PC_Package_Specification,
      PC_Package_Declarations,
      PC_Package_Name,
      PC_Annex_Library,
      PC_Annex_Subclause,
      PC_Annex_Path,

      PC_Array_Dimensions,
      PC_Array_Dimension_Size,
      PC_Array_Selection,

      PC_Reference_Category,
      PC_Classifier_Category,
      PC_Computed_Term,

      PC_Component,
      PC_Component_Category,
      PC_Unique_Component_Type_Identifier,
      PC_Component_Type,
      PC_Component_Type_Extension,
      PC_Unique_Component_Impl_Name,
      PC_Component_Implementation,
      PC_Component_Implementation_Extension,

      PC_Connections,
      PC_Connection,
      PC_Connection_Refinement,
      PC_Connection_Reference,
      PC_Data_Connection,
      PC_Data_Connection_Refinement,
      PC_Event_Connection,
      PC_Event_Connection_Refinement,
      PC_Event_Data_Connection,
      PC_Event_Data_Connection_Refinement,
      PC_Feature_Group_Connection,
      PC_Feature_Group_Connection_Refinement,
      PC_Parameter_Connection,
      PC_Parameter_Connection_Refinement,
      PC_Access_Connection,
      PC_Access_Connection_Refinement,

      PC_Features,
      PC_Refines_Type,
      PC_Feature,
      PC_Feature_Refinement,
      PC_Parameters,
      PC_Parameter,
      PC_Parameter_Refinement,

      PC_Port_Connection,
      PC_Port_Connection_Refinement,

      PC_Flow_Identifier,
      PC_Subcomponent_Flow_Identifier,
      PC_Flow_Specifications,
      PC_Flow_Spec,
      PC_Flow_Spec_Refinement,
      PC_Flow_Source_Spec,
      PC_Flow_Source_Spec_Refinement,
      PC_Flow_Sink_Spec,
      PC_Flow_Sink_Spec_Refinement,
      PC_Flow_Path_Spec,
      PC_Flow_Path_Spec_Refinement,
      PC_End_To_End_Flow_Spec,
      PC_End_To_End_Flow_Refinement,

      PC_Flow_Implementations,
      PC_Flow_Implementation,
      PC_Flow_Implementation_Refinement,
      PC_Flow_Path_Implementation,
      PC_Flow_Path_Implementation_Refinement,
      PC_Flow_Sink_Implementation,
      PC_Flow_Sink_Implementation_Refinement,
      PC_Flow_Source_Implementation,
      PC_Flow_Source_Implementation_Refinement,

      PC_Feature_Group_Spec,
      PC_Feature_Group_Refinement,
      PC_Feature_Group_Type,
      PC_Feature_Group_Type_Extension,
      PC_Feature_Group_Or_Port_Group_Or_Port_Spec,
      PC_Feature_Refinement_Or_Feature_Group_Refinement,
      PC_Unique_Feature_Group_Type_Reference,
      PC_Feature_Group_Or_Port_Group_Refinement,
      PC_Port_Group_Spec_Or_Feature_Group_Spec,
      PC_Port_Spec_Or_Feature_Group_Spec,
      PC_Port_Refinement_Or_Feature_Group_Refinement,

      PC_Mode,
      PC_Mode_Refinement,
      PC_In_Modes,
      PC_In_Modes_And_Transitions,
      PC_Mode_Or_Mode_Transition,
      PC_Mode_Or_Transition,
      PC_Mode_Transition,
      PC_Mode_Transition_Trigger,
      PC_Requires_Modes_Subclause,

      PC_Name_Visibility_Declaration,
      PC_Alias_Declaration,
      PC_Import_Declaration,

      PC_Port_Type,
      PC_Port_Group_Spec,
      PC_Port_Group_Refinement,
      PC_Port_Group_Type,
      PC_Port_Group_Type_Extension,
      PC_Port_Spec,
      PC_Port_Refinement,
      PC_Unique_Port_Group_Type_Reference,
      PC_Unique_Port_Identifier,
      PC_Port_Refinement_Or_Port_Group_Refinement,

      PC_Properties,
      PC_Property_Set,
      PC_Property_Declaration,

      PC_Property_Type,
      PC_Property_Type_Declaration,
      PC_Property_Type_Designator,
      PC_Unique_Property_Type_Identifier,

      PC_Prototype,
      PC_Prototype_Refinement,
      PC_Prototype_Or_Prototype_Refinement,
      PC_Prototype_Bindings,
      PC_Prototype_Binding,

      PC_Enumeration_Type,
      PC_Units_Type,
      PC_Unit_Definition,
      PC_Number_Type,
      PC_Range_Type,
      PC_Number_Range,
      PC_Real_Range,
      PC_Integer_Range,
      PC_Classifier_Type,
      PC_Reference_Type,
      PC_Referable_Element_Category,

      PC_Record_Type,
      PC_Record_Term,
      PC_Record_Type_Element,
      PC_Record_Term_Element,

      PC_Property_Definition_Declaration,
      PC_Single_Valued_Property,
      PC_Multi_Valued_Property,

      PC_Property_Owner_Category,
      PC_Property_Owner_Or_Category,

      PC_Contained_Element_Path,
      PC_Contained_Element,
      PC_Named_Element,
      PC_Named_Element_Category,
      PC_Named_Element_Component,
      PC_Named_Element_Component_With_Access,
      PC_Named_Element_Flow,
      PC_Named_Element_Feature,
      PC_Named_Element_End_To_End_Flow,
      PC_Named_Element_Identifier,
      PC_Named_Element_Mode,
      PC_Named_Element_Port_Or_Access_Or_Event,

      PC_Property_Constant,
      PC_Unique_Property_Constant_Identifier,
      PC_Single_Valued_Property_Constant,
      PC_Multi_Valued_Property_Constant,
      PC_Constant_Signed_Integer_Value,
      PC_Constant_Signed_Real_Value,
      PC_Constant_String_Value,
      PC_Constant_Property_Value,

      PC_Property_Association,
      PC_Access_Property_Association,
      PC_Contained_Property_Association,
      PC_Property_Association_Or_Contained_Property_Association,

      PC_Property_Expression,
      PC_Property_List_Value,
      PC_Property_Term,
      PC_Boolean_Property_Term,
      PC_Component_Classifier_Term,
      PC_Reference_Term,
      PC_Boolean_Term,
      PC_Boolean_Or_Record_Term,
      PC_Number_Range_Term,
      PC_Integer_Range_Term,
      PC_Real_Range_Term,

      PC_Data_Subprogram_Spec,
      PC_Data_Subprogram_Refinement,
      PC_Server_Subprogram,
      PC_Server_Subprogram_Refinement,
      PC_Subprogram_Call,
      PC_Subprogram_Call_Sequences,
      PC_Subprogram_Call_Sequence,
      PC_Called_Subprogram,

      PC_Subcomponents,
      PC_Subcomponent,
      PC_Subcomponent_Refinement,
      PC_Subcomponent_Access,
      PC_Subcomponent_Access_Refinement,
      PC_Subcomponent_Access_Classifier);

   --  Error Message Code

   type Error_Message_Code is
     (EMC_Access_Property_Association_Is_Not_Allowed,
      EMC_Contained_Property_Association_Is_Not_Allowed,
      EMC_Memory_Access_Connection_Is_Not_Allowed,
      EMC_Debug,
      EMC_Invalid_Range,
      EMC_List_Is_Empty,
      EMC_No_Defining_Identifier,
      EMC_Refinement_Is_Not_Allowed,
      EMC_Extends_Incompatible_Entity,
      EMC_Not_Allowed_In_AADL_V1,
      EMC_Not_Allowed_In_AADL_V2,
      EMC_Port_Group_Not_Allowed_In_AADL_V2,
      EMC_Mode_Transition_Not_Allowed_In_Requires_Modes,
      EMC_Feature_Group_Not_Allowed_In_AADL_V1,
      EMC_Odd_Number_Of_Element_Expected,
      EMC_At_Least_Tow_Elements_Expected,
      EMC_At_Least_Three_Elements_Expected);

   procedure Display_Parsing_Error
     (Code      : Parsing_Code;
      Error_Msg : Error_Message_Code);
   procedure DPE
     (Code      : Parsing_Code;
      Error_Msg : Error_Message_Code) renames
     Display_Parsing_Error;
   --  Display an output message:
   --     Location: parsing ..., <Msg>

   procedure Display_Parsing_Error (Code : Parsing_Code);
   procedure DPE (Code : Parsing_Code) renames Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., unexpected `Current_Token_Image`

   procedure Display_Parsing_Error
     (Code          : Parsing_Code;
      Error_Msg     : Error_Message_Code;
      Current_Token : Token_Type);
   procedure DPE
     (Code          : Parsing_Code;
      Error_Msg     : Error_Message_Code;
      Current_Token : Token_Type) renames
     Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., unexpected `Current_Token_Image`,
   --               error message code

   procedure Display_Parsing_Error (Code : Parsing_Code; Identifier : Name_Id);
   procedure DPE
     (Code       : Parsing_Code;
      Identifier : Name_Id) renames
     Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., identifier <...> expected, found
   --     Current_Token_Image

   procedure Display_Parsing_Error
     (Code           : Parsing_Code;
      Expected_Token : Token_Type);
   procedure DPE
     (Code           : Parsing_Code;
      Expected_Token : Token_Type) renames
     Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., token ... expected, found Current_Token_Image

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Expected_Tokens : Token_List_Type);
   procedure DPE
     (Code            : Parsing_Code;
      Expected_Tokens : Token_List_Type) renames
     Display_Parsing_Error;
   --  Display an output error message:
   --     Location: parsing ..., token ... or ... or ... expected,
   --     found Current_Token_Image

   function Image (Code : Parsing_Code) return String;
   --  Return corresponding string of given parsing code

   function Image (Code : Error_Message_Code) return String;
   --  Return corresponding string of given error message code

end Ocarina.FE_AADL.Parser_Errors;
