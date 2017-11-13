------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . F E _ A A D L _ E M A . P A R S E R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2015-2017 ESA & ISAE.                    --
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

with Ocarina.Parser; use Ocarina.Parser;

with Ocarina.ME_AADL_EMA.EMA_Tokens;
with Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
with Ocarina.EMA_Values;
with Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
with Ocarina.FE_AADL_EMA.Lexer;
with Ocarina.FE_AADL_EMA.Parser_Errors;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.FE_AADL_EMA.Parser is

   use Ocarina.ME_AADL_EMA.EMA_Tokens;
   use Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
   use Ocarina.EMA_Values;
   use Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
   use Ocarina.FE_AADL_EMA.Lexer;
   use Ocarina.FE_AADL_EMA.Parser_Errors;

   Language  : constant String := "emv2";
   EMA_Annex : Node_Id;

   function P_Library return Node_Id;
   function P_Annex_Library return Node_Id;
   function P_Error_Type_Library return Node_Id;
   function P_Error_Behavior_State_Machine return Node_Id;
   function P_Error_Type_Mappings return Node_Id;
   function P_Error_Type_Transformations return Node_Id;
   function P_Error_Model_Library_Reference return Node_Id;
   function P_Error_Type_Library_List return List_Id;
   function P_Error_Type_Library_List_Used return List_Id;
   function P_Error_Type_Library_List_Extended return List_Id;
   function P_Error_Type_Library_Element return Node_Id;
   function P_Error_Type_Transformation_Set_Reference return Node_Id;
   function P_Error_Behavior_Event_List return List_Id;
   function P_Error_Behavior_State return Node_Id;
   function P_Error_Behavior_State_List return List_Id;
   function P_Error_Behavior_Transition_List return List_Id;
   function P_Error_Type_Mapping return Node_Id;
   function P_Use_Error_Types return Node_Id;
   function P_Error_Type_Transformation return Node_Id;
   function P_Error_Type_Definition return Node_Id;
   function P_Error_Type_Alias return Node_Id;
   function P_Error_Type_Set_Definition return Node_Id;
   function P_Error_Type_Set_Alias return Node_Id;
   function P_Error_Event return Node_Id;
   function P_Recover_Event return Node_Id;
   function P_Repair_Event return Node_Id;
   function P_Error_Type_Set return List_Id;
   function P_Error_Type_Set_Or_Noerror return Node_Id;
   function P_Target_Error_Type_Instance return Node_Id;
   function P_Transition return Node_Id;
   function P_Branching_Transition return Node_Id;
   function P_Error_Type_Reference return Node_Id;
   function P_Error_Type_Set_Reference return Node_Id;
   function P_Error_Event_Condition return Node_Id;
   function P_Event_Initiators return List_Id;
   function P_Initiator_Reference return Node_Id;
   function P_Port_Reference return Node_Id;
   function P_Mode_Transition_Reference return Node_Id;
   function P_Self_Event_Reference return Node_Id;
   function P_Event_Initiation return Node_Id;
   function P_Type_Set_Element return Node_Id;
   function P_Error_Type_Or_Set_Reference return Node_Id;
   function P_Error_Type_Product return List_Id;
   function P_Error_Source_State return Node_Id;
   function P_Error_Condition return Node_Id;
   function P_Error_Transition_Target return Node_Id;
   function P_Error_Transition_Branch return Node_Id;
   function P_Branch_Probability return Node_Id;
   function P_Fixed_Probability_Value return Node_Id;
   function P_Error_Condition_Trigger return Node_Id;
   function P_Error_Propagation_Point return Node_Id;
   function P_Feature_Reference return Node_Id;
   function P_Binding_Reference return Node_Id;

   function P_Subclause return Node_Id;
   function P_Annex_Subclause return Node_Id;
   function P_Error_Type_Mappings_Reference return Node_Id;
   function P_Error_Behavior_State_Machine_Reference return Node_Id;
   function P_Error_Propagations return Node_Id;
   function P_Error_Propagation_Or_Containment return Node_Id;
   function P_Error_Flow return Node_Id;
   function P_Error_Source return Node_Id;
   function P_Fault_Source return Node_Id;
   function P_Failure_Mode_Description return Node_Id;
   function P_Fault_Condition return Node_Id;
   function P_Error_Sink return Node_Id;
   function P_Error_Path return Node_Id;
   function P_Component_Error_Behavior return Node_Id;
   function P_Outgoing_Propagation_Condition return Node_Id;
   function P_Propagation_Target return Node_Id;
   function P_Outgoing_Propagation_Condition_List return List_Id;
   function P_Error_Detection return Node_Id;
   function P_Error_Detection_Effect return Node_Id;
   function P_Internal_Event_Reference return Node_Id;
   function P_Error_Code_Value return Node_Id;
   function P_Property_Constant_Term return Node_Id;
   function P_Error_Detection_List return List_Id;
   function P_Error_State_To_Mode_Mapping return Node_Id;
   function P_Error_State_To_Mode_Mapping_List return List_Id;
   function P_Composite_Error_Behavior return List_Id;
   function P_Composite_Error_State return Node_Id;
   function P_Composite_State_Expression return Node_Id;
   function P_Composite_State_Element return Node_Id;
   function P_Subcomponent_Error_State return Node_Id;
   function P_Connection_Error_Behavior return Node_Id;
   function P_Connection_Error_Source return Node_Id;
   function P_Propagation_Paths return Node_Id;
   function P_Propagation_Point return Node_Id;
   function P_Propagation_Path return Node_Id;
   function P_Qualified_Propagation_Point (Code_Expected  : Parsing_Code)
   return Node_Id;
   function P_EMV2_Properties_Section return List_Id;
   function P_Emv2_Contained_Property_Association return Node_Id;
   function P_Property_Identifier return Node_Id;
   function P_Emv2_Containment_Path return Node_Id;
   function P_Assignment return Node_Id;
   function P_In_Modes (Code : Parsing_Code) return List_Id;
   function P_Property_Value return Node_Id;
   function P_Numeric_Term (Exist_Unit : in Boolean) return Node_Id;
   function P_Time_Range return Node_Id;
   function P_AADLNumber (Exist_Unit : Boolean) return Node_Id;

   function P_Identifier return Node_Id;
   procedure P_Composed_Identifier
   (Identifier_1 : out  Node_Id; Identifier_2 : out Node_Id;
    Parent_Node : in out Node_Id; Code : in Parsing_Code);
   procedure P_Composed_Referenced_Identifier
   (List_Identifier : out  List_Id; Identifier_2 : out Node_Id;
    Parent_Node : in out Node_Id; Code : in Parsing_Code);
   --  for composed identifier containing package reference or
   --  error_model_library_reference

   --  For List_Id i should have defined a function returning node_Id
   --  but , since we won't use the function returning node_id
   --  anywhere else except that particular list , then
   --  the whole treatment in the function returning list_id

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Ocarina.Parser.Register_Parser (Language, Process'Access);
   end Init;

   ---------------
   --  Process  --
   ---------------

   function Process (AADL_Root  : Node_Id;
                     From       : Location;
                     To         : Location := No_Location;
                     Container  : Node_Id)
                    return Node_Id
   is
      pragma Unreferenced (AADL_Root, From, To);
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
   begin

      if Ocarina.ME_AADL.AADL_Tree.Nodes."="
      (Kind (Container), K_Component_Type) or else
      Ocarina.ME_AADL.AADL_Tree.Nodes."="
      (Kind (Container), K_Component_Implementation) or else
      Ocarina.ME_AADL.AADL_Tree.Nodes."="
      (Kind (Container), K_Feature_Group_Type)
      then
         EMA_Annex := P_Subclause;
      elsif Ocarina.ME_AADL.AADL_Tree.Nodes."="
      (Kind (Container), K_Package_Specification)
      then
         EMA_Annex := P_Library;
      else
         DPE;
         EMA_Annex := No_Node;
      end if;

      return EMA_Annex;
   end Process;

   ---------------
   -- P_Library --
   ---------------

   --  EMA_Annex ::= {** Annex_Library **}

   function P_Library return Node_Id
   is
      EMA_Annex : Node_Id := No_Node;
   begin
      if Next_Token /= T_EOF then
         EMA_Annex := P_Annex_Library;
      end if;

      return EMA_Annex;
   end P_Library;

   ---------------------
   -- P_Annex_Library --
   ---------------------

   --  Annex_Library ::=
   --  [ error_type_library ]
   --  { error_behavior_state_machine } *
   --  { error_type_mappings } *
   --  { error_type_transformations } *

   --  The first 2 nodes begin with 'error'
   --  and the other 2 begin with 'type'
   --  So we have to test the second token :
   --  if not we have to start over with Restore_Lexer

   function P_Annex_Library return Node_Id is
      Error_Model_Library : Node_Id;

      Error_Type_Library : Node_Id;
      Error_Behavior_State_Machine_List : List_Id;
      Error_Behavior_State_Machine : Node_Id;
      Error_Type_Mappings_List : List_Id;
      Error_Type_Mappings : Node_Id;
      Error_Type_Transformations_List : List_Id;
      Error_Type_Transformations : Node_Id;

      Next : Token_Type;
      Loc_Begin, Loc_End, Loc, Loc_Start_1, Loc_Start_2 : Location;
      Exist_Node_1, Exist_Node_3_Or_4 : Boolean := False;
   begin
      Error_Model_Library :=
      New_Node (K_Annex_Library, Token_Location);

      --  To Test if there is no node entred
      --  => the propblem is with the first token
      Save_Lexer (Loc_Begin);

      --  Scanning node 1

      Save_Lexer (Loc_Start_1);

      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_R_Error then
         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         if Next = T_Types then
            Restore_Lexer (Loc);
            Scan_Token;
            Error_Type_Library := P_Error_Type_Library;
            Set_Error_Type_Library (Error_Model_Library, Error_Type_Library);
            if Present (Error_Type_Library) then
               Exist_Node_1 := True;
            else
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc_Start_1);
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Scanning node 2

      Error_Behavior_State_Machine_List := New_List
      (K_List_Id, Token_Location);
      Set_Error_Behavior_State_Machine_List
      (Error_Model_Library,  Error_Behavior_State_Machine_List);

      Save_Lexer (Loc);
      Next := Next_Token;
      while Next = T_R_Error loop
         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Behavior then
            Scan_Token;
            Error_Behavior_State_Machine := P_Error_Behavior_State_Machine;
            Append_Node_To_List
            (Error_Behavior_State_Machine, Error_Behavior_State_Machine_List);
            if No (Error_Behavior_State_Machine) then
               return No_Node;
            end if;
         else
            if Exist_Node_1 then
               Scan_Token;
               DPE (PC_Error_Model_Library_Constructs,
               Expected_Token => (T_Behavior));
            else
               Scan_Token;
               DPE (PC_Error_Model_Library_Constructs,
               Expected_Tokens => (T_Types,
                                   T_Behavior));
            end if;
            return No_Node;
         end if;
         Save_Lexer (Loc);
         Next := Next_Token;
      end loop;

      --  Scanning node 3

      Error_Type_Mappings_List := New_List (K_List_Id, Token_Location);
      Set_Error_Type_Mappings_List (Error_Model_Library,
      Error_Type_Mappings_List);

      Save_Lexer (Loc);
      Next := Next_Token;
      while Next = T_Type loop
         --  puisque c'est une boucle on doit stoker à chaque fois
         --  la nouvelle position avant 'type'
         Loc_Start_2 := Loc;

         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         if Next = T_Mappings then
            Restore_Lexer (Loc);
            Scan_Token;
            Error_Type_Mappings := P_Error_Type_Mappings;
            Append_Node_To_List (Error_Type_Mappings,
            Error_Type_Mappings_List);
            if Present (Error_Type_Mappings) then
               Exist_Node_3_Or_4 := True;
            else
               return No_Node;
            end if;
         else
            if Next /= T_Transformations then
               Scan_Token;
               DPE (PC_Error_Model_Library_Constructs,
               Expected_Tokens => (T_Mappings,
                                   T_Transformations));
               return No_Node;
            end if;
            Restore_Lexer (Loc_Start_2);
            exit;
         end if;
         Save_Lexer (Loc);
         Next := Next_Token;
      end loop;
      Restore_Lexer (Loc);

      --  Scanning node 4

      Error_Type_Transformations_List := New_List (K_List_Id, Token_Location);
      Set_Error_Type_Transformations_List
      (Error_Model_Library,  Error_Type_Transformations_List);

      while Token = T_Type loop
         Scan_Token;
         if Token = T_Transformations then
            Error_Type_Transformations := P_Error_Type_Transformations;
            Append_Node_To_List
            (Error_Type_Transformations, Error_Type_Transformations_List);
            if Present (Error_Type_Transformations) then
               Exist_Node_3_Or_4 := True;
            else
               return No_Node;
            end if;
         else
            DPE (PC_Error_Model_Library_Constructs,
            Expected_Token => T_Transformations);
         end if;
         Scan_Token;
      end loop;

      --  In case if the first token does not exist

      Save_Lexer (Loc_End);
      if (Loc_Begin = Loc_End or else
      (Exist_Node_3_Or_4 = False and then Loc_Begin /= Loc_End))
      and then Next_Token /= T_EOF
      then
         Scan_Token;
         DPE (PC_Error_Model_Library_Constructs,
         Expected_Tokens => (T_R_Error,
                             T_Type));
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Exist_Node_3_Or_4 and then
         (Next = T_Transformations or else Next = T_Mappings)
         then
            Scan_Token;
            DPE (PC_Error_Model_Library_Constructs,
            Expected_Token => T_Type);
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;
      end if;

      return Error_Model_Library;
   end P_Annex_Library;

   --------------------------
   -- P_Error_Type_Library --
   --------------------------

   --  error_type_library ::=
   --  error types
   --  [ use types error_type_library_list ; ]
   --  [ extends error_type_library_list with ]
   --  { error_type_library_element } +
   --  [ properties
   --  { error_type_emv2_contained_property_association } + ]
   --  end types;

   function P_Error_Type_Library return Node_Id is
      Error_Type_Library : Node_Id;
      Error_Type_Library_List_Used : List_Id;
      Error_Type_Library_List_Extended : List_Id;
      Error_Type_Library_Element_List : List_Id;
      Error_Type_Library_Element : Node_Id;
      Properties : List_Id;
      Identifier : Node_Id;
      Next : Token_Type;
      Loc_Start, Loc_End, Loc : Location;
   begin
      Error_Type_Library := New_Node (K_Error_Type_Library, Token_Location);

      --  Scanning node 1

      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_Use then
         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         if Next = T_Types then
            Restore_Lexer (Loc);
            Scan_Token;
            Error_Type_Library_List_Used := P_Error_Type_Library_List_Used;
            Set_Error_Type_Library_List_Used
            (Error_Type_Library, Error_Type_Library_List_Used);
            if No (Error_Type_Library_List_Used) then
               return No_Node;
            end if;
         else
            Scan_Token;
            DPE (PC_Error_Type_Library, Expected_Token => T_Types);
            Restore_Lexer (Loc);
            Skip_Declaration (T_Semi_Colon);
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Scanning node 2

      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_Extends then
         Restore_Lexer (Loc);
         Scan_Token;
         Error_Type_Library_List_Extended
         := P_Error_Type_Library_List_Extended;
         Set_Error_Type_Library_List_Extended
         (Error_Type_Library, Error_Type_Library_List_Extended);
         if No (Error_Type_Library_List_Extended) then
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Scanning node 3

      Error_Type_Library_Element_List := New_List (K_List_Id, Token_Location);

      --  All nodes of error_type_library_element begin with
      --  an identifier
      --  => there is no identifier we won't have to enter the function

      Save_Lexer (Loc);
      Identifier := P_Identifier;
      if No (Identifier) then
         Scan_Token;
         DPE (PC_Error_Type_Library_Element, Expected_Token => T_Identifier);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Error_Type_Library_Element := P_Error_Type_Library_Element;
      if No (Error_Type_Library_Element) then
         return No_Node;
      end if;

      loop
         Append_Node_To_List
         (Error_Type_Library_Element, Error_Type_Library_Element_List);
         Save_Lexer (Loc);
         if Next_Token = T_End or else Next_Token = T_Properties then
            Restore_Lexer (Loc);
            exit;
         end if;
         Restore_Lexer (Loc);
         Save_Lexer (Loc_Start);
         Error_Type_Library_Element := P_Error_Type_Library_Element;
         Save_Lexer (Loc_End);
         if Loc_Start /= Loc_End and then No (Error_Type_Library_Element)
         then
            return No_Node;
         end if;
         exit when Error_Type_Library_Element = No_Node;
      end loop;

      Set_Error_Type_Library_Element_List
      (Error_Type_Library,  Error_Type_Library_Element_List);

      --  Scanning node 4

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Properties then
         Scan_Token;
         Properties := P_EMV2_Properties_Section;
         Set_Properties (Error_Type_Library, Properties);
         if No (Properties) then
            return No_Node;
         end if;
      end if;

      --  Scanning : end types;

      Save_Lexer (Loc);
      if Next_Token = T_End then
         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         if Next_Token = T_Types then
            Restore_Lexer (Loc);
            Scan_Token;
            Save_Lexer (Loc);
            Scan_Token (T_Semi_Colon);
            if Token = T_Error then
               Restore_Lexer (Loc);
               Save_Lexer (Loc);
               Scan_Token;
               DPE (PC_Error_Type_Library, Expected_Token => T_Semi_Colon);
               Restore_Lexer (Loc);
            end if;
         else
            Restore_Lexer (Loc);
            Scan_Token;
            DPE (PC_Error_Type_Library, Expected_Token => T_Types);
            return No_Node;
         end if;
      else
         Scan_Token;
         DPE (PC_Error_Type_Library, Expected_Token => T_End);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      return Error_Type_Library;
   end P_Error_Type_Library;

   ------------------------------------
   -- P_Error_Behavior_State_Machine --
   ------------------------------------

   --  error_behavior_state_machine ::=
   --  error behavior defining_state_machine_identifier
   --  [ use types error_type_library_list ; ]
   --  [ use transformations error_type_transformation_set_reference ; ]
   --  [ events { error_behavior_event } + ]
   --  [ states { error_behavior_state } + ]
   --  [ transitions { error_behavior_transition } + ]
   --  [ properties
   --  { error_behavior_state_machine_emv2_contained_property_association } +
   --  ]
   --  end behavior ;

   function P_Error_Behavior_State_Machine return Node_Id is
      Error_Behavior_State_Machine : Node_Id;

      Identifier : Node_Id;
      Error_Type_Library_List : List_Id;
      Error_Type_Transformation_Set_Reference : Node_Id;
      Error_Behavior_Event_List : List_Id;
      Error_Behavior_State_List : List_Id;
      Error_Behavior_Transition_List : List_Id;
      Properties : List_Id;

      Next : Token_Type;
      Loc, Loc_Start : Location;
      Test_Use_Types : BOOLEAN := False;
      Wait : Boolean := True;
   begin
      Error_Behavior_State_Machine :=
      New_Node (K_Error_Behavior_State_Machine, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         Scan_Token;
         DPE (PC_Error_Behavior_State_Machine,
         Expected_Token => T_Identifier);
         return No_Node;
      end if;
      Set_Identifier (Error_Behavior_State_Machine, Identifier);

      Save_Lexer (Loc);
      if Next_Token = T_End then
         Wait := False;
         Restore_Lexer (Loc);
      end if;

      --  Scanning node 1

      Save_Lexer (Loc_Start);

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Types then
            Scan_Token;
            Error_Type_Library_List := P_Error_Type_Library_List_Used;
            Set_Error_Type_Library_List
            (Error_Behavior_State_Machine, Error_Type_Library_List);
            if Present (Error_Type_Library_List) then
               Test_Use_Types := True;
               Wait := False;
               --  The node 'use types' may exist once
               --  If it exists => the second node 'use' must be
               --  'use transformations' (we can use this for the error
               --  message)
            else
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc_Start);
         end if;
      end if;

      --  Scanning node 2

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Wait := True;
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Transformations then
            Scan_Token;
            Error_Type_Transformation_Set_Reference :=
            P_Error_Type_Transformation_Set_Reference;
            Set_Error_Type_Transformation_Set_Reference
            (Error_Behavior_State_Machine,
            Error_Type_Transformation_Set_Reference);
            if Present (Error_Type_Transformation_Set_Reference) then
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               if Next = T_Semi_Colon then
                  Scan_Token;
                  Wait := False;
               else
                  Scan_Token;
                  DPE (PC_Error_Behavior_State_Machine,
                  Expected_Token => T_Semi_Colon);
                  return No_Node;
               end if;
            else
               return No_Node;
            end if;
         else
            Scan_Token;
            if Test_Use_Types then
               DPE (PC_Error_Behavior_State_Machine,
               Expected_Token => T_Transformations);
            else
               DPE (PC_Error_Behavior_State_Machine,
               Expected_Tokens => (T_Types,
                                   T_Transformations));
            end if;
            return No_Node;
         end if;
      end if;

      --  Scanning node 3

      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_Events then
         Wait := True;
         Restore_Lexer (Loc);
         Scan_Token;
         Error_Behavior_Event_List := P_Error_Behavior_Event_List;
         if Present (Error_Behavior_Event_List) then
            Set_Error_Behavior_Event_List (Error_Behavior_State_Machine,
            Error_Behavior_Event_List);
            Wait := False;
         else
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Scanning node 4
      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_States then
         Wait := True;
         Restore_Lexer (Loc);
         Scan_Token;
         Error_Behavior_State_List := P_Error_Behavior_State_List;
         Set_Error_Behavior_State_List (Error_Behavior_State_Machine,
         Error_Behavior_State_List);
         if Present (Error_Behavior_State_List) then
            Wait := False;
         else
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Scanning node 5

      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_Transitions then
         Wait := True;
         Restore_Lexer (Loc);
         Scan_Token;
         Error_Behavior_Transition_List := P_Error_Behavior_Transition_List;
         Set_Error_Behavior_Transition_List (Error_Behavior_State_Machine,
         Error_Behavior_Transition_List);
         if Present (Error_Behavior_Transition_List) then
            Wait := False;
         else
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Scanning node 6

      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_Properties then
         Wait := True;
         Restore_Lexer (Loc);
         Scan_Token;
         Properties := P_EMV2_Properties_Section;
         Set_Properties (Error_Behavior_State_Machine, Properties);
         if Present (Properties) then
            Wait := False;
         else
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Scanning : end behavior;

      if Wait = False then
         Save_Lexer (Loc);
         if Next_Token = T_End then
            Restore_Lexer (Loc);
            Scan_Token;
            Save_Lexer (Loc);
            if Next_Token = T_Behavior then
               Restore_Lexer (Loc);
               Scan_Token;
               Save_Lexer (Loc);
               Scan_Token (T_Semi_Colon);
               if Token = T_Error then
                  Restore_Lexer (Loc);
                  Save_Lexer (Loc);
                  Scan_Token;
                  DPE (PC_Error_Behavior_State_Machine,
                  Expected_Token => T_Semi_Colon);
                  Restore_Lexer (Loc);
               end if;
            else
               Restore_Lexer (Loc);
               Scan_Token;
               DPE (PC_Error_Behavior_State_Machine,
               Expected_Token => T_Types);
               return No_Node;
            end if;
         else
            Scan_Token;
            DPE (PC_Error_Behavior_State_Machine, Expected_Token => T_End);
            Restore_Lexer (Loc);
            return No_Node;
         end if;
      end if;

      return Error_Behavior_State_Machine;
   end P_Error_Behavior_State_Machine;

   ---------------------------
   -- P_Error_type_mappings --
   ---------------------------

   --  Error_type_mappings ::=
   --  type mappings defining_type_mappings_identifier
   --  [ use types error_type_library_list ; ]
   --  { error_type_mapping } +
   --  end mappings;

   function P_Error_Type_Mappings return Node_Id is
      Error_Type_Mappings : Node_Id;
      Identifier : Node_Id;
      Error_Type_Library_List : List_Id;
      Error_Type_Mapping : Node_Id;
      Error_Type_Mapping_List : List_Id;
      Next : Token_Type;
      Loc_Start, Loc_End, Loc : Location;
   begin
      Error_Type_Mappings := New_Node (K_Error_Type_Mappings, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         Scan_Token;
         DPE (PC_Error_Type_Mappings,
         Expected_Token => T_Identifier);
         return No_Node;
      end if;
      Set_Identifier (Error_Type_Mappings, Identifier);

      --  Scanning node 1

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         Scan_Token;
         if Token = T_Types then
            Error_Type_Library_List := P_Error_Type_Library_List_Used;
            Set_Error_Type_Library_List
            (Error_Type_Mappings, Error_Type_Library_List);
            if No (Error_Type_Library_List) then
               return No_Node;
            end if;
         else
            DPE (PC_Error_Type_Mappings,
            Expected_Token => T_Types);
            return No_Node;
         end if;
      end if;

      --  Scanning node 2
      Error_Type_Mapping_List := New_List (K_List_Id, Token_Location);
      Set_Error_Type_Mapping_List
      (Error_Type_Mappings,  Error_Type_Mapping_List);
      loop
         Save_Lexer (Loc_Start);
         Loc := Loc_Start;
         Next := Next_Token;
         Restore_Lexer (Loc);
         Error_Type_Mapping := No_Node;
         if Next = T_Left_Brace then
            Error_Type_Mapping := P_Error_Type_Mapping;
         end if;
         Save_Lexer (Loc_End);
         if Present (Error_Type_Mapping) then
            Append_Node_To_List (Error_Type_Mapping,
            Error_Type_Mapping_List);
         elsif Loc_Start /= Loc_End then
            return No_Node;
         end if;

         exit when Error_Type_Mapping = No_Node;
      end loop;
      if Is_Empty (Error_Type_Mapping_List) then
         Scan_Token;
         DPE (PC_Error_Type_Mapping,
         Expected_Token => T_Left_Brace);
         return No_Node;
      end if;

      --  Scanning : end mappings;

      Save_Lexer (Loc);
      if Next_Token = T_End then
         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         if Next_Token = T_Mappings then
            Restore_Lexer (Loc);
            Scan_Token;
            Save_Lexer (Loc);
            Scan_Token (T_Semi_Colon);
            if Token = T_Error then
               Restore_Lexer (Loc);
               Save_Lexer (Loc);
               Scan_Token;
               DPE (PC_Error_Type_Mappings, Expected_Token => T_Semi_Colon);
               Restore_Lexer (Loc);
            end if;
         else
            Restore_Lexer (Loc);
            Scan_Token;
            DPE (PC_Error_Type_Mappings, Expected_Token => T_Mappings);
            return No_Node;
         end if;
      else
         Scan_Token;
         DPE (PC_Error_Type_Mappings, Expected_Token => T_End);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      return Error_Type_Mappings;
   end P_Error_Type_Mappings;

   ----------------------------------
   -- P_Error_Type_Transformations --
   ----------------------------------

   --  error_type_transformations ::=
   --  type transformations defining_type_transformation_set_identifier
   --  [ use_error_types ]
   --  {error_type_transformation } +
   --  end transformations;

   function P_Error_Type_Transformations return Node_Id is
      Error_Type_Transformations : Node_Id;
      Identifier : Node_Id;
      Use_Error_Types : Node_Id;
      Error_Type_Transformation : Node_Id;
      Error_Type_Transformation_List : List_Id;

      Loc, Loc_Start, Loc_End : Location;
   begin
      Error_Type_Transformations :=
      New_Node (K_Error_Type_Transformations, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         Scan_Token;
         DPE (PC_Error_Type_Transformations,
         Expected_Token => T_Identifier);
         return No_Node;
      end if;
      Set_Identifier (Error_Type_Transformations, Identifier);

      --  Scanning node 1

      Save_Lexer (Loc_Start);
      Use_Error_Types := P_Use_Error_Types;
      Save_Lexer (Loc_End);
      if Present (Use_Error_Types) then
         Set_Use_Error_Types (Error_Type_Transformations,
         Use_Error_Types);
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      --  Scanning node 2

      Error_Type_Transformation_List := New_List (K_List_Id, Token_Location);
      Set_Error_Type_Transformation_List
      (Error_Type_Transformations,  Error_Type_Transformation_List);

      --  { error_type_transformation } +
      loop
         Save_Lexer (Loc_Start);
         Error_Type_Transformation := P_Error_Type_Transformation;
         Save_Lexer (Loc_End);
         if Present (Error_Type_Transformation) then
            Append_Node_To_List (Error_Type_Transformation,
            Error_Type_Transformation_List);
         elsif Loc_Start /= Loc_End then
            return No_Node;
         end if;

         exit when Error_Type_Transformation = No_Node;
      end loop;
      if Is_Empty (Error_Type_Transformation_List) then
         Scan_Token;
         DPE (PC_Error_Type_Transformation,
         Expected_Tokens => (T_All,
                             T_Left_Brace));
         return No_Node;
      end if;

      --  Scanning : end transformations;

      Save_Lexer (Loc);
      if Next_Token = T_End then
         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         if Next_Token = T_Transformations then
            Restore_Lexer (Loc);
            Scan_Token;
            Save_Lexer (Loc);
            Scan_Token (T_Semi_Colon);
            if Token = T_Error then
               Restore_Lexer (Loc);
               Save_Lexer (Loc);
               Scan_Token;
               DPE (PC_Error_Type_Transformations,
               Expected_Token => T_Semi_Colon);
               Restore_Lexer (Loc);
            end if;
         else
            Restore_Lexer (Loc);
            Scan_Token;
            DPE (PC_Error_Type_Transformations,
            Expected_Token => T_Transformations);
            return No_Node;
         end if;
      else
         Scan_Token;
         DPE (PC_Error_Type_Transformations, Expected_Token => T_End);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      return Error_Type_Transformations;
   end P_Error_Type_Transformations;

   -------------------------------------
   -- P_Error_Model_Library_Reference --
   -------------------------------------

   --  error_model_library_reference ::=
   --    package_or_package_alias_identifier

   --  package_name ::=
   --    { package_identifier :: } * package_identifier

   function P_Error_Model_Library_Reference return Node_Id is
      Error_Model_Library_Reference : Node_Id;

      Identifiers : List_Id;
      Id_Package : Node_Id;

      Loc : Location;
   begin
      Error_Model_Library_Reference := New_Node
       (K_Error_Model_Library_Reference, Token_Location);

      Id_Package := P_Identifier;
      if No (Id_Package) then
         Scan_Token;
         DPE (PC_Error_Model_Library_Reference,
              Expected_Token => T_Identifier);
         return No_Node;
      end if;

      Identifiers := New_List (K_List_Id, Token_Location);
      Set_Identifiers (Error_Model_Library_Reference, Identifiers);
      Append_Node_To_List (Id_Package, Identifiers);

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Colon_Colon then
         while Token = T_Colon_Colon loop
            Id_Package := P_Identifier;
            if No (Id_Package) then
               Scan_Token;
               DPE (PC_Error_Model_Library_Reference,
                    Expected_Token => T_Identifier);
               return No_Node;
            end if;
            Append_Node_To_List (Id_Package, Identifiers);
            Save_Lexer (Loc);
            Scan_Token;
            if Token /= T_Colon_Colon then
               Restore_Lexer (Loc);
               exit;
            end if;
         end loop;
      else
         Restore_Lexer (Loc);
      end if;

      return Error_Model_Library_Reference;
   end P_Error_Model_Library_Reference;

   -------------------------------
   -- P_Error_Type_Library_List --
   -------------------------------

   --  error_type_library_list ::=
   --    error_model_library_reference { , error_model_library_reference } *

   function P_Error_Type_Library_List return List_Id is
      Error_Type_Library_List        : List_Id;

      Error_Model_Library_Reference  : Node_Id;

      Loc : Location;
   begin
      Error_Type_Library_List := New_List (K_List_Id, Token_Location);

      loop
         Save_Lexer (Loc);
         Error_Model_Library_Reference := P_Error_Model_Library_Reference;
         if No (Error_Model_Library_Reference) then
            return No_List;
         end if;

         Append_Node_To_List (Error_Model_Library_Reference,
         Error_Type_Library_List);

         Save_Lexer (Loc);
         exit when Next_Token /= T_Comma;
         Restore_Lexer (Loc);
         Scan_Token; -- ','
      end loop;

      return Error_Type_Library_List;
   end P_Error_Type_Library_List;

   ------------------------------------
   -- P_Error_Type_Library_List_Used --
   ------------------------------------

   --  The same as Error_Type_Library_List but it ends with ";"

   function P_Error_Type_Library_List_Used return List_Id is
      Error_Type_Library_List_Used : List_Id;
      Error_Type_Library_List : List_Id;
      Loc : Location;
   begin
      Error_Type_Library_List_Used := New_List (K_List_Id, Token_Location);

      Error_Type_Library_List := P_Error_Type_Library_List;
      if No (Error_Type_Library_List) then
         return No_List;
      end if;

      Error_Type_Library_List_Used := Error_Type_Library_List;

      Save_Lexer (Loc);
      if Next_Token /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Type_Library_List, Expected_Token => T_Semi_Colon);
         Restore_Lexer (Loc);
         return No_List;
      else
         Restore_Lexer (Loc);
         Scan_Token;
      end if;

      return Error_Type_Library_List_Used;
   end P_Error_Type_Library_List_Used;

   ----------------------------------------
   -- P_Error_Type_Library_List_Extended --
   ----------------------------------------

   --  The same as Error_Type_Library_List but it ends with "with"

   function P_Error_Type_Library_List_Extended return List_Id is
      Error_Type_Library_List_Extended : List_Id;
      Error_Type_Library_List : List_Id;
      Loc : Location;
   begin
      Error_Type_Library_List_Extended := New_List
      (K_List_Id, Token_Location);

      Error_Type_Library_List := P_Error_Type_Library_List;
      if No (Error_Type_Library_List) then
         return No_List;
      end if;

      Error_Type_Library_List_Extended := Error_Type_Library_List;

      Save_Lexer (Loc);
      if Next_Token /= T_With then
         Scan_Token;
         DPE (PC_Error_Type_Library_List, Expected_Token => T_With);
         Restore_Lexer (Loc);
      else
         Restore_Lexer (Loc);
         Scan_Token;
      end if;

      return Error_Type_Library_List_Extended;
   end P_Error_Type_Library_List_Extended;

   ----------------------------------
   -- P_Error_Type_Library_Element --
   ----------------------------------

   --  error_type_library_element ::=
   --  error_type_definition | error_type_alias
   --  | error_type_set_definition | error_type_set_alias

   function P_Error_Type_Library_Element return Node_Id is
      Error_Type_Library_Element : Node_Id;
      Element : Node_Id;

      --  For the same reason as the function P_Initiator_Reference
      --  i'm using pointer locations.
      Loc_Start, Loc_1, Loc_2, Loc_3 : Location;
   begin
      Error_Type_Library_Element :=
      New_Node (K_Error_Type_Library_Element, Token_Location);

      Save_Lexer (Loc_Start);
      Element := P_Error_Type_Definition;
      Save_Lexer (Loc_1);
      if Present (Element) then
         Set_Error_Type_Definition (Error_Type_Library_Element,
         Element);
      else
         if Loc_1 /= Loc_Start then
            return No_Node;
         end if;
         Element := P_Error_Type_Alias;
         Save_Lexer (Loc_2);
         if Present (Element) then
            Set_Error_Type_Alias (Error_Type_Library_Element,
            Element);
         else
            if Loc_2 /= Loc_Start then
               return No_Node;
            end if;
            Element := P_Error_Type_Set_Definition;
            Save_Lexer (Loc_3);
            if Present (Element) then
               Set_Error_Type_Set_Definition (Error_Type_Library_Element,
               Element);
            else
               if Loc_3 /= Loc_Start then
                  return No_Node;
               end if;
               Element := P_Error_Type_Set_Alias;
               if Present (Element) then
                  Set_Error_Type_Set_Alias (Error_Type_Library_Element,
                  Element);
               else
                  return No_Node;
               end if;
            end if;
         end if;
      end if;

      return Error_Type_Library_Element;
   end P_Error_Type_Library_Element;

   -----------------------------------------------
   -- P_Error_Type_Transformation_Set_Reference --
   -----------------------------------------------

   --  The type transformation set reference in a use transformations
   --  statement must exist in the namespace of the Error Model library
   --  containing the reference or in the Error Model library identified
   --  by the qualifying package name.

   --  error_type_transformation_set_Reference ::=
   --    [package_reference :: ] Error_Type_Transformations_identifier

   function P_Error_Type_Transformation_Set_Reference return Node_Id is
      Error_Type_Transformation_Set_Reference : Node_Id;

      Transformation_Set_Reference_Identifier : Node_Id;
      Package_Reference : Node_Id := No_Node;
      Identifiers : List_Id;
   begin
      Error_Type_Transformation_Set_Reference :=
      New_Node (K_Error_Type_Transformation_Set_Reference, Token_Location);

      P_Composed_Referenced_Identifier
      (Identifiers, Transformation_Set_Reference_Identifier,
      Error_Type_Transformation_Set_Reference,
      PC_Error_Type_Transformation_Set_Reference);

      if No (Error_Type_Transformation_Set_Reference) then
         return No_Node;
      end if;

      Set_Identifier (Error_Type_Transformation_Set_Reference,
      Transformation_Set_Reference_Identifier);

      if Present (Identifiers) and then not Is_Empty (Identifiers)
      then
         Package_Reference := New_Node (K_Package_Reference, Token_Location);
         Set_Identifiers (Package_Reference, Identifiers);
         Set_Package_Reference
           (Error_Type_Transformation_Set_Reference, Package_Reference);
      end if;

      return Error_Type_Transformation_Set_Reference;
   end P_Error_Type_Transformation_Set_Reference;

   ---------------------------------
   -- P_Error_Behavior_Event_List --
   ---------------------------------

   --  error_behavior_event ::=
   --    error_event | recover_event | repair_event

   function P_Error_Behavior_Event_List return List_Id is
      Error_Behavior_Event_List : List_Id;

      Error_Behavior_Event : Node_Id := No_Node;

      Loc, Loc_Start : Location;
      Token_Test : Token_Type;
   begin
      Error_Behavior_Event_List := New_List (K_List_Id, Token_Location);

      --  For the precision of error messages , i testes the first 3 tokens

      loop

         Save_Lexer (Loc_Start);
         Loc := Loc_Start;
         if Next_Token = T_Identifier then
            Restore_Lexer (Loc);
            Scan_Token;

            --  if token =':' => we have to retourn to the initial position
            --  pour faire des tests
            Save_Lexer (Loc);
            if Next_Token /= T_Colon then
               Restore_Lexer (Loc);
               Scan_Token;
               DPE (PC_Error_Behavior_Event,
               Expected_Token => T_Colon);
               return No_List;
            else
               --  If Next_Token = T_Colon
               Restore_Lexer (Loc);
               Scan_Token;
               Save_Lexer (Loc);
               Token_Test := Next_Token;
               Restore_Lexer (Loc);

               case Token_Test is
                  when T_R_Error =>
                     Restore_Lexer (Loc_Start);
                     Error_Behavior_Event := P_Error_Event;
                  when T_Recover =>
                     Restore_Lexer (Loc_Start);
                     Error_Behavior_Event := P_Recover_Event;
                  when T_Repair =>
                     Restore_Lexer (Loc_Start);
                     Error_Behavior_Event := P_Repair_Event;

                  when others =>
                     Scan_Token;
                     DPE (PC_Error_Behavior_Event,
                          Expected_Tokens => (T_R_Error,
                                              T_Recover,
                                              T_Repair));
                     return No_List;
               end case;

               Append_Node_To_List (Error_Behavior_Event,
               Error_Behavior_Event_List);

               if No (Error_Behavior_Event) then
                  return No_List;
               end if;
            end if;

         else
            Restore_Lexer (Loc);
            if Is_Empty (Error_Behavior_Event_List) then
               Scan_Token;
               DPE (PC_Error_Behavior_Event,
               Expected_Token => T_Identifier);
               return No_List;
            else
               exit;
            end if;
         end if;
         exit when Error_Behavior_Event = No_Node;

      end loop;

      return Error_Behavior_Event_List;
   end P_Error_Behavior_Event_List;

   ----------------------------
   -- P_Error_Behavior_State --
   ----------------------------

   --  error_behavior_state ::=
   --    defining_error_behavior_state_identifier : [ initial ] state
   --    [ error_type_set ] ;

   function P_Error_Behavior_State return Node_Id is
      Error_Behavior_State : Node_Id;

      Identifier : Node_Id;
      Error_Type_Set : List_Id;

      Next1, Next2, Next : Token_Type;
      Loc, Loc_Start : Location;
   begin
      Error_Behavior_State := New_Node (K_Error_Behavior_State,
                                        Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Set_Identifier (Error_Behavior_State, Identifier);

      --  ':'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Colon then
         Scan_Token;
         DPE (PC_Error_Behavior_State,
              Expected_Token => T_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      Save_Lexer (Loc_Start);
      Next1 := Next_Token;
      if Next1 /= T_State then
         if Next1 /= T_Initial then
            Restore_Lexer (Loc_Start);
            Scan_Token;
            DPE (PC_Error_Behavior_State,
            Expected_Token => T_State);
            return No_Node;
         else
            --  Next1 = T_Initial
            Restore_Lexer (Loc_Start);
            Scan_Token;
            Save_Lexer (Loc);
            Next2 := Next_Token;
            if Next2 /= T_State then
               Scan_Token;
               DPE (PC_Error_Behavior_State,
               Expected_Token => T_State);
               Restore_Lexer (Loc);
               return No_Node;
            else
               --  Next2 = T_State
               Restore_Lexer (Loc);
               Scan_Token;
            end if;
         end if;
      else
         Restore_Lexer (Loc_Start);
         Scan_Token;
      end if;

      --  '{'
      Save_Lexer (Loc);
      if Next_Token = T_Left_Brace then
         Restore_Lexer (Loc);
         Scan_Token;
         Error_Type_Set := P_Error_Type_Set;
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Error_Behavior_State, Error_Type_Set);
         else
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  ';'
      Save_Lexer (Loc);
      if Next_Token /= T_Semi_Colon then
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Error_Behavior_State,
         Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      return Error_Behavior_State;
   end P_Error_Behavior_State;

   ---------------------------------
   -- P_Error_Behavior_State_List --
   ---------------------------------

   function P_Error_Behavior_State_List return List_Id is
      Error_Behavior_State_List : List_Id;

      Error_Behavior_State : Node_Id := No_Node;

      Identifier : Node_Id;
      Loc_Start, Loc_End, Loc : Location;
   begin
      Error_Behavior_State_List := New_List (K_List_Id, Token_Location);

      --  The list requires at least one Error_Behavior_State
      Save_Lexer (Loc);
      Identifier := P_Identifier;
      if No (Identifier) then
         Scan_Token;
         DPE (PC_Error_Behavior_State,
         Expected_Token => T_Identifier);
         return No_List;
      else
         Restore_Lexer (Loc);
      end if;

      loop
         Save_Lexer (Loc_Start);
         Error_Behavior_State := P_Error_Behavior_State;
         Save_Lexer (Loc_End);
         if No (Error_Behavior_State) then
            if Loc_Start /= Loc_End then
               return No_List;
            else
               exit;
            end if;
         else
            Append_Node_To_List (Error_Behavior_State,
            Error_Behavior_State_List);
         end if;
      end loop;

      if Is_Empty (Error_Behavior_State_List) then
         Scan_Token;
         DPE (PC_Error_Behavior_State, EMC_List_Is_Empty, "");
         return No_List;
      end if;

      return Error_Behavior_State_List;
   end P_Error_Behavior_State_List;

   --------------------------------------
   -- P_Error_Behavior_Transition_List --
   --------------------------------------

   --  error_behavior_transition ::=
   --    transition | branching_transition

   function P_Error_Behavior_Transition_List return List_Id is
      Error_Behavior_Transition_List : List_Id;
      Error_Behavior_Transition : Node_Id;

      Loc_Start_1, Loc_End_1, Loc_Start_2, Loc_End_2 : Location;
   begin
      Error_Behavior_Transition_List := New_List (K_List_Id, Token_Location);

      loop
         Save_Lexer (Loc_Start_1);
         Error_Behavior_Transition := P_Transition;
         Save_Lexer (Loc_End_1);
         if No (Error_Behavior_Transition) then
            Save_Lexer (Loc_Start_2);
            Error_Behavior_Transition := P_Branching_Transition;
            Save_Lexer (Loc_End_2);
            if No (Error_Behavior_Transition) then
               --  The list of the node 'error_behavior_transition'
               --  must contain at least 1 node
               if Loc_Start_1 = Loc_End_1 and then
                  Loc_Start_2 = Loc_End_2 and then
                  Is_Empty (Error_Behavior_Transition_List)
               then
                  Scan_Token;
                  DPE (PC_Error_Behavior_Transition,
                  Expected_Tokens => (T_All,
                                      T_Identifier));
                  --  A revoir : branching_transition de quoi il est formé

                  return No_List;
               elsif Loc_Start_1 /= Loc_End_1 or else
                     Loc_Start_2 /= Loc_End_2
               then
                  return No_List;
               else
                  exit;
               end if;
            end if;
         end if;
         Append_Node_To_List (Error_Behavior_Transition,
         Error_Behavior_Transition_List);
      end loop;

      if Is_Empty (Error_Behavior_Transition_List) then
         return No_List;
      end if;

      return Error_Behavior_Transition_List;
   end P_Error_Behavior_Transition_List;

   --------------------------
   -- P_Error_Type_Mapping --
   --------------------------

   --  error_type_mapping ::=
   --  source_error_type_set -> target_error_type_instance ;

   function P_Error_Type_Mapping return Node_Id is
      Error_Type_Mapping : Node_Id;
      Source_Error_Type_Set : List_Id;
      Target_Error_Type_Instance : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_Type_Mapping := New_Node
      (K_Error_Type_Mapping, Token_Location);

      --  source_error_type_set
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Source_Error_Type_Set := P_Error_Type_Set;
         if No (Source_Error_Type_Set) then
            return No_Node;
         end if;
         Set_Source_Error_Type_Set (Error_Type_Mapping, Source_Error_Type_Set);
      else
         Scan_Token;
         DPE (PC_Source_Error_Type_Set,
              Expected_Token => T_Left_Brace);
         return No_Node;
      end if;

      --  '->'
      Save_Lexer (Loc);
      if Next_Token /= T_Direct_Connection then
         Scan_Token;
         DPE (PC_Error_Type_Mapping,
              Expected_Token => T_Direct_Connection);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      Target_Error_Type_Instance := P_Target_Error_Type_Instance;
      if No (Target_Error_Type_Instance) then
         return No_Node;
      end if;
      Set_Target_Error_Type_Instance (Error_Type_Mapping,
      Target_Error_Type_Instance);

      --  ';'
      Save_Lexer (Loc);
      if Next_Token /= T_Semi_Colon then
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Error_Type_Mapping,
              Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Restore_Lexer (Loc);
         Scan_Token;
      end if;

      return Error_Type_Mapping;
   end P_Error_Type_Mapping;

   -----------------------
   -- P_Use_Error_Types --
   -----------------------

   --  The use_error_types (use types) clause makes the defining identifiers
   --  of error types and type sets from the listed Error Model libraries
   --  visible within the error type mappings or error type transformations
   --  declaration.

   --  use_error_types ::=
   --   use types error_type_library_list ;

   function P_Use_Error_Types return Node_Id is
      Use_Error_Types : Node_Id := No_Node;

      Error_Type_Library_List : List_Id;

      Next : Token_Type;
      Loc : Location;
   begin
      Use_Error_Types := New_Node (K_Use_Error_Types, Token_Location);

      Save_Lexer (Loc);
      Next := Next_Token;
      if Next = T_Use then
         Restore_Lexer (Loc);
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         if Next = T_Types then
            Restore_Lexer (Loc);
            Scan_Token;
            Error_Type_Library_List := P_Error_Type_Library_List_Used;
            if No (Error_Type_Library_List) then
               return No_Node;
            end if;
            Set_Error_Type_Library_List (Use_Error_Types,
            Error_Type_Library_List);
         else
            Restore_Lexer (Loc);
            Scan_Token;
            DPE (PC_Use_Error_Types,
                 Expected_Token => T_Types);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      return Use_Error_Types;
   end P_Use_Error_Types;

   ---------------------------------
   -- P_Error_Type_Transformation --
   ---------------------------------

   --  error_type_transformation ::=
   --   ( source_error_type_set_or_noerror | all )
   --   –[ [ contributor_error_type_set_or_noerror ] ]->
   --   target_error_type_instance ;

   function P_Error_Type_Transformation return Node_Id is
      Error_Type_Transformation : Node_Id;
      Source_Error_Type_Set_Or_Noerror : Node_Id;
      Contributor_Error_Type_Set_Or_Noerror : Node_Id;
      Target_Error_Type_Instance : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_Type_Transformation := New_Node
      (K_Error_Type_Transformation, Token_Location);

      --  all
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      --  source_error_type_set_no_error
      else
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Left_Brace then
            Scan_Token;
            Source_Error_Type_Set_Or_Noerror := P_Error_Type_Set_Or_Noerror;
            if Present (Source_Error_Type_Set_Or_Noerror) then
               Set_Source_Error_Type_Set_Or_Noerror (Error_Type_Transformation,
               Source_Error_Type_Set_Or_Noerror);
            end if;
         else
            return No_Node;
         end if;
      end if;

      --  Ce test sert à ne pas anticiper les messages d'erreurs
      if Token /= T_Right_Brace and then Token /= T_All
      and then No (Source_Error_Type_Set_Or_Noerror)
      then
         return No_Node;
      end if;

      --  '-['
      Save_Lexer (Loc);
      if Next_Token = T_Left_Step_Bracket then
         Restore_Lexer (Loc);
         Scan_Token;

         --  Contributor_Error_Type_Set
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Left_Brace then
            Scan_Token;
            Contributor_Error_Type_Set_Or_Noerror :=
            P_Error_Type_Set_Or_Noerror;
            Set_Contributor_Error_Type_Set_Or_Noerror
            (Error_Type_Transformation, Contributor_Error_Type_Set_Or_Noerror);
            if No (Contributor_Error_Type_Set_Or_Noerror) then
               return No_Node;
            end if;
         end if;

         --  ']->'
         Save_Lexer (Loc);
         if Next_Token = T_Right_Step_Bracket then
            Restore_Lexer (Loc);
            Scan_Token;

            --  Target_Error_Type_Instance
            Target_Error_Type_Instance := P_Target_Error_Type_Instance;
            if No (Target_Error_Type_Instance) then
               return No_Node;
            else

               --  ';'
               Save_Lexer (Loc);
               if Next_Token /= T_Semi_Colon then
                  Restore_Lexer (Loc);
                  Scan_Token;
                  DPE (PC_Error_Type_Transformation,
                  Expected_Token => T_Semi_Colon);
                  return No_Node;
               else
                  Restore_Lexer (Loc);
                  Scan_Token;
               end if;

            end if;

         else
            Restore_Lexer (Loc);
            Scan_Token;
            DPE (PC_Error_Type_Transformation,
            Expected_Token => T_Right_Step_Bracket);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Error_Type_Transformation,
         Expected_Token => T_Left_Step_Bracket);
         return No_Node;
      end if;

      return Error_Type_Transformation;
   end P_Error_Type_Transformation;

   -----------------------------
   -- P_Error_Type_Definition --
   -----------------------------

   --  error_type_definition ::=
   --  defining_error_type_identifier : type
   --  [ extends error_type_reference ] ;

   function P_Error_Type_Definition return Node_Id is
      Error_Type_Definition : Node_Id;
      Identifier : Node_Id;
      Start_Loc, Loc : Location;
      Error_Type_Reference : Node_Id;
      Next : Token_Type;
   begin
      Error_Type_Definition := New_Node
      (K_Error_Type_Definition, Token_Location);

      --  All nodes begin with an identifier ,
      --  we cannot determinate exactly the node type
      --  with the first token , so we have to advance the
      --  pointer to the second token
      --  => if wrong the pointer returns to the initial position.
      Save_Lexer (Start_Loc);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Set_Identifier (Error_Type_Definition, Identifier);

      --  ':'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Colon then
         if Next /= T_Renames then
            Scan_Token;
            DPE (PC_Error_Type_Library_Element,
            Expected_Tokens => (T_Colon,
                                T_Renames));
            Restore_Lexer (Loc);
         end if;
         Restore_Lexer (Start_Loc);
         --  error => initial position
         return No_Node;
      end if;
      Scan_Token;

      --  'type'
      Save_Lexer (Loc);
      if Next_Token /= T_Type then
         Scan_Token;
         DPE (PC_Error_Type_Library_Element,
         Expected_Token => T_Type);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      --  extends error_type_reference
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Extends then
         Scan_Token;
         Error_Type_Reference := P_Error_Type_Reference;
         if No (Error_Type_Reference) then
            return No_Node;
         else
            Set_Error_Type_Reference (Error_Type_Definition,
            Error_Type_Reference);
         end if;
      elsif Next = T_Set then
         Restore_Lexer (Start_Loc);
         --  error => initial position
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      if Next_Token /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Type_Definition,
         Expected_Token => T_Semi_Colon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      return Error_Type_Definition;
   end P_Error_Type_Definition;

   ------------------------
   -- P_Error_Type_Alias --
   ------------------------

   --  error_type_alias ::=
   --  defining_error_type_alias_identifier renames type error_type_reference ;

   function P_Error_Type_Alias return Node_Id is
      Error_Type_Alias : Node_Id;
      Error_Type_Reference : Node_Id;
      Identifier : Node_Id;
      Start_Loc, Loc : Location;
      Next : Token_Type;
   begin
      Error_Type_Alias := New_Node
      (K_Error_Type_Alias, Token_Location);

      Save_Lexer (Start_Loc);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Set_Identifier (Error_Type_Alias, Identifier);

      --  'renames'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Renames then
         Restore_Lexer (Start_Loc);
         --  error => initial position
         return No_Node;
      end if;
      Scan_Token;

      --  'type'
      Save_Lexer (Loc);
      if Next_Token /= T_Type then
         Scan_Token;
         DPE (PC_Error_Type_Library_Element,
         Expected_Token => T_Type);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      --  Testing if Next_Token is 'set'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Set then
         Restore_Lexer (Start_Loc);
         --  error => initial position
         return No_Node;
      elsif Next /= T_Identifier then
         Scan_Token;
         DPE (PC_Error_Type_Library_Element,
         Expected_Tokens => (T_Identifier,
                             T_Set));
         Restore_Lexer (Start_Loc);
         return No_Node;
      end if;

      --  error_type_reference
      Save_Lexer (Loc);
      Error_Type_Reference := P_Error_Type_Reference;
      if No (Error_Type_Reference) then
         Restore_Lexer (Loc);
         return No_Node;
      else
         Set_Error_Type_Reference (Error_Type_Alias,
         Error_Type_Reference);
      end if;

      --  ';'
      Save_Lexer (Loc);
      if Next_Token /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Type_Alias,
         Expected_Token => T_Semi_Colon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      return Error_Type_Alias;
   end P_Error_Type_Alias;

   ---------------------------------
   -- P_Error_Type_Set_Definition --
   ---------------------------------

   --  error_type_set_definition ::=
   --  defining_error_type_set_identifier : type set error_type_set ;

   function P_Error_Type_Set_Definition return Node_Id is
      Error_Type_Set_Definition : Node_Id;
      Error_Type_Set : List_Id;
      Identifier : Node_Id;
      Loc, Loc_Start : Location;
      Next : Token_Type;
   begin
      Error_Type_Set_Definition := New_Node
      (K_Error_Type_Set_Definition, Token_Location);

      --  The node may be error_type_set_alias so we have to
      --  return to the first position
      Save_Lexer (Loc_Start);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Set_Identifier (Error_Type_Set_Definition, Identifier);

      --  ':'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Colon then
         --  the node may be error_type_set_alias
         Restore_Lexer (Loc_Start);
         return No_Node;
      end if;
      Scan_Token;

      --  'type'
      Save_Lexer (Loc);
      if Next_Token /= T_Type then
         Scan_Token;
         DPE (PC_Error_Type_Set_Definition,
         Expected_Token => T_Type);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      --  'set'
      Save_Lexer (Loc);
      if Next_Token /= T_Set then
         Scan_Token;
         DPE (PC_Error_Type_Set_Definition,
         Expected_Token => T_Set);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      --  error_type_set
      --  '{'
      Save_Lexer (Loc);
      if Next_Token /= T_Left_Brace then
         Scan_Token;
         DPE (PC_Error_Type_Set, Expected_Token => T_Left_Brace);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      Error_Type_Set := P_Error_Type_Set;
      if No (Error_Type_Set) then
         return No_Node;
      else
         Set_Error_Type_Set (Error_Type_Set_Definition,
         Error_Type_Set);
      end if;

      --  ';'
      Save_Lexer (Loc);
      if Next_Token /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Type_Set_Definition,
         Expected_Token => T_Semi_Colon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      return Error_Type_Set_Definition;
   end P_Error_Type_Set_Definition;

   ----------------------------
   -- P_Error_Type_Set_Alias --
   ----------------------------

   --  error_type_set_alias ::=
   --  defining_error_type_set_alias_identifier renames type set
   --  error_type_set_reference ;

   function P_Error_Type_Set_Alias return Node_Id  is
      Error_Type_Set_Alias : Node_Id;

      Error_Type_Set_Reference : Node_Id;
      Identifier : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_Type_Set_Alias := New_Node
      (K_Error_Type_Set_Alias, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Set_Identifier (Error_Type_Set_Alias, Identifier);

      --  'renames'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Renames then
         return No_Node;
      end if;
      Scan_Token;

      --  'type'
      Save_Lexer (Loc);
      if Next_Token /= T_Type then
         Scan_Token;
         DPE (PC_Error_Type_Library_Element,
         Expected_Token => T_Type);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      --  'set'
      Save_Lexer (Loc);
      if Next_Token /= T_Set then
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      --  error_type_set_reference
      Save_Lexer (Loc);
      Error_Type_Set_Reference := P_Error_Type_Set_Reference;
      if No (Error_Type_Set_Reference) then
         Restore_Lexer (Loc);
         return No_Node;
      else
         Set_Error_Type_Set_Reference (Error_Type_Set_Alias,
         Error_Type_Set_Reference);
      end if;

      --  ';'
      Save_Lexer (Loc);
      if Next_Token /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Type_Set_Alias,
         Expected_Token => T_Semi_Colon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;

      return Error_Type_Set_Alias;
   end P_Error_Type_Set_Alias;

   -------------------
   -- P_Error_Event --
   -------------------

   --  error_event ::=
   --   defining_error_behavior_event_identifier : error event
   --   [ error_type_set ]
   --   [ if error_event_condition ] ;

   function P_Error_Event return Node_Id is
      Error_Event : Node_Id;

      Identifier : Node_Id;
      Error_Type_Set : List_Id;
      Error_Event_Condition : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_Event := New_Node (K_Error_Event, Token_Location);

      --  It is useless to test the first 3 tokens
      --  since the caller function does that already

      --  identifier
      Identifier := P_Identifier;
      Set_Identifier (Error_Event, Identifier);
      --  ':'
      Scan_Token;
      --  'error'
      Scan_Token;

      --  'event'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Event then
         Scan_Token;
         DPE (PC_Error_Event, Expected_Token => T_Event);
         return No_Node;
      end if;
      Scan_Token;

      --  '{'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Error_Type_Set := P_Error_Type_Set;
         Set_Error_Type_Set (Error_Event, Error_Type_Set);
         if No (Error_Type_Set) then
            return No_Node;
         end if;
      end if;

      --  'if'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_If then
         Scan_Token;
         Error_Event_Condition := P_Error_Event_Condition;
         Set_Error_Event_Condition (Error_Event,
         Error_Event_Condition);
         if No (Error_Event_Condition) then
            return No_Node;
         end if;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Event, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;
      Scan_Token;

      return Error_Event;
   end P_Error_Event;

   ---------------------
   -- P_Recover_Event --
   ---------------------

   --  recover_event ::=
   --   defining_error_behavior_event_identifier : recover event
   --   [ when recover_event_initiators ]
   --   [ if error_event_condition ] ;

   function P_Recover_Event return Node_Id is
      Recover_Event : Node_Id;

      Identifier : Node_Id;
      Error_Event_Condition : Node_Id;
      Recover_Event_Initiators : List_Id;

      Next : Token_Type;
      Loc : Location;
   begin
      Recover_Event := New_Node (K_Recover_Event, Token_Location);

      --  identifier
      Identifier := P_Identifier;
      Set_Identifier (Recover_Event, Identifier);
      --  ':'
      Scan_Token;
      --  'recover'
      Scan_Token;

      --  'event'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Event then
         Scan_Token;
         DPE (PC_Recover_Event, Expected_Token => T_Event);
         return No_Node;
      end if;
      Scan_Token;

      --  'when'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_When then
         Scan_Token;
         Recover_Event_Initiators := P_Event_Initiators;
         Set_Recover_Event_Initiators (Recover_Event,
         Recover_Event_Initiators);
         if No (Recover_Event_Initiators) then
            return No_Node;
         end if;
      end if;

      --  'if'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_If then
         Scan_Token;
         Error_Event_Condition := P_Error_Event_Condition;
         Set_Error_Event_Condition (Recover_Event,
         Error_Event_Condition);
         if No (Error_Event_Condition) then
            return No_Node;
         end if;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Recover_Event, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;
      Scan_Token;

      return Recover_Event;
   end P_Recover_Event;

   --------------------
   -- P_Repair_Event --
   --------------------

   --  repair_event ::=
   --   defining_error_behavior_event_identifier : repair event
   --   [ repair_event_initiation ] ;

   function P_Repair_Event return Node_Id is
      Repair_Event : Node_Id;

      Repair_Event_Initiation : Node_Id;
      Identifier : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Repair_Event := New_Node (K_Repair_Event, Token_Location);

      --  identifier
      Identifier := P_Identifier;
      Set_Identifier (Repair_Event, Identifier);
      --  ':'
      Scan_Token;
      --  'repair'
      Scan_Token;

      --  'event'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Event then
         Scan_Token;
         DPE (PC_Repair_Event, Expected_Token => T_Event);
         return No_Node;
      end if;
      Scan_Token;

      --  'repair_event_initiation'
      Repair_Event_Initiation := P_Event_Initiation;
      Set_Repair_Event_Initiation (Repair_Event,
      Repair_Event_Initiation);

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Repair_Event, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;
      Scan_Token;

      return Repair_Event;
   end P_Repair_Event;

   ----------------------
   -- P_Error_Type_Set --
   ----------------------

   --  error_type_set ::=
   --   { type_set_element { , type_set_element } * }

   --  Before using this function we have to make sure that '{'
   --  already exists

   function P_Error_Type_Set return List_Id is
      Error_Type_Set : List_Id;
      Type_Set_Element : Node_Id;
      Loc_Start, Loc_End, Loc : Location;
   begin
      Error_Type_Set := New_List (K_List_Id, Token_Location);
      --  type_set_element { , type_set_element } *
      loop
         Save_Lexer (Loc_Start);
         Type_Set_Element := P_Type_Set_Element;
         Save_Lexer (Loc_End);
         if No (Type_Set_Element) then
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Type_Set_Element,
               Expected_Token => T_Identifier);
            end if;
            return No_List;
         end if;

         Append_Node_To_List (Type_Set_Element, Error_Type_Set);

         Save_Lexer (Loc);
         exit when Next_Token /= T_Comma;
         Restore_Lexer (Loc);
         Scan_Token; -- ','
      end loop;

      --  '}'
      Save_Lexer (Loc);
      if Next_Token /= T_Right_Brace then
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Type_Set_Element,
         Expected_Token => T_Right_Brace);
         return No_List;
      else
         Restore_Lexer (Loc);
         Scan_Token;
      end if;

      return Error_Type_Set;
   end P_Error_Type_Set;

   ---------------------------------
   -- P_Error_Type_Set_Or_Noerror --
   ---------------------------------

   --  error_type_set_or_noerror ::=
   --    error_type_set | { noerror }

   --  We have to test if we have '{' before entering the function

   function P_Error_Type_Set_Or_Noerror return Node_Id is
      Error_Type_Set_Or_No_Error : Node_Id;
      Error_Type_Set : List_Id := No_List;

      Type_Set_Element : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
   begin
      Error_Type_Set_Or_No_Error := New_Node (K_Error_Type_Set_Or_No_Error,
      Token_Location);

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Noerror then
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Right_Brace then
            Scan_Token;
         else
            Scan_Token;
            DPE (PC_Error_Type_Set_Or_No_Error,
            Expected_Token => T_Right_Brace);
            return No_Node;
         end if;
      else
         Save_Lexer (Loc);
         Save_Lexer (Loc_Start);
         Type_Set_Element := P_Type_Set_Element;
         Save_Lexer (Loc_End);
         if Present (Type_Set_Element) then
            Restore_Lexer (Loc);
         elsif Loc_Start /= Loc_End then
            return No_Node;
         else
            Scan_Token;
            DPE (PC_Error_Type_Set_Or_No_Error,
                 Expected_Tokens => (T_Noerror,
                                     T_Identifier));
            return No_Node;
         end if;

         Save_Lexer (Loc_Start);
         Error_Type_Set := P_Error_Type_Set;
         Save_Lexer (Loc_End);
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Error_Type_Set_Or_No_Error,
            Error_Type_Set);
         else
            return No_Node;
         end if;
      end if;

      return Error_Type_Set_Or_No_Error;
   end P_Error_Type_Set_Or_Noerror;

   ----------------------------------
   -- P_Target_Error_Type_Instance --
   ----------------------------------

   --  target_error_type_instance ::=
   --    { error_type_reference | error_type_product }

   function P_Target_Error_Type_Instance return Node_Id is
      Target_Error_Type_Instance : Node_Id;

      Error_Type_Product : List_Id;
      Error_Type_Reference : Node_Id;

      Loc, Loc_Start : Location;
      Next : Token_Type;
   begin
      Target_Error_Type_Instance := New_Node
      (K_Target_Error_Type_Instance, Token_Location);

      --  '{'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Left_Brace then
         Scan_Token;
         DPE (PC_Target_Error_Type_Instance,
              Expected_Token => T_Left_Brace);
         return No_Node;
      else
         Scan_Token;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Identifier then
         DPE (PC_Target_Error_Type_Instance, PC_Error_Type_Reference,
         PC_Error_Type_Product);
         return No_Node;
      end if;

      Save_Lexer (Loc_Start);
      Error_Type_Reference := P_Error_Type_Reference;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Star then
         Restore_Lexer (Loc_Start);
         Error_Type_Product := P_Error_Type_Product;
         if Present (Error_Type_Product) then
            Set_Error_Type_Product (Target_Error_Type_Instance,
            Error_Type_Product);
         else
            return No_Node;
         end if;
      elsif Present (Error_Type_Reference) then
         Set_Error_Type_Reference (Target_Error_Type_Instance,
         Error_Type_Reference);
      else
         return No_Node;
      end if;

      --  '}'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Right_Brace then
         Scan_Token;
         DPE (PC_Target_Error_Type_Instance,
              Expected_Token => T_Right_Brace);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Target_Error_Type_Instance;
   end P_Target_Error_Type_Instance;

   ------------------
   -- P_Transition --
   ------------------

   --  transition ::=
   --   [ defining_error_transition_identifier : ]
   --   error_source_state –[ error_condition ]->
   --   ( error_transition_target | error_transition_branch ) ;

   function P_Transition return Node_Id is
      Transition : Node_Id;

      Identifier : Node_Id;
      Error_Source_State : Node_Id;
      Error_Condition : Node_Id;
      Error_Transition_Target : Node_Id := No_Node;
      Error_Transition_Branch : Node_Id := No_Node;

      Loc, Loc_Begin, Loc_End : Location;
      Next : Token_Type;
   begin
      Transition := New_Node (K_Transition, Token_Location);

      --  defining_error_transition_identifier
      Identifier := P_Identifier;
      if Present (Identifier) then
         --  ':'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next /= T_Colon then
            Scan_Token;
            DPE (PC_Transition, Expected_Token => T_Colon);
            return No_Node;
         else
            Scan_Token;
         end if;
      end if;
      Set_Identifier (Transition, Identifier);

      --  Error_Source_State
      Error_Source_State := P_Error_Source_State;
      if No (Error_Source_State) then
         return No_Node;
      end if;
      Set_Error_Source_State (Transition, Error_Source_State);

      --  '–['
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Left_Step_Bracket then
         Scan_Token;
         DPE (PC_Transition, Expected_Token => T_Left_Step_Bracket);
         return No_Node;
      end if;
      Scan_Token;

      --  error_condition
      Save_Lexer (Loc_Begin);
      Error_Condition := P_Error_Condition;
      Save_Lexer (Loc_End);
      if No (Error_Condition) then
         if Loc_Begin = Loc_End then
            Scan_Token;
            DPE (PC_Transition, PC_Error_Condition,
            PC_Error_Condition_Trigger, Expected_Tokens =>
            (T_Left_Paren, T_Numeric_Literal));
         end if;
         return No_Node;
      end if;
      Set_Error_Condition (Transition, Error_Condition);

      --  ']->'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Right_Step_Bracket then
         Scan_Token;
         DPE (PC_Transition, Expected_Token => T_Right_Step_Bracket);
         return No_Node;
      end if;
      Scan_Token;

      --  ( error_transition_target | error_transition_branch )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Identifier or else Next = T_Same
      then
         Save_Lexer (Loc_Begin);
         Error_Transition_Target := P_Error_Transition_Target;
         Save_Lexer (Loc_End);
         if No (Error_Transition_Target) and then Loc_Begin /= Loc_End
         then
            return No_Node;
         elsif Present (Error_Transition_Target) then
            Set_Error_Transition_Target (Transition,
            Error_Transition_Target);
         end if;
      elsif Next = T_Left_Paren then
         Error_Transition_Branch := P_Error_Transition_Branch;
         if Present (Error_Transition_Branch) then
            Set_Error_Transition_Branch (Transition,
            Error_Transition_Branch);
         else
            return No_Node;
         end if;
      end if;

      if No (Error_Transition_Target) and then No (Error_Transition_Branch)
      then
         Scan_Token;
         DPE (PC_Transition, Expected_Tokens => (T_State,
                                                 T_Target_Error_State_Id,
                                                 T_Left_Paren));
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Transition, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;
      Scan_Token;

      return Transition;
   end P_Transition;

   ----------------------------
   -- P_Branching_Transition --
   ----------------------------

   --  A revoir

   --  A transition can be a branching transition
   --  with multiple target states.

   --  branching_transition ::=
   --   [ defining_error_transition_identifier : ]
   --   error_source_state –[ error_condition ]->
   --   ( ( error_transition_target )+ | error_transition_branch ) ;

   function P_Branching_Transition return Node_Id is
      --  Branching_Transition : Node_Id;
   begin
      --  Branching_Transition := New_Node (K_Transition, Token_Location);
      --  return Branching_Transition;
      return No_Node;
   end P_Branching_Transition;

   ----------------------------
   -- P_Error_Type_Reference --
   ----------------------------

   --  error_type_reference ::=
   --     [ error_model_library_reference :: ] error_type_identifier

   function P_Error_Type_Reference return Node_Id is
      Error_Type_Reference : Node_Id;

      Error_Model_Library_Reference : Node_Id;
      Error_Type_Identifier : Node_Id;
      Identifiers : List_Id;
   begin
      Error_Type_Reference := New_Node (K_Error_Type_Reference,
      Token_Location);

      P_Composed_Referenced_Identifier
      (Identifiers, Error_Type_Identifier,
      Error_Type_Reference, PC_Error_Type_Reference);

      if No (Error_Type_Reference) then
         return No_Node;
      end if;

      Set_Identifier (Error_Type_Reference,
      Error_Type_Identifier);

      if Present (Identifiers) and then not Is_Empty (Identifiers)
      then
         Error_Model_Library_Reference := New_Node
            (K_Error_Model_Library_Reference, Token_Location);
         Set_Identifiers (Error_Model_Library_Reference, Identifiers);
         Set_Error_Model_Library_Reference
           (Error_Type_Reference, Error_Model_Library_Reference);
      end if;

      return Error_Type_Reference;
   end P_Error_Type_Reference;

   --------------------------------
   -- P_Error_Type_Set_Reference --
   --------------------------------

   --  error_type_set_reference ::=
   --    [error_model_library_reference :: ] error_type_set_identifier

   function P_Error_Type_Set_Reference return Node_Id is
      Error_Type_Set_Reference : Node_Id;

      Error_Model_Library_Reference : Node_Id;
      Error_Type_Set_Identifier : Node_Id;
      Identifiers : List_Id;
   begin
      Error_Type_Set_Reference :=
      New_Node (K_Error_Type_Set_Reference, Token_Location);

      P_Composed_Referenced_Identifier
      (Identifiers, Error_Type_Set_Identifier,
      Error_Type_Set_Reference, PC_Error_Type_Set_Reference);

      if No (Error_Type_Set_Reference) then
         return No_Node;
      end if;

      Set_Identifier (Error_Type_Set_Reference,
      Error_Type_Set_Identifier);

      if Present (Identifiers) and then not Is_Empty (Identifiers)
      then
         Error_Model_Library_Reference := New_Node
            (K_Error_Model_Library_Reference, Token_Location);
         Set_Identifiers (Error_Model_Library_Reference, Identifiers);
         Set_Error_Model_Library_Reference
           (Error_Type_Set_Reference, Error_Model_Library_Reference);
      end if;

      return Error_Type_Set_Reference;
   end P_Error_Type_Set_Reference;

   -----------------------------
   -- P_Error_Event_Condition --
   -----------------------------

   --  error_event_condition ::= string_literal

   function P_Error_Event_Condition return Node_Id is
      Error_Event_Condition : Node_Id;
      Loc : Location;
   begin
      Error_Event_Condition :=
      New_Node (K_Literal, Token_Location);

      Save_Lexer (Loc);
      if Next_Token = T_String_Literal then
         Restore_Lexer (Loc);
         Scan_Token;
         Set_Value (Error_Event_Condition,
         New_String_Value (String_Literal_Value));
      else
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Error_Event_Condition,
         Expected_Token => T_String_Literal);
         return No_Node;
      end if;

      return Error_Event_Condition;
   end P_Error_Event_Condition;

   ------------------------
   -- P_Event_Initiators --
   ------------------------

   --  event_initiators ::=
   --   ( initiator_reference { , initiator_reference }* )

   --  a list must be between ()

   --  since event_initiators is only used in recover_event ,
   --  i can call it : PC_Recover_Event_Initiators instead of
   --  PC_Event_Initiators

   function P_Event_Initiators return List_Id is
      Event_Initiators : List_Id;
      Initiator_Reference : Node_Id;

      Next : Token_Type;
      Loc : Location;
   begin
      Event_Initiators := New_List (K_List_Id, Token_Location);

      --  '('
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Recover_Event_Initiators,
         Expected_Token => T_Left_Paren);
         return No_List;
      end if;

      --  initiator_reference { , initiator_reference }*
      loop
         Initiator_Reference := P_Initiator_Reference;
         if No (Initiator_Reference) then
            return No_List;
         end if;

         Append_Node_To_List (Initiator_Reference, Event_Initiators);

         Save_Lexer (Loc);
         exit when Next_Token /= T_Comma;
         Restore_Lexer (Loc);
         Scan_Token; -- ','
      end loop;

      --  ')'
      Save_Lexer (Loc);
      if Next_Token /= T_Right_Paren then
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Recover_Event_Initiators,
         Expected_Token => T_Right_Paren);
         return No_List;
      else
         Restore_Lexer (Loc);
         Scan_Token;
      end if;

      return Event_Initiators;
   end P_Event_Initiators;

   ---------------------------
   -- P_Initiator_Reference --
   ---------------------------

   --  initiator_reference ::=
   --    mode_transition_reference | port_reference | self_event_reference

   function P_Initiator_Reference return Node_Id is
      Initiator_Reference : Node_Id;

      Initiator_Reference_Node : Node_Id;

      --  I'm using location to enhance error messages
      --  for example : if in P_Port_Reference we began
      --  treating it and at some point there was something
      --  missing the location will be <> from the location of
      --  start , unlike if the node is completely mismach,
      --  both locations will be equal
      Loc_Start, Loc_Fin_Func1, Loc_Fin_Func2, Loc_Fin_Func3 : Location;
   begin
      Initiator_Reference := New_Node (K_Initiator_Reference,
      Token_Location);

      Save_Lexer (Loc_Start);
      Initiator_Reference_Node := P_Port_Reference;
      Save_Lexer (Loc_Fin_Func1);
      if Present (Initiator_Reference_Node) then
         Set_Port_Reference (Initiator_Reference, Initiator_Reference_Node);
         return Initiator_Reference;
      else
         Initiator_Reference_Node := P_Mode_Transition_Reference;
         Save_Lexer (Loc_Fin_Func2);
         if Present (Initiator_Reference_Node) then
            Set_Mode_Transition_Reference (Initiator_Reference,
            Initiator_Reference_Node);
            return Initiator_Reference;
         else
            Initiator_Reference_Node := P_Self_Event_Reference;
            Save_Lexer (Loc_Fin_Func3);
            if Present (Initiator_Reference_Node) then
               Set_Self_Event_Reference (Initiator_Reference,
               Initiator_Reference_Node);
               return Initiator_Reference;
            else
               --  if the 3 positions on the pointer after the functions
               --  are equal to 'Loc_Start' then we have to display this error
               --  message
               if Loc_Start = Loc_Fin_Func1 and then
               Loc_Start = Loc_Fin_Func2 and then
               Loc_Start = Loc_Fin_Func3
               then
                  Scan_Token;
                  DPE (PC_Initiator_Reference, PC_Mode_Transition_Reference,
                  PC_Port_Reference, PC_Self_Event_Reference);
               end if;
               return No_Node;
            end if;
         end if;
      end if;

   end P_Initiator_Reference;

   ----------------------
   -- P_Port_Reference --
   ----------------------

   --  port_reference ::=
   --   { featuregroup_identifier . } * port_identifier

   function P_Port_Reference return Node_Id is
      Port_Reference : Node_Id;

      Featuregroup_Identifier_List : List_Id := No_List;
      Featuregroup_Identifier : Node_Id;
      Identifier : Node_Id := No_Node;
      Node_Inter : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Port_Reference := New_Node (K_Port_Reference,
      Token_Location);

      --  first identifier : it may be featuregroup_identifier
      --  or port_identifier
      Node_Inter := P_Identifier;
      if No (Node_Inter) then
         return No_Node;
      end if;

      --  '.'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Dot then
         Scan_Token;
         Featuregroup_Identifier_List := New_List (K_List_Id,
         Token_Location);
         Append_Node_To_List (Node_Inter,
         Featuregroup_Identifier_List);
         loop
            Featuregroup_Identifier := P_Identifier;
            if No (Featuregroup_Identifier) then
               Scan_Token;
               DPE (PC_Port_Reference,
               Expected_Token => T_Identifier);
               return No_Node;
            end if;

            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            exit when Next /= T_Dot;
            Scan_Token;

            --  if there is no dot after the identifier then it is
            --  feature_identifier
            --  => we dont do append unless we find a dot
            Append_Node_To_List (Featuregroup_Identifier,
            Featuregroup_Identifier_List);
         end loop;
         --  after the loop, we have a Featuregroup_Identifier not appended
         --  yet to the list
         Set_Identifier (Port_Reference, Featuregroup_Identifier);
         Set_Featuregroup_Identifier_List (Port_Reference,
         Featuregroup_Identifier_List);
         return Port_Reference;
      else
         Identifier := Node_Inter;
         Set_Identifier (Port_Reference, Identifier);
         return Port_Reference;
      end if;

   end P_Port_Reference;

   ---------------------------------
   -- P_Mode_Transition_Reference --
   ---------------------------------

   --  mode_transition_reference ::=
   --     [ package_reference :: ] mode_identifier

   function P_Mode_Transition_Reference return Node_Id is
      Mode_Transition_Reference  : Node_Id;

      Package_Reference          : Node_Id;
      Mode_Identifier            : Node_Id;
      Identifiers                : List_Id;
   begin
      Mode_Transition_Reference :=
      New_Node (K_Mode_Transition_Reference, Token_Location);

      P_Composed_Referenced_Identifier
      (Identifiers, Mode_Identifier,
       Mode_Transition_Reference,
       PC_Mode_Transition_Reference);

      if No (Mode_Transition_Reference) then
         return No_Node;
      end if;

      Set_Identifier (Mode_Transition_Reference,
      Mode_Identifier);

      if Present (Identifiers) and then not Is_Empty (Identifiers)
      then
         Package_Reference := New_Node (K_Package_Reference, Token_Location);
         Set_Identifiers (Package_Reference, Identifiers);
         Set_Package_Reference
           (Mode_Transition_Reference, Package_Reference);
      end if;

      return Mode_Transition_Reference;
   end P_Mode_Transition_Reference;

   ----------------------------
   -- P_Self_Event_Reference --
   ----------------------------

   --  A revoir : La syntaxe n'est pas claire

   function P_Self_Event_Reference return Node_Id is
   begin
      return No_Node;
   end P_Self_Event_Reference;

   ------------------------
   -- P_Event_Initiation --
   ------------------------

   --  A revoir : la syntaxe n'est pas claire

   --  event_initiation ::=
   --  recover_event_identifier | repair_event_identifier | mode_transition

   function P_Event_Initiation return Node_Id is
      --  Event_Initiation : Node_Id;
   begin
      --  Event_Initiation := New_Node (K_Event_Initiation,
      --  Token_Location);

      --  return Event_Initiation;
      return No_Node;
   end P_Event_Initiation;

   ------------------------
   -- P_Type_Set_Element --
   ------------------------

   --  type_set_element ::=
   --    error_type_or_set_reference | error_type_product

   function P_Type_Set_Element return Node_Id is
      Type_Set_Element : Node_Id := No_Node;

      Error_Type_Or_Set_Reference : Node_Id;
      Error_Type_Product : List_Id;

      Loc_Start, Loc : Location;
      Next : Token_Type;
   begin
      Type_Set_Element := New_Node (K_Type_Set_Element,
      Token_Location);

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Identifier then
         return No_Node;
      end if;

      Save_Lexer (Loc_Start);
      Error_Type_Product := P_Error_Type_Product;
      if Present (Error_Type_Product) then
         Set_Error_Type_Product (Type_Set_Element,
         Error_Type_Product);
      else
         Save_Lexer (Loc);
         --  This instruction means that it's a error_type_product
         --  node and there is a problem in this node
         --  so it can't be the other node
         if Loc_Start /= Loc then
            return No_Node;
         end if;
         Error_Type_Or_Set_Reference := P_Error_Type_Or_Set_Reference;
         if Present (Error_Type_Or_Set_Reference) then
            Set_Error_Type_Or_Set_Reference (Type_Set_Element,
            Error_Type_Or_Set_Reference);
         end if;
      end if;

      return Type_Set_Element;
   end P_Type_Set_Element;

   -----------------------------------
   -- P_Error_Type_Or_Set_Reference --
   -----------------------------------

   --  error_type_or_set_reference ::=
   --    error_type_set_reference | error_type_reference

   --  Set both error_type_set_reference and error_type_reference
   --  and check wich one is true in semantic analysis

   function P_Error_Type_Or_Set_Reference return Node_Id is
      Error_Type_Or_Set_Reference : Node_Id;

      Node : Node_Id;
      Loc : Location;
   begin
      Error_Type_Or_Set_Reference := New_Node (K_Error_Type_Or_Set_Reference,
      Token_Location);

      Save_Lexer (Loc);
      Node := P_Error_Type_Reference;
      if Present (Node) then
         Set_Error_Type_Reference (Error_Type_Or_Set_Reference,
         Node);
      end if;

      Restore_Lexer (Loc);
      Node := P_Error_Type_Set_Reference;
      if Present (Node) then
         Set_Error_Type_Set_Reference (Error_Type_Or_Set_Reference,
         Node);
      end if;

      if No (Node) then
         return No_Node;
      end if;

      return Error_Type_Or_Set_Reference;
   end P_Error_Type_Or_Set_Reference;

   --------------------------
   -- P_Error_Type_Product --
   --------------------------

   --  error_type_product ::=
   --    error_type_reference ( * error_type_reference )+

   --  L'utilisation de error_type_product est toujours accompagné
   --  avec l'utilisation de error_type_reference dans l'annexe EMA
   --  de style : error_type_product | error_type_reference
   --  => je peux adapter la fonction P_Error_Type_Product
   --  avec des messages d'erreurs reliés à error_type_reference

   function P_Error_Type_Product return List_Id is
      Error_Type_Product : List_Id;

      Error_Type_Reference : Node_Id;

      Loc, Loc_Start : Location;
      Next : Token_Type;
   begin
      Error_Type_Product := New_List (K_List_Id, Token_Location);

      Save_Lexer (Loc_Start);

      Error_Type_Reference := P_Error_Type_Reference;
      if Present (Error_Type_Reference) then
         Append_Node_To_List (Error_Type_Reference, Error_Type_Product);
      else
         return No_List;
      end if;

      loop
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next /= T_Star then
            if Length (Error_Type_Product) = 1 then
               Restore_Lexer (Loc_Start);
               return No_List;
            end if;
            exit;
         else
            Scan_Token;
            Error_Type_Reference := P_Error_Type_Reference;
            if No (Error_Type_Reference) then
               return No_List;
            else
               Append_Node_To_List (Error_Type_Reference,
               Error_Type_Product);
            end if;
         end if;
      end loop;

      return Error_Type_Product;
   end P_Error_Type_Product;

   --------------------------
   -- P_Error_Source_State --
   --------------------------

   --  error_source_state ::=
   --   all | ( source_error_state_identifier
   --   [ source_error_type_set ] )

   function P_Error_Source_State return Node_Id is
      Error_Source_State : Node_Id;

      Identifier : Node_Id;
      Source_Error_Type_Set : List_Id;

      Next : Token_Type;
      Loc : Location;
   begin
      Error_Source_State := New_Node (K_Error_Source_State, Token_Location);

      --  'all'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
         return Error_Source_State;
      end if;

      --  source_error_state_identifier
      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;
      Set_Identifier (Error_Source_State, Identifier);

      --  source_error_type_set
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Source_Error_Type_Set := P_Error_Type_Set;
         if Present (Source_Error_Type_Set) then
            Set_Source_Error_Type_Set (Error_Source_State,
            Source_Error_Type_Set);
         else
            return No_Node;
         end if;
      end if;

      return Error_Source_State;
   end P_Error_Source_State;

   -------------------------------
   -- P_Error_Transition_Target --
   -------------------------------

   --  error_transition_target ::=
   --   ( target_error_state_identifier [ target_error_type_instance ] )
   --   | same state

   function P_Error_Transition_Target return Node_Id is
      Error_Transition_Target : Node_Id;

      Identifier : Node_Id;
      Target_Error_Type_Instance : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_Transition_Target := New_Node (K_Error_Transition_Target,
      Token_Location);

      --  'same'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Same then
         Scan_Token;
         --  'state'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_State then
            Scan_Token;
            return Error_Transition_Target;
         else
            Scan_Token;
            DPE (PC_Error_Transition_Target,
            Expected_Token => T_State);
            return No_Node;
         end if;
      end if;

      --  target_error_state_identifier
      Identifier := P_Identifier;
      if No (Identifier) then
         Scan_Token;
         DPE (PC_Error_Transition_Target,
         Expected_Tokens => (T_Same,
                             T_Identifier));
         return No_Node;
      end if;
      Set_Identifier (Error_Transition_Target, Identifier);

      --  target_error_type_instance
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Target_Error_Type_Instance := P_Target_Error_Type_Instance;
         if Present (Target_Error_Type_Instance) then
            Set_Target_Error_Type_Instance (Error_Transition_Target,
            Target_Error_Type_Instance);
         else
            return No_Node;
         end if;
      end if;

      return Error_Transition_Target;
   end P_Error_Transition_Target;

   -------------------------------
   -- P_Error_Transition_Branch --
   -------------------------------

   --  error_transition_branch ::=
   --   ( error_transition_target with branch_probability
   --   { , error_transition_target with branch_probability }* )

   function P_Error_Transition_Branch return Node_Id is
      Error_Transition_Branch : Node_Id;

      Error_Transition_Target_List : List_Id;
      Error_Transition_Target : Node_Id;
      Branch_Probability_List : List_Id;
      Branch_Probability : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_Transition_Branch := New_Node (K_Error_Transition_Branch,
      Token_Location);

      --  '('
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Transition_Branch,
         Expected_Token => T_Left_Paren);
         return No_Node;
      end if;

      --  declaring both lists
      Error_Transition_Target_List := New_List (K_List_Id, Token_Location);
      Set_Error_Transition_Target_List (Error_Transition_Branch,
      Error_Transition_Target_List);

      Branch_Probability_List := New_List (K_List_Id, Token_Location);
      Set_Branch_Probability_List (Error_Transition_Branch,
      Branch_Probability_List);

      --  error_transition_target with branch_probability
      --  { , error_transition_target with branch_probability }*
      loop
         --  error_transition_targe
         Error_Transition_Target := P_Error_Transition_Target;
         if No (Error_Transition_Target) then
            return No_Node;
         end if;
         Append_Node_To_List (Error_Transition_Target,
         Error_Transition_Target_List);

         --  with
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next /= T_With then
            Scan_Token;
            DPE (PC_Error_Transition_Branch,
            Expected_Token => T_With);
            return No_Node;
         else
            Scan_Token;
         end if;

         --  'branch_probability'
         Branch_Probability := P_Branch_Probability;
         if No (Branch_Probability) then
            return No_Node;
         end if;
         Append_Node_To_List (Error_Transition_Target,
         Branch_Probability_List);

         --  ','
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         exit when Next /= T_Comma;
         Scan_Token;
      end loop;

      --  ')'
      Save_Lexer (Loc);
      if Next_Token /= T_Right_Paren then
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Error_Transition_Branch,
         Expected_Token => T_Right_Paren);
         return No_Node;
      else
         Restore_Lexer (Loc);
         Scan_Token;
      end if;

      return Error_Transition_Branch;
   end P_Error_Transition_Branch;

   -----------------------
   -- P_Error_Condition --
   -----------------------

   --  error_condition ::=
   --  error_condition_trigger
   --  | ( error_condition )
   --  | error_condition and error_condition
   --  | error_condition or error_condition
   --  | numeric_literal ormore
   --  ( error_condition_trigger { , error_condition_trigger } + )
   --  | numeric_literal orless
   --  ( error_condition_trigger { , error_condition_trigger } + )

   --  A revoir : lors du traîtement sémantique de
   --  error_condition_1 (and | or ) error_condition_2
   --  error_condition_1 sera un sous noeud de error_condition_2
   --  => Le même cas de Pointcut_Expression dans ao4aadl
   --  ce n'est pas le cas pour error_condition_trigger

   function P_Error_Condition return Node_Id is
      use Ocarina.ME_AADL_EMA;

      Error_Condition : Node_Id;

      Error_Condition_Node : Node_Id := No_Node;
      Numeric_Literal : Node_Id;
      Error_Condition_Trigger_List : List_Id;
      Error_Condition_Trigger : Node_Id;
      Operator_Node : Node_Id := No_Node;
      Operat_Kind   : Ocarina.ME_AADL_EMA.Operator_Kind;

      Loc, Loc_Start, Loc_End : Location;
      Next : Token_Type;
      Escape : Boolean := False;
   begin
      Error_Condition := New_Node
        (K_Error_Condition, Token_Location);

      --  Error_Condition_Trigger

      Error_Condition_Trigger := P_Error_Condition_Trigger;
      if Present (Error_Condition_Trigger) then
         Set_Error_Condition_Trigger
           (Error_Condition, Error_Condition_Trigger);
         return Error_Condition;
      end if;

      --  ( Error_Condition_Node )

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
         Save_Lexer (Loc_Start);
         Error_Condition_Node := P_Error_Condition;
         Save_Lexer (Loc_End);
         Set_Error_Condition_Node
           (Error_Condition, Error_Condition_Node);

         if No (Error_Condition_Node) then
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Error_Condition,
                    PC_Error_Condition,
                    PC_Error_Condition_Trigger,
                    Expected_Tokens =>
                      (T_Left_Paren, T_Numeric_Literal));
            end if;
            return No_Node;

         else
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next /= T_Right_Paren then
               Scan_Token;
               DPE (PC_Error_Condition,
                    Expected_Token => T_Right_Paren);
               return No_Node;
            else
               Scan_Token;
               return Error_Condition;
            end if;
         end if;
      end if;

      --  error_condition and error_condition
      --  error_condition or error_condition

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_And =>
            Scan_Token;
            Operat_Kind := OK_And;
         when T_Or =>
            Scan_Token;
            Operat_Kind := OK_Or;
         when others =>
            Escape := True;
      end case;

      if Escape = False then
         Operator_Node :=  New_Node (K_Operator, Token_Location);
         Set_Operator_Kind (Operator_Node,
                            Ocarina.ME_AADL_EMA.Operator_Kind'Pos
                            (Operat_Kind));
         Set_Operator (Error_Condition, Operator_Node);

         Error_Condition_Node := P_Error_Condition;
         if No (Error_Condition_Node) then
            Scan_Token;
            DPE (PC_Error_Condition, PC_Error_Condition,
            PC_Error_Condition_Trigger, Expected_Tokens =>
            (T_Left_Paren, T_Numeric_Literal));
            return No_Node;
         else
            Set_Error_Condition_Node (Error_Condition, Error_Condition_Node);
            return Error_Condition;
         end if;
      end if;

      --  numeric_literal ormore
      --  ( error_condition_trigger { , error_condition_trigger } + )
      --  numeric_literal orless
      --  ( error_condition_trigger { , error_condition_trigger } + )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_Real_Literal =>
            Scan_Token;
            Numeric_Literal := New_Node (K_Literal, Token_Location);
            Set_Value (Numeric_Literal,
                       New_Real_Value (Real_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));

         when T_Integer_Literal =>
            Scan_Token;
            Numeric_Literal := New_Node (K_Literal, Token_Location);
            Set_Value (Numeric_Literal,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));
         when others =>
            return No_Node;
      end case;
      Set_Numeric_Literal (Error_Condition, Numeric_Literal);

      --  orless | ormore
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_OrLess =>
            Scan_Token;
            Operat_Kind := OK_OrLess;
         when T_OrMore =>
            Scan_Token;
            Operat_Kind := OK_OrMore;
         when others =>
            Scan_Token;
            DPE (PC_Operator, EMC_Operator_Unknown, "");
            return No_Node;
      end case;

      Operator_Node :=  New_Node (K_Operator, Token_Location);
      Set_Operator_Kind (Operator_Node,
                         Ocarina.ME_AADL_EMA.Operator_Kind'Pos
                         (Operat_Kind));
      Set_Operator (Error_Condition, Operator_Node);

      --  '('
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
         --  error_condition_trigger
         Save_Lexer (Loc_Start);
         Error_Condition_Trigger := P_Error_Condition_Trigger;
         Save_Lexer (Loc_End);
         if Present (Error_Condition_Trigger) then
            Error_Condition_Trigger_List :=
              New_List (K_List_Id,
                        Token_Location);
            Set_Error_Condition_Trigger_List
              (Error_Condition,
               Error_Condition_Trigger_List);
            Append_Node_To_List
            (Error_Condition_Trigger, Error_Condition_Trigger_List);

            --  { , error_condition_trigger } +
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);

            --  If there is no additional error_condition_trigger,
            --  then return immediately, else continue.

            if Next /= T_Comma then
               Scan_Token;
               return Error_Condition_Trigger;
            else
               Scan_Token;
            end if;

            loop
               Save_Lexer (Loc_Start);
               Error_Condition_Trigger := P_Error_Condition_Trigger;
               Save_Lexer (Loc_End);
               if No (Error_Condition_Trigger) then
                  if Loc_Start = Loc_End then
                     DPE (PC_Error_Condition, PC_Error_Condition_Trigger);
                  end if;
                  return No_Node;
               end if;

               Append_Node_To_List
               (Error_Condition_Trigger, Error_Condition_Trigger_List);

               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               --  Error_Condition_Trigger_List must contain at
               --  least 3 elements
               if Next /= T_Comma and then
               Length (Error_Condition_Trigger_List) = 2
               then
                  Scan_Token;
                  DPE (PC_Error_Condition,
                  Expected_Token => T_Comma);
                  return No_Node;
               end if;
               exit when Next /= T_Comma;
               Scan_Token; -- ','
            end loop;

            --  ')'
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Right_Paren then
               Scan_Token;
            else
               Scan_Token;
               DPE (PC_Error_Condition,
                    Expected_Token => T_Right_Paren);
               return No_Node;
            end if;

         else
            if Loc_Start = Loc_End then
               DPE (PC_Error_Condition, PC_Error_Condition_Trigger);
            end if;
            return No_Node;
         end if;

      else
         Scan_Token;
         DPE (PC_Error_Condition,
              Expected_Token => T_Left_Paren);
         return No_Node;
      end if;

      return Error_Condition;
   end P_Error_Condition;

   --------------------------
   -- P_Branch_Probability --
   --------------------------

   --  branch_probability ::=
   --    fixed_probability_value | others

   function P_Branch_Probability return Node_Id is
      Branch_Probability : Node_Id;

      Fixed_Probability_Value : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Branch_Probability := New_Node (K_Branch_Probability, Token_Location);

      --  'others'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Others then
         Scan_Token;
         return Branch_Probability;
      end if;

      --  Fixed_Probability_Value
      Fixed_Probability_Value := P_Fixed_Probability_Value;
      Set_Fixed_Probability_Value (Branch_Probability,
      Fixed_Probability_Value);
      if No (Fixed_Probability_Value) then
         Scan_Token;
         DPE (PC_Branch_Probability, PC_Fixed_Probability_Value,
         Expected_Token => T_Others);
         return No_Node;
      end if;

      return Branch_Probability;
   end P_Branch_Probability;

   -------------------------------
   -- P_Fixed_Probability_Value --
   -------------------------------

   --  fixed_probability_value ::=
   --    real_literal |
   --    ( [ property_set_identifier :: ] real_property_identifier )

   function P_Fixed_Probability_Value return Node_Id is
      Fixed_Probability_Value : Node_Id;

      Property_Set_Identifier : Node_Id;
      Real_Property_Identifier : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      --  real_literal
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Real_Literal then
         Scan_Token;
         Fixed_Probability_Value := New_Node (K_Literal, Token_Location);
         Set_Value (Fixed_Probability_Value,
                    New_Real_Value (Real_Literal_Value,
                                    False,
                                    Numeric_Literal_Base,
                                    Numeric_Literal_Exp));
         return Fixed_Probability_Value;
      end if;

      --  [ property_set_identifier :: ] real_property_identifier
      Fixed_Probability_Value := New_Node (K_Fixed_Probability_Value,
      Token_Location);

      P_Composed_Identifier
      (Property_Set_Identifier, Real_Property_Identifier,
       Fixed_Probability_Value,
       PC_Fixed_Probability_Value);

      if No (Fixed_Probability_Value) then
         return No_Node;
      end if;

      Set_Identifier (Fixed_Probability_Value,
      Real_Property_Identifier);

      if Present (Property_Set_Identifier) then
         Set_Property_Set_Identifier (Fixed_Probability_Value,
         Property_Set_Identifier);
      end if;

      return Fixed_Probability_Value;
   end P_Fixed_Probability_Value;

   -------------------------------
   -- P_Error_Condition_Trigger --
   -------------------------------

   --  error_condition_trigger ::=
   --   error_behavior_event_identifier [error_type_set ]
   --   | [ in ] incoming_error_propagation_point [ error_type_set_or_noerror ]
   --   | subcomponent_identifier .  outgoing_error_propagation_point
   --     [ error_type_set_or_noerror ]

   function P_Error_Condition_Trigger return Node_Id is
      Error_Condition_Trigger : Node_Id := No_Node;

      Error_Type_Set : List_Id;
      Incoming_Error_Propagation_Point : Node_Id;
      Error_Type_Set_Or_Noerror : Node_Id;
      Identifier : Node_Id;
      Outgoing_Error_Propagation_Point : Node_Id;

      Next : Token_Type;
      Loc_Begin, Loc_Start, Loc_End, Loc : Location;
      Exist_First, Exist_Second, Exist_Third : Boolean := False;
      Not_Second_Node : Boolean := False;
      Exist_In, Wait : Boolean := False;
   begin
      Error_Condition_Trigger := New_Node (K_Error_Condition_Trigger,
      Token_Location);

      Save_Lexer (Loc_Begin);

      --  error_behavior_event_identifier [error_type_set ]
      --  subcomponent_identifier .  outgoing_error_propagation_point
      --  [ error_type_set_or_noerror ]
      Identifier := P_Identifier;
      if Present (Identifier) then
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Dot then
            Scan_Token;
            Save_Lexer (Loc_Start);
            Outgoing_Error_Propagation_Point :=
            P_Error_Propagation_Point;
            Save_Lexer (Loc_End);
            if Token /= T_Identifier then
               Not_Second_Node := True;
            end if;
            if Present (Outgoing_Error_Propagation_Point) then
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               if Next = T_Left_Brace then
                  Scan_Token;
                  Save_Lexer (Loc);
                  Next := Next_Token;
                  Restore_Lexer (Loc);
                  if Not_Second_Node then
                     Save_Lexer (Loc_Start);
                     Error_Type_Set_Or_Noerror :=
                     P_Error_Type_Set_Or_Noerror;
                     Save_Lexer (Loc_End);
                     if Present (Error_Type_Set_Or_Noerror) then
                        Exist_Third := True;
                     elsif Loc_Start /= Loc_End then
                        Wait := True;
                     end if;
                  elsif Next = T_Identifier or else Next = T_NoError
                  then
                     Save_Lexer (Loc_Start);
                     Error_Type_Set_Or_Noerror :=
                     P_Error_Type_Set_Or_Noerror;
                     Save_Lexer (Loc_End);
                     if Present (Error_Type_Set_Or_Noerror) then
                        Exist_Third := True;
                     elsif Loc_Start /= Loc_End then
                        Wait := True;
                     end if;
                  end if;
               else
                  Exist_Third := True;
               end if;
            elsif Loc_Start /= Loc_End then
               Wait := True;
            elsif Loc_Start = Loc_End then
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               Scan_Token;
               DPE (PC_Error_Condition_Trigger);
               return No_Node;
            end if;
         elsif Next = T_Left_Brace then
            Scan_Token;
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Identifier then
               Save_Lexer (Loc_Start);
               Error_Type_Set := P_Error_Type_Set;
               Save_Lexer (Loc_End);
               if Present (Error_Type_Set) then
                  Exist_First := True;
               elsif Loc_Start /= Loc_End then
                  Wait := True;
               end if;
            end if;
         else
            Exist_First := True;
         end if;
      end if;

      if Exist_First then
         Set_Identifier (Error_Condition_Trigger, Identifier);
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Error_Condition_Trigger,
            Error_Type_Set);
         end if;
      elsif Exist_Third then
         Set_Identifier (Error_Condition_Trigger, Identifier);
         Set_Outgoing_Error_Propagation_Point
         (Error_Condition_Trigger, Outgoing_Error_Propagation_Point);
         if Present (Error_Type_Set_Or_Noerror) then
            Set_Error_Type_Set_Or_Noerror (Error_Condition_Trigger,
            Error_Type_Set_Or_Noerror);
         end if;
      end if;

      --  [ in ] incoming_error_propagation_point [ error_type_set_or_noerror ]
      if Wait = False and then Not_Second_Node = False
      then
         Restore_Lexer (Loc_Begin);
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_In then
            Scan_Token;
            Exist_In := True;
         end if;
         Save_Lexer (Loc_Start);
         Incoming_Error_Propagation_Point :=
         P_Error_Propagation_Point;
         Save_Lexer (Loc_End);
         if Present (Incoming_Error_Propagation_Point) then
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Left_Brace then
               Scan_Token;
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               if Next = T_Identifier or else Next = T_NoError
               then
                  Save_Lexer (Loc_Start);
                  Error_Type_Set_Or_Noerror :=
                  P_Error_Type_Set_Or_Noerror;
                  Save_Lexer (Loc_End);
                  if Present (Error_Type_Set_Or_Noerror) then
                     Exist_Second := True;
                  end if;
               else
                  Scan_Token;
                  DPE (PC_Error_Condition_Trigger);
               end if;
            else
               Exist_Second := True;
            end if;
         elsif Exist_In then
            DPE (PC_Error_Condition_Trigger,
            PC_Incoming_Error_Propagation_Point);
         end if;

         if Exist_Second then
            Set_Incoming_Error_Propagation_Point
            (Error_Condition_Trigger, Incoming_Error_Propagation_Point);
            if Present (Error_Type_Set_Or_Noerror) then
               Set_Error_Type_Set_Or_Noerror (Error_Condition_Trigger,
               Error_Type_Set_Or_Noerror);
            end if;
         end if;
      end if;

      if Exist_First or else Exist_Second or else Exist_Third
      then
         return Error_Condition_Trigger;
      else
         return No_Node;
      end if;

   end P_Error_Condition_Trigger;

   -------------------------------
   -- P_Error_Propagation_Point --
   -------------------------------

   --  error_propagation_point ::=
   --  feature_reference | binding_reference
   --  | propagation_point_identifier

   --  "feature_reference" and "propagation_point_identifier"
   --  can both be identifiers
   --  so i have to Set_ both nodes and remove unused node
   --  in the semantic part

   function P_Error_Propagation_Point return Node_Id is
      Error_Propagation_Point : Node_Id;

      Feature_Reference : Node_Id;
      Binding_Reference : Node_Id;
      Propagation_Point_Identifier : Node_Id;

      Loc_Start : Location;
      Escape : Boolean := True;
   begin
      Error_Propagation_Point := New_Node (K_Error_Propagation_Point,
      Token_Location);

      Save_Lexer (Loc_Start);
      Propagation_Point_Identifier := P_Identifier;
      if Present (Propagation_Point_Identifier) then
         Set_Identifier (Error_Propagation_Point,
         Propagation_Point_Identifier);
         Escape := False;
      end if;

      if Escape = False then
         Restore_Lexer (Loc_Start);
      end if;
      Feature_Reference := P_Feature_Reference;
      if Present (Feature_Reference) then
         Set_Feature_Reference (Error_Propagation_Point,
         Feature_Reference);
         return Error_Propagation_Point;
      end if;

      Binding_Reference := P_Binding_Reference;
      if Present (Binding_Reference) then
         Set_Binding_Reference (Error_Propagation_Point,
         Binding_Reference);
      else
         return No_Node;
      end if;

      return Error_Propagation_Point;
   end P_Error_Propagation_Point;

   -------------------------
   -- P_Feature_Reference --
   -------------------------

   --  feature_reference ::=
   --  ( { feature_group_identifier . } *
   --  feature_identifier )
   --  | access

   function P_Feature_Reference return Node_Id is
      Feature_Reference : Node_Id;

      Featuregroup_Identifier_List : List_Id := No_List;
      Featuregroup_Identifier : Node_Id;
      Identifier : Node_Id := No_Node;
      Node_Inter : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Feature_Reference := New_Node (K_Feature_Reference,
      Token_Location);

      --  'access'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Access then
         Scan_Token;
         return Feature_Reference;
      end if;

      Node_Inter := P_Identifier;
      if No (Node_Inter) then
         return No_Node;
      end if;

      --  '.'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Dot then
         Scan_Token;
         Featuregroup_Identifier_List := New_List (K_List_Id,
         Token_Location);
         Append_Node_To_List (Node_Inter,
         Featuregroup_Identifier_List);
         loop
            Featuregroup_Identifier := P_Identifier;
            if No (Featuregroup_Identifier) then
               Scan_Token;
               DPE (PC_Feature_Reference,
               Expected_Token => T_Identifier);
               return No_Node;
            end if;

            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            exit when Next /= T_Dot;
            Scan_Token;

            --  if there is no dot after the identifier then it is
            --  feature_identifier
            --  => we dont do append unless we find a dot
            Append_Node_To_List (Featuregroup_Identifier,
            Featuregroup_Identifier_List);
         end loop;
         --  after the loop, we have a Featuregroup_Identifier not appended
         --  yet to the list
         Set_Identifier (Feature_Reference, Featuregroup_Identifier);
         Set_Featuregroup_Identifier_List (Feature_Reference,
         Featuregroup_Identifier_List);
         return Feature_Reference;
      else
         Identifier := Node_Inter;
         Set_Identifier (Feature_Reference, Identifier);
         return Feature_Reference;
      end if;

   end P_Feature_Reference;

   -------------------------
   -- P_Binding_Reference --
   -------------------------

   --  binding_reference ::=
   --  processor | memory | connection | binding | bindings

   function P_Binding_Reference return Node_Id is
      use Ocarina.ME_AADL_EMA;

      Binding_Reference : Node_Id;
      Binding_Kind   : Ocarina.ME_AADL_EMA.Binding_Kind;

      Next : Token_Type;
      Loc : Location;
   begin
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_Processor =>
            Scan_Token;
            Binding_Kind := BK_Processor;
         when T_Memory =>
            Scan_Token;
            Binding_Kind := BK_Memory;
         when T_Connection =>
            Scan_Token;
            Binding_Kind := BK_Connection;
         when T_Binding =>
            Scan_Token;
            Binding_Kind := BK_Binding;
         when T_Bindings =>
            Scan_Token;
            Binding_Kind := BK_Bindings;
         when others =>
            return No_Node;
      end case;

      Binding_Reference := New_Node (K_Binding_Reference,
      Token_Location);
      Set_Binding_Kind (Binding_Reference,
                        Ocarina.ME_AADL_EMA.Binding_Kind'Pos
                        (Binding_Kind));

      return Binding_Reference;
   end P_Binding_Reference;

   -----------------
   -- P_Subclause --
   -----------------

   --  EMA_Annex ::= {** Annex_Subclause **}

   function P_Subclause return Node_Id
   is
      EMA_Annex : Node_Id := No_Node;
   begin
      if Next_Token /= T_EOF then
         EMA_Annex := P_Annex_Subclause;
      end if;

      return EMA_Annex;
   end P_Subclause;

   -----------------------
   -- P_Annex_Subclause --
   -----------------------

   --  Annex_Subclause ::=
   --   [ use types error_type_library_list ; ]
   --   [ use type equivalence error_type_mappings_reference ; ]
   --   [ use mappings error_type_mappings_reference ; ]
   --   [ use behavior error_behavior_state_machine_reference ; ]
   --   [ error_propagations ]
   --   [ component_error_behavior ]
   --   [ composite_error_behavior ]
   --   [ connection_error_behavior ]
   --   [ propagation_paths ]
   --   [ EMV2_properties_section ]

   function P_Annex_Subclause return Node_Id is
      Annex_Subclause : Node_Id;

      Error_Type_Library_List : List_Id;
      Error_Type_Mappings_Reference_Equivalence : Node_Id;
      Error_Type_Mappings_Reference_Mappings : Node_Id;
      Error_Behavior_State_Machine_Reference : Node_Id;
      Error_Propagations : Node_Id;
      Component_Error_Behavior : Node_Id;
      Composite_Error_Behavior : List_Id;
      Connection_Error_Behavior : Node_Id;
      Propagation_Paths : Node_Id;
      EMV2_Properties_Section : List_Id;

      Next : Token_Type;
      Loc_Before, Loc_After, Loc, Loc_Start : Location;
   begin
      Annex_Subclause := New_Node
      (K_Annex_Subclause, Token_Location);

      --  'use'
      Save_Lexer (Loc_Start);
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         --  'types'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Types then
            Scan_Token;
            Error_Type_Library_List := P_Error_Type_Library_List_Used;
            if No (Error_Type_Library_List) then
               return No_Node;
            else
               Set_Error_Type_Library_List (Annex_Subclause,
               Error_Type_Library_List);
            end if;
         else
            if Next /= T_Type and then Next /= T_Mappings
            and then Next /= T_Behavior
            then
               Scan_Token;
               DPE (PC_Error_Model_Component_Constructs,
               Expected_Tokens => (T_Types, T_Type, T_Mappings, T_Behavior));
               return No_Node;
            end if;
            Restore_Lexer (Loc_Start);
         end if;
      end if;
      --  'use'
      Save_Lexer (Loc_Start);
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         --  'type'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Type then
            Scan_Token;
            --  'equivalence'
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Equivalence then
               Scan_Token;
               Error_Type_Mappings_Reference_Equivalence :=
               P_Error_Type_Mappings_Reference;
               if No (Error_Type_Mappings_Reference_Equivalence) then
                  return No_Node;
               else
                  Set_Error_Type_Mappings_Reference_Equivalence
                  (Annex_Subclause,
                  Error_Type_Mappings_Reference_Equivalence);
               end if;
            else
               Scan_Token;
               DPE (PC_Error_Model_Component_Constructs,
               Expected_Token => T_Equivalence);
               return No_Node;
            end if;
         else
            if Next /= T_Mappings and then Next /= T_Behavior
            then
               Scan_Token;
               DPE (PC_Error_Model_Component_Constructs,
               Expected_Tokens => (T_Type, T_Mappings, T_Behavior));
               return No_Node;
            end if;
            Restore_Lexer (Loc_Start);
         end if;
      end if;

      --  'use'
      Save_Lexer (Loc_Start);
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         --  'mappings'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Mappings then
            Scan_Token;
            Error_Type_Mappings_Reference_Mappings :=
            P_Error_Type_Mappings_Reference;
            if No (Error_Type_Mappings_Reference_Mappings) then
               return No_Node;
            else
               Set_Error_Type_Mappings_Reference_Mappings
               (Annex_Subclause,
               Error_Type_Mappings_Reference_Mappings);
            end if;
         else
            if Next /= T_Behavior then
               Scan_Token;
               DPE (PC_Error_Model_Component_Constructs,
               Expected_Tokens => (T_Mappings, T_Behavior));
               return No_Node;
            end if;
            Restore_Lexer (Loc_Start);
         end if;
      end if;

      --  'use'
      Save_Lexer (Loc_Start);
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         --  'behavior'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Behavior then
            Scan_Token;
            Error_Behavior_State_Machine_Reference :=
            P_Error_Behavior_State_Machine_Reference;
            if No (Error_Behavior_State_Machine_Reference) then
               return No_Node;
            else
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               if Next = T_Semi_Colon then
                  Scan_Token;
                  Set_Error_Behavior_State_Machine_Reference
                  (Annex_Subclause,
                  Error_Behavior_State_Machine_Reference);
               else
                  Scan_Token;
                  DPE (PC_Error_Model_Component_Constructs,
                       Expected_Token => T_Semi_Colon);
                  return No_Node;
               end if;
            end if;
         else
            Scan_Token;
            DPE (PC_Error_Model_Component_Constructs,
            Expected_Token => T_Behavior);
            return No_Node;
         end if;
      end if;

      --  error_propagations
      Save_Lexer (Loc_Before);
      Error_Propagations := P_Error_Propagations;
      Save_Lexer (Loc_After);
      if Present (Error_Propagations) then
         Set_Error_Propagations (Annex_Subclause,
         Error_Propagations);
      elsif Loc_Before /= Loc_After then
         return No_Node;
      end if;

      --  component_error_behavior
      Save_Lexer (Loc_Before);
      Component_Error_Behavior := P_Component_Error_Behavior;
      Save_Lexer (Loc_After);
      if Present (Component_Error_Behavior) then
         Set_Component_Error_Behavior (Annex_Subclause,
         Component_Error_Behavior);
      elsif Loc_Before /= Loc_After then
         return No_Node;
      end if;

      --  composite_error_behavior
      Save_Lexer (Loc_Before);
      Composite_Error_Behavior := P_Composite_Error_Behavior;
      Save_Lexer (Loc_After);
      if Present (Composite_Error_Behavior) then
         Set_Composite_Error_Behavior (Annex_Subclause,
         Composite_Error_Behavior);
      elsif Loc_Before /= Loc_After then
         return No_Node;
      end if;

      --  connection_error_behavior
      Save_Lexer (Loc_Before);
      Connection_Error_Behavior := P_Connection_Error_Behavior;
      Save_Lexer (Loc_After);
      if Present (Connection_Error_Behavior) then
         Set_Connection_Error_Behavior (Annex_Subclause,
         Connection_Error_Behavior);
      elsif Loc_Before /= Loc_After then
         return No_Node;
      end if;

      --  propagation_paths
      Save_Lexer (Loc_Before);
      Propagation_Paths := P_Propagation_Paths;
      Save_Lexer (Loc_After);
      if Present (Propagation_Paths) then
         Set_Propagation_Paths (Annex_Subclause,
         Propagation_Paths);
      elsif Loc_Before /= Loc_After then
         return No_Node;
      end if;

      --  EMV2_properties_section
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Properties then
         Scan_Token;
         Save_Lexer (Loc_Before);
         EMV2_Properties_Section := P_EMV2_Properties_Section;
         Save_Lexer (Loc_After);
         if Present (EMV2_Properties_Section) then
            Set_EMV2_Properties_Section (Annex_Subclause,
            EMV2_Properties_Section);
         elsif Loc_Before /= Loc_After then
            return No_Node;
         end if;
      end if;

      Scan_Token;
      if Token /= T_EOF then
         DPE (PC_Error_Model_Component_Constructs);
         return No_Node;
      end if;

      return Annex_Subclause;
   end P_Annex_Subclause;

   -------------------------------------
   -- P_Error_Type_Mappings_Reference --
   -------------------------------------

   --  error_type_mappings_reference ::=
   --   [ package_reference :: ] type_mappings_identifier

   --  The only contexts we are using this function is that it ends
   --  with ';'

   function P_Error_Type_Mappings_Reference return Node_Id is
      Error_Type_Mappings_Reference : Node_Id;

      Package_Reference : Node_Id;
      Type_Mappings_Identifier : Node_Id;
      Identifiers : List_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_Type_Mappings_Reference :=
      New_Node (K_Error_Type_Mappings_Reference, Token_Location);

      P_Composed_Referenced_Identifier
      (Identifiers, Type_Mappings_Identifier,
       Error_Type_Mappings_Reference,
       PC_Error_Type_Mappings_Reference);

      if No (Error_Type_Mappings_Reference) then
         return No_Node;
      end if;

      Set_Identifier (Error_Type_Mappings_Reference,
      Type_Mappings_Identifier);

      if Present (Identifiers) and then not Is_Empty (Identifiers)
      then
         Package_Reference := New_Node (K_Package_Reference, Token_Location);
         Set_Identifiers (Package_Reference, Identifiers);
         Set_Package_Reference
           (Error_Type_Mappings_Reference, Package_Reference);
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Semi_Colon then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Type_Mappings_Reference,
         Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Error_Type_Mappings_Reference;
   end P_Error_Type_Mappings_Reference;

   ----------------------------------------------
   -- P_Error_Behavior_State_Machine_Reference --
   ----------------------------------------------

   --   error_behavior_state_machine_reference ::=
   --    [error_model_library_reference :: ]
   --    error_behavior_state_machine_identifier

   function P_Error_Behavior_State_Machine_Reference return Node_Id is
      Error_Behavior_State_Machine_Reference : Node_Id;

      Error_Model_Library_Reference : Node_Id;
      Error_Behavior_State_Mchine_Id : Node_Id;
      Identifiers : List_Id;
   begin
      Error_Behavior_State_Machine_Reference :=
      New_Node (K_Error_Behavior_State_Machine_Reference, Token_Location);

      P_Composed_Referenced_Identifier
      (Identifiers, Error_Behavior_State_Mchine_Id,
      Error_Behavior_State_Machine_Reference,
      PC_Error_Behavior_State_Machine_Reference);

      if No (Error_Behavior_State_Machine_Reference) then
         return No_Node;
      end if;

      Set_Identifier (Error_Behavior_State_Machine_Reference,
      Error_Behavior_State_Mchine_Id);

      if Present (Identifiers) and then not Is_Empty (Identifiers)
      then
         Error_Model_Library_Reference := New_Node
            (K_Error_Model_Library_Reference, Token_Location);
         Set_Identifiers (Error_Model_Library_Reference, Identifiers);
         Set_Error_Model_Library_Reference
           (Error_Behavior_State_Machine_Reference,
            Error_Model_Library_Reference);
      end if;

      return Error_Behavior_State_Machine_Reference;
   end P_Error_Behavior_State_Machine_Reference;

   --------------------------
   -- P_Error_Propagations --
   --------------------------

   --  error_propagations ::=
   --   error propagations
   --     { error_propagation | error_containment } *
   --     [ flows { error_flow } + ]
   --   end propagations;

   function P_Error_Propagations return Node_Id is
      Error_Propagations : Node_Id;

      Error_Propagation_List : List_Id;
      Error_Containment_List : List_Id;
      Node : Node_Id;
      Error_Flow_List : List_Id;
      Error_Flow : Node_Id;

      Loc_Before, Loc_After, Loc : Location;
      Next : Token_Type;
      Identifier : Node_Id;
      --  This variable is used to clarify the function
      --  specifically : [ flows { error_flow } + ]
      Escape : Boolean := True;
   begin
      Error_Propagations := New_Node (K_Error_Propagations,
      Token_Location);

      --  'error'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_R_Error then
         Scan_Token;
      else
         return No_Node;
      end if;

      --  'propagations'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Propagations then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Propagations,
         Expected_Token => T_Propagations);
         return No_Node;
      end if;

      --  { error_propagation | error_containment } *
      Error_Propagation_List := New_List (K_List_Id,
      Token_Location);
      Error_Containment_List := New_List (K_List_Id,
      Token_Location);
      loop
         Save_Lexer (Loc_Before);
         Node := P_Error_Propagation_Or_Containment;
         Save_Lexer (Loc_After);
         if No (Node) then
            if Loc_Before /= Loc_After then
               return No_Node;
            else
               exit;
            end if;
         else
            if Kind (Node) = K_Error_Containment then
               Append_Node_To_List (Node, Error_Containment_List);
            elsif Kind (Node) = K_Error_Propagation then
               Append_Node_To_List (Node, Error_Propagation_List);
            end if;
         end if;
      end loop;
      if Length (Error_Propagation_List) /= 0 then
         Set_Error_Propagation_List (Error_Propagations,
         Error_Propagation_List);
      end if;
      if Length (Error_Containment_List) /= 0 then
         Set_Error_Containment_List (Error_Propagations,
         Error_Containment_List);
      end if;

      --   [ flows { error_flow } + ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Flows then
         Scan_Token;
         Escape := False;
      end if;

      if Escape = False then
         Error_Flow_List := New_List (K_List_Id,
         Token_Location);
         Save_Lexer (Loc);
         Identifier := P_Identifier;
         if No (Identifier) then
            Scan_Token;
            DPE (PC_Error_Flow,
            Expected_Token => T_Identifier);
            return No_Node;
         else
            Restore_Lexer (Loc);
         end if;
         loop
            Save_Lexer (Loc_Before);
            Error_Flow := P_Error_Flow;
            Save_Lexer (Loc_After);
            if Loc_Before /= Loc_After and then Error_Flow = No_Node
            then
               return No_Node;
            end if;
            exit when Error_Flow = No_Node;
            Append_Node_To_List (Error_Flow,
            Error_Flow_List);
         end loop;
         if Length (Error_Flow_List) = 0 then
            return No_Node;
         else
            Set_Error_Flow_List (Error_Propagations,
            Error_Flow_List);
         end if;
      end if;

      --  'end'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_End then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Propagations,
         Expected_Token => T_End);
         return No_Node;
      end if;

      --  'propagations'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Propagations then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Propagations,
         Expected_Token => T_Propagations);
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Semi_Colon then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Propagations,
         Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Error_Propagations;
   end P_Error_Propagations;

   ----------------------------------------
   -- P_Error_Propagation_Or_Containment --
   ----------------------------------------

   --  error_propagation ::=
   --    error_propagation_point :
   --    ( in | out ) propagation error_type_set ;

   --  error_containment ::=
   --    error_propagation_point :
   --    not ( in | out ) propagation error_type_set ;

   --  This function treats both nodes : error_propagation
   --  and error_containment

   function P_Error_Propagation_Or_Containment return Node_Id is
      use Ocarina.ME_AADL_EMA;

      Error_Node : Node_Id;

      Error_Propagation_Point : Node_Id;
      Error_Type_Set : List_Id;
      Propagation : Node_Id := No_Node;
      Propagation_Kind   : Ocarina.ME_AADL_EMA.Propagation_Kind;

      Loc : Location;
      Next : Token_Type;
   begin

      --  error_propagation_point
      Error_Propagation_Point := P_Error_Propagation_Point;
      if No (Error_Propagation_Point) then
         return No_Node;
      end if;

      --  ':'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Colon then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Propagations, Expected_Token => T_Colon);
         return No_Node;
      end if;

      --  'not'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Not then
         Scan_Token;
         Error_Node := New_Node (K_Error_Containment,
         Token_Location);
      else
         Error_Node := New_Node (K_Error_Propagation,
         Token_Location);
      end if;
      Set_Error_Propagation_Point (Error_Node,
      Error_Propagation_Point);

      --  ( in | out )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_In =>
            Scan_Token;
            Propagation_Kind := PK_In;
         when T_Out =>
            Scan_Token;
            Propagation_Kind := PK_Out;
         when others =>
            Scan_Token;
            if Kind (Error_Node) = K_Error_Containment then
               DPE (PC_Error_Containment,
               Expected_Tokens => (T_In, T_Out));
            elsif Kind (Error_Node) = K_Error_Propagation then
               DPE (PC_Error_Propagation,
               Expected_Tokens => (T_Not, T_In, T_Out));
            end if;
            return No_Node;
      end case;

      Propagation :=  New_Node (K_Propagation, Token_Location);
      Set_Propagation_Kind (Propagation,
                            Ocarina.ME_AADL_EMA.Propagation_Kind'Pos
                            (Propagation_Kind));
      Set_Propagation (Error_Node, Propagation);

      --  'propagation'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Propagation then
         Scan_Token;
      else
         Scan_Token;
         if Kind (Error_Node) = K_Error_Containment then
            DPE (PC_Error_Containment,
            Expected_Token => T_Propagation);
         elsif Kind (Error_Node) = K_Error_Propagation then
            DPE (PC_Error_Propagation,
            Expected_Token => T_Propagation);
         end if;
         return No_Node;
      end if;

      --  error_type_set
      --  I have to test if '{' from error_type_set exists
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Error_Type_Set := P_Error_Type_Set;
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Error_Node, Error_Type_Set);
         else
            return No_Node;
         end if;
      else
         Scan_Token;
         DPE (PC_Error_Type_Set,
              Expected_Token => T_Left_Brace);
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Semi_Colon then
         Scan_Token;
      else
         Scan_Token;
         if Kind (Error_Node) = K_Error_Containment then
            DPE (PC_Error_Containment,
            Expected_Token => T_Semi_Colon);
         elsif Kind (Error_Node) = K_Error_Propagation then
            DPE (PC_Error_Propagation,
            Expected_Token => T_Semi_Colon);
         end if;
         return No_Node;
      end if;

      return Error_Node;
   end P_Error_Propagation_Or_Containment;

   ------------------
   -- P_Error_Flow --
   ------------------

   --  error_flow ::=
   --     error_source | error_sink | error_path

   function P_Error_Flow return Node_Id is
      Error_Flow : Node_Id;

      Error_Flow_Node : Node_Id;

      Loc, Loc_Start : Location;
      Next : Token_Type;
      Identifier : Node_Id;
   begin
      Error_Flow := New_Node (K_Error_Flow, Token_Location);

      --  All the nodes begin with : "identifier : error"
      Save_Lexer (Loc_Start);

      --  identifier
      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      end if;

      --  ':'
      Scan_Token;
      if Token /= T_Colon then
         DPE (PC_Error_Flow, Expected_Token => T_Colon);
         return No_Node;
      end if;

      --  'error'
      Scan_Token;
      if Token /= T_R_Error then
         DPE (PC_Error_Flow, Expected_Token => T_R_Error);
         return No_Node;
      end if;

      --  'source' or 'sink' or 'path'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_Source =>
            Restore_Lexer (Loc_Start);
            Error_Flow_Node := P_Error_Source;
            if Present (Error_Flow_Node) then
               Set_Error_Source (Error_Flow,
               Error_Flow_Node);
            else
               return No_Node;
            end if;
         when T_Sink =>
            Restore_Lexer (Loc_Start);
            Error_Flow_Node := P_Error_Sink;
            if Present (Error_Flow_Node) then
               Set_Error_Sink (Error_Flow,
               Error_Flow_Node);
            else
               return No_Node;
            end if;
         when T_Path =>
            Restore_Lexer (Loc_Start);
            Error_Flow_Node := P_Error_Path;
            if Present (Error_Flow_Node) then
               Set_Error_Path (Error_Flow,
               Error_Flow_Node);
            else
               return No_Node;
            end if;
         when others =>
            Scan_Token;
            DPE (PC_Error_Flow, Expected_Tokens => (T_Source,
                                                    T_Sink,
                                                    T_Path));
            return No_Node;
      end case;

      return Error_Flow;
   end P_Error_Flow;

   --------------------
   -- P_Error_Source --
   --------------------

   --  error_source ::=
   --   defining_error_source_identifier :
   --   error source
   --   ( outgoing_error_propagation_point | all )
   --   [ effect_error_type_set ] [ when fault_source ]
   --   [ if fault_condition ] ;

   --  The syntax effect_error_type_set is the same as
   --  error_type_set

   function P_Error_Source return Node_Id is
      Error_Source : Node_Id;

      Identifier : Node_Id;
      Outgoing_Error_Propagation_Point : Node_Id;
      Effect_Error_Type_Set : List_Id;
      Fault_Source : Node_Id;
      Fault_Condition : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
   begin
      Error_Source := New_Node (K_Error_Source, Token_Location);

      --  It 's useless to test the existance of the first part
      --  because it's already tested
      --  identifier
      Identifier := P_Identifier;
      Set_Identifier (Error_Source, Identifier);

      --  ':'
      Scan_Token;

      --  'error'
      Scan_Token;

      --  'source'
      Scan_Token;

      --  ( outgoing_error_propagation_point | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Save_Lexer (Loc_Start);
         Outgoing_Error_Propagation_Point := P_Error_Propagation_Point;
         if Present (Outgoing_Error_Propagation_Point) then
            Set_Outgoing_Error_Propagation_Point (Error_Source,
            Outgoing_Error_Propagation_Point);
         else
            Save_Lexer (Loc_End);
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Error_Source, PC_Outgoing_Error_Propagation_Point,
               Expected_Token => T_All);
            end if;
            return No_Node;
         end if;
      end if;

      --  [ effect_error_type_set ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Effect_Error_Type_Set := P_Error_Type_Set;
         if Present (Effect_Error_Type_Set) then
            Set_Effect_Error_Type_Set (Error_Source,
            Effect_Error_Type_Set);
         else
            return No_Node;
         end if;
      end if;

      --  [ when fault_source ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_When then
         Scan_Token;
         Fault_Source := P_Fault_Source;
         if Present (Fault_Source) then
            Set_Fault_Source (Error_Source, Fault_Source);
         else
            return No_Node;
         end if;
      end if;

      --  [ if fault_condition ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_If then
         Scan_Token;
         Fault_Condition := P_Fault_Condition;
         if Present (Fault_Condition) then
            Set_Fault_Condition (Error_Source, Fault_Condition);
         else
            return No_Node;
         end if;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Source, Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Error_Source;
   end P_Error_Source;

   --------------------
   -- P_Fault_Source --
   --------------------

   --  fault_source ::=
   --    ( error_behavior_state [ error_type_set ])
   --    | error_type_set | failure_mode_description

   function P_Fault_Source return Node_Id is
      Fault_Source : Node_Id;

      Error_Behavior_State : Node_Id;
      Error_Type_Set : List_Id;
      Failure_Mode_Description : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
      Exist : Boolean := True;
   begin
      Fault_Source := New_Node (K_Fault_Source, Token_Location);

      --  ( error_behavior_state [ error_type_set ])
      Save_Lexer (Loc_Start);
      Error_Behavior_State := P_Error_Behavior_State;
      Save_Lexer (Loc_End);
      if Present (Error_Behavior_State) then
         Set_Error_Behavior_State (Fault_Source,
         Error_Behavior_State);
         --  '{'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Left_Brace then
            Scan_Token;
         else
            return Fault_Source;
         end if;
         Save_Lexer (Loc_Start);
         Error_Type_Set := P_Error_Type_Set;
         Save_Lexer (Loc_End);
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Fault_Source,
            Error_Type_Set);
         elsif Loc_Start /= Loc_End then
            return No_Node;
         end if;
         return Fault_Source;
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      --  error_type_set
      --  '{'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Save_Lexer (Loc_Start);
         Error_Type_Set := P_Error_Type_Set;
         Save_Lexer (Loc_End);
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Fault_Source, Error_Type_Set);
            return Fault_Source;
         elsif Loc_Start /= Loc_End then
            return No_Node;
         end if;
      else
         Exist := False;
      end if;

      --  failure_mode_description
      Save_Lexer (Loc_Start);
      Failure_Mode_Description := P_Failure_Mode_Description;
      Save_Lexer (Loc_End);
      if Present (Failure_Mode_Description) then
         Set_Failure_Mode_Description (Fault_Source,
         Failure_Mode_Description);
         return Fault_Source;
      elsif Loc_Start = Loc_End and then Exist = False
      then
         Scan_Token;
         DPE (PC_Fault_Source, PC_Error_Behavior_State, PC_Error_Type_Set,
              PC_Failure_Mode_Description);
         return No_Node;
      else
         return No_Node;
      end if;

   end P_Fault_Source;

   --------------------------------
   -- P_Failure_Mode_Description --
   --------------------------------

   --  failure_mode_description ::= string_literal;

   function P_Failure_Mode_Description return Node_Id is
      Failure_Mode_Description : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Failure_Mode_Description :=
      New_Node (K_Literal, Token_Location);

      Save_Lexer (Loc);
      if Next_Token = T_String_Literal then
         Restore_Lexer (Loc);
         Scan_Token;
         Set_Value (Failure_Mode_Description,
         New_String_Value (String_Literal_Value));
      else
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Failure_Mode_Description, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;
      Scan_Token;

      return Failure_Mode_Description;
   end P_Failure_Mode_Description;

   -----------------------
   -- P_Fault_Condition --
   -----------------------

   --  fault_condition ::= string_literal;

   function P_Fault_Condition return Node_Id is
      Fault_Condition : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Fault_Condition :=
      New_Node (K_Literal, Token_Location);

      Save_Lexer (Loc);
      if Next_Token = T_String_Literal then
         Restore_Lexer (Loc);
         Scan_Token;
         Set_Value (Fault_Condition,
         New_String_Value (String_Literal_Value));
      else
         Restore_Lexer (Loc);
         Scan_Token;
         DPE (PC_Fault_Condition, Expected_Token => T_String_Literal);
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Fault_Condition, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;
      Scan_Token;

      return Fault_Condition;
   end P_Fault_Condition;

   ------------------
   -- P_Error_Sink --
   ------------------

   --  error_sink ::=
   --     defining_error_sink_identifier :
   --     error sink ( incoming_error_propagation_point | all )
   --     [ error_type_set ] ;

   function P_Error_Sink return Node_Id is
      Error_Sink : Node_Id;

      Incoming_Error_Propagation_Point : Node_Id;
      Error_Type_Set : List_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
      Identifier : Node_Id;
   begin
      Error_Sink := New_Node (K_Error_Sink, Token_Location);

      --  identifier
      Identifier := P_Identifier;
      Set_Identifier (Error_Sink, Identifier);

      --  ':'
      Scan_Token;

      --  'error'
      Scan_Token;

      --  'sink'
      Scan_Token;

      --  ( incoming_error_propagation_point | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Save_Lexer (Loc_Start);
         Incoming_Error_Propagation_Point := P_Error_Propagation_Point;
         if Present (Incoming_Error_Propagation_Point) then
            Set_Incoming_Error_Propagation_Point (Error_Sink,
            Incoming_Error_Propagation_Point);
         else
            Save_Lexer (Loc_End);
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Error_Sink, PC_Incoming_Error_Propagation_Point,
               Expected_Token => T_All);
            end if;
            return No_Node;
         end if;
      end if;

      --  [ error_type_set ]
      --  '{'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Error_Type_Set := P_Error_Type_Set;
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Error_Sink, Error_Type_Set);
         else
            return No_Node;
         end if;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Sink, Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Error_Sink;
   end P_Error_Sink;

   ------------------
   -- P_Error_Path --
   ------------------

   --  error_path ::=
   --    defining_error_path_identifier :
   --    error path
   --    ( incoming_error_propagation_point | all )
   --    [ error_type_set ] ->
   --    ( outgoing_error_propagation_point | all )
   --    [ target_error_type_instance ] ;

   function P_Error_Path return Node_Id is
      Error_Path : Node_Id;

      Incoming_Error_Propagation_Point : Node_Id;
      Error_Type_Set : List_Id;
      Outgoing_Error_Propagation_Point : Node_Id;
      Target_Error_Type_Instance : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
      Identifier : Node_Id;
   begin
      Error_Path := New_Node (K_Error_Path, Token_Location);

      --  identifier
      Identifier := P_Identifier;
      Set_Identifier (Error_Path, Identifier);

      --  ':'
      Scan_Token;

      --  'error'
      Scan_Token;

      --  'path'
      Scan_Token;

      --  ( incoming_error_propagation_point | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Save_Lexer (Loc_Start);
         Incoming_Error_Propagation_Point := P_Error_Propagation_Point;
         if Present (Incoming_Error_Propagation_Point) then
            Set_Incoming_Error_Propagation_Point (Error_Path,
            Incoming_Error_Propagation_Point);
         else
            Save_Lexer (Loc_End);
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Error_Path, PC_Incoming_Error_Propagation_Point,
               Expected_Token => T_All);
            end if;
            return No_Node;
         end if;
      end if;

      --  [ error_type_set ]
      --  '{'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Error_Type_Set := P_Error_Type_Set;
         if Present (Error_Type_Set) then
            Set_Error_Type_Set (Error_Path, Error_Type_Set);
         else
            return No_Node;
         end if;
      end if;

      --  '->'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Direct_Connection then
         Scan_Token;
         DPE (PC_Error_Path, Expected_Token => T_Direct_Connection);
         return No_Node;
      else
         Scan_Token;
      end if;

      --  ( outgoing_error_propagation_point | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Save_Lexer (Loc_Start);
         Outgoing_Error_Propagation_Point := P_Error_Propagation_Point;
         if Present (Outgoing_Error_Propagation_Point) then
            Set_Outgoing_Error_Propagation_Point (Error_Path,
            Outgoing_Error_Propagation_Point);
         else
            Save_Lexer (Loc_End);
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Error_Path, PC_Outgoing_Error_Propagation_Point,
               Expected_Token => T_All);
            end if;
            return No_Node;
         end if;
      end if;

      --  [ target_error_type_instance ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Save_Lexer (Loc_Start);
         Target_Error_Type_Instance := P_Target_Error_Type_Instance;
         Save_Lexer (Loc_End);
         if Present (Target_Error_Type_Instance) then
            Set_Target_Error_Type_Instance (Error_Path,
            Target_Error_Type_Instance);
         elsif Loc_Start /= Loc_End then
            return No_Node;
         end if;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Path, Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Error_Path;
   end P_Error_Path;

   --------------------------------
   -- P_Component_Error_Behavior --
   --------------------------------

   --  component_error_behavior ::=
   --   component error behavior
   --    [ use transformations error_type_transformation_set_reference ; ]
   --    [ events { error_behavior_event } + ]
   --    [ transitions { component_specific_error_behavior_transition } + ]
   --    [ propagations { outgoing_propagation_condition } + ]
   --    [ detections { error_detection } + ]
   --    [ mode mappings { error_state_to_mode_mapping } + ]
   --   end component;

   function P_Component_Error_Behavior return Node_Id is
      Component_Error_Behavior : Node_Id;

      Error_Type_Transformation_Set_Reference : Node_Id;
      Error_Behavior_Event_List : List_Id;
      Component_Specific_List : List_Id;
      Outgoing_Propagation_Condition_List : List_Id;
      Error_Detection_List : List_Id;
      Error_State_To_Mode_Mapping_List : List_Id;

      Next : Token_Type;
      Loc : Location;
   begin
      Component_Error_Behavior := New_Node (K_Component_Error_Behavior,
      Token_Location);

      --  'component'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Component then
         Scan_Token;
      else
         return No_Node;
      end if;

      --  'error'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_R_Error then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Component_Error_Behavior, Expected_Token => T_R_Error);
         return No_Node;
      end if;

      --  'behavior'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Behavior then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Component_Error_Behavior, Expected_Token => T_Behavior);
         return No_Node;
      end if;

      --  [ use transformations error_type_transformation_set_reference ; ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Transformations then
            Scan_Token;
            Error_Type_Transformation_Set_Reference :=
            P_Error_Type_Transformation_Set_Reference;
            if Present (Error_Type_Transformation_Set_Reference) then
               Set_Error_Type_Transformation_Set_Reference
               (Component_Error_Behavior,
               Error_Type_Transformation_Set_Reference);
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               if Next = T_Semi_Colon then
                  Scan_Token;
               else
                  Scan_Token;
                  DPE (PC_Component_Error_Behavior,
                  Expected_Token => T_Semi_Colon);
                  return No_Node;
               end if;
            else
               return No_Node;
            end if;
         else
            Scan_Token;
            DPE (PC_Component_Error_Behavior,
            Expected_Token => T_Transformations);
            return No_Node;
         end if;
      end if;

      --  [ events { error_behavior_event } + ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Events then
         Scan_Token;
         Error_Behavior_Event_List := P_Error_Behavior_Event_List;
         if Present (Error_Behavior_Event_List) then
            Set_Error_Behavior_Event_List (Component_Error_Behavior,
            Error_Behavior_Event_List);
         else
            return No_Node;
         end if;
      end if;

      --  [ transitions { component_specific_error_behavior_transition } + ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Transitions then
         Scan_Token;
         Component_Specific_List := P_Error_Behavior_Transition_List;
         if Present (Component_Specific_List) then
            Set_Component_Specific_Error_Behavior_Transition_List
            (Component_Error_Behavior,
            Component_Specific_List);
         else
            return No_Node;
         end if;
      end if;

      --  [ propagations { outgoing_propagation_condition } + ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Propagations then
         Scan_Token;
         Outgoing_Propagation_Condition_List :=
         P_Outgoing_Propagation_Condition_List;
         if Present (Outgoing_Propagation_Condition_List) then
            Set_Outgoing_Propagation_Condition_List
            (Component_Error_Behavior,
            Outgoing_Propagation_Condition_List);
         else
            return No_Node;
         end if;
      end if;

      --  [ detections { error_detection } + ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Detections then
         Scan_Token;
         Error_Detection_List := P_Error_Detection_List;
         if Present (Error_Detection_List) then
            Set_Error_Detection_List (Component_Error_Behavior,
            Error_Detection_List);
         else
            return No_Node;
         end if;
      end if;

      --  [ mode mappings { error_state_to_mode_mapping } + ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Mode then
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Mappings then
            Scan_Token;
            Error_State_To_Mode_Mapping_List :=
            P_Error_State_To_Mode_Mapping_List;
            if Present (Error_State_To_Mode_Mapping_List) then
               Set_Error_State_To_Mode_Mapping_List
               (Component_Error_Behavior,
               Error_State_To_Mode_Mapping_List);
            else
               return No_Node;
            end if;
         else
            Scan_Token;
            DPE (PC_Component_Error_Behavior, Expected_Token => T_Mappings);
            return No_Node;
         end if;
      end if;

      --  'end'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_End then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Component_Error_Behavior, Expected_Token => T_End);
         return No_Node;
      end if;

      --  'component'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Component then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Component_Error_Behavior, Expected_Token => T_Component);
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Component_Error_Behavior, Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Component_Error_Behavior;
   end P_Component_Error_Behavior;

   --------------------------------------
   -- P_Outgoing_Propagation_Condition --
   --------------------------------------

   --  outgoing_propagation_condition ::=
   --   [ defining_outgoing_propagation_identifier : ]
   --   ( error_source_state | all )
   --   -[ [ error_condition ] ]->
   --   propagation_target ;

   function P_Outgoing_Propagation_Condition return Node_Id is
      Outgoing_Propagation_Condition : Node_Id;

      Error_Source_State : Node_Id;
      Error_Condition : Node_Id;
      Propagation_Target : Node_Id;
      Identifier : Node_Id;

      Loc_Begin, Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
   begin
      Outgoing_Propagation_Condition := New_Node
      (K_Outgoing_Propagation_Condition, Token_Location);

      --  [ defining_outgoing_propagation_identifier : ]
      Save_Lexer (Loc_Begin);
      Identifier := P_Identifier;
      if Present (Identifier) then
         Set_Identifier (Outgoing_Propagation_Condition,
         Identifier);
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Colon then
            Scan_Token;
         else
            Restore_Lexer (Loc_Begin);
            --  because it may be error_source_state
         end if;
      end if;

      --  ( error_source_state | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Error_Source_State := P_Error_Source_State;
         if Present (Error_Source_State) then
            Set_Error_Source_State (
            Outgoing_Propagation_Condition,
            Error_Source_State);
         else
            return No_Node;
         end if;
      end if;

      --  -[ [ error_condition ] ]->
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Left_Step_Bracket then
         Scan_Token;
         DPE (PC_Outgoing_Propagation_Condition,
         Expected_Token => T_Left_Step_Bracket);
         return No_Node;
      else
         Scan_Token;
      end if;

      Save_Lexer (Loc_Start);
      Error_Condition := P_Error_Condition;
      Save_Lexer (Loc_End);
      if Present (Error_Condition) then
         Set_Error_Condition (Outgoing_Propagation_Condition,
         Error_Condition);
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Right_Step_Bracket then
         Scan_Token;
         DPE (PC_Outgoing_Propagation_Condition,
         Expected_Token => T_Right_Step_Bracket);
         return No_Node;
      else
         Scan_Token;
      end if;

      --  propagation_target
      Propagation_Target := P_Propagation_Target;
      if Present (Propagation_Target) then
         Set_Propagation_Target (Outgoing_Propagation_Condition,
         Propagation_Target);
      else
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Outgoing_Propagation_Condition,
         Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Outgoing_Propagation_Condition;
   end P_Outgoing_Propagation_Condition;

   --------------------------
   -- P_Propagation_Target --
   --------------------------

   --  propagation_target ::=
   --    ( error_propagation_point | all )
   --    [ propagated_target_error_type_instance | { noerror } ]

   function P_Propagation_Target return Node_Id is
      Propagation_Target : Node_Id;

      Error_Propagation_Point : Node_Id;
      Propagated_Target_Error_Type_Instance : Node_Id := No_Node;

      Next : Token_Type;
      Loc_Start, Loc_End, Loc, Loc_Node_Instance : Location;
   begin
      Propagation_Target := New_Node (K_Propagation_Target,
      Token_Location);

      --  ( error_propagation_point | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Save_Lexer (Loc_Start);
         Error_Propagation_Point := P_Error_Propagation_Point;
         Save_Lexer (Loc_End);
         if Present (Error_Propagation_Point) then
            Set_Error_Propagation_Point (Propagation_Target,
            Error_Propagation_Point);
         elsif Loc_Start = Loc_End then
            Scan_Token;
            DPE (PC_Propagation_Target, PC_Error_Propagation_Point,
            Expected_Token => T_All);
            return No_Node;
         end if;
      end if;

      --  [ propagated_target_error_type_instance | { noerror } ]
      Save_Lexer (Loc_Node_Instance);
      Next := Next_Token;
      Restore_Lexer (Loc_Node_Instance);
      if Next = T_Left_Brace then
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Noerror then
            Scan_Token;
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Right_Brace then
               Scan_Token;
               return Propagation_Target;
            else
               Scan_Token;
               DPE (PC_Propagation_Target, Expected_Token => T_Right_Brace);
               return No_Node;
            end if;
         else
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            Restore_Lexer (Loc_Node_Instance);
            Save_Lexer (Loc_Start);
            if Next = T_Identifier then
               Propagated_Target_Error_Type_Instance :=
               P_Target_Error_Type_Instance;
            end if;
            Save_Lexer (Loc_End);
            if Present (Propagated_Target_Error_Type_Instance)
            then
               Set_Propagated_Target_Error_Type_Instance
               (Propagation_Target,
               Propagated_Target_Error_Type_Instance);
            elsif Loc_Start = Loc_End then
               Scan_Token; --  '{'
               Scan_Token;
               DPE (PC_Propagation_Target, PC_Error_Type_Reference,
               PC_Error_Type_Product, Expected_Token => T_Noerror);
               return No_Node;
            else
               return No_Node;
            end if;
         end if;
      end if;

      return Propagation_Target;
   end P_Propagation_Target;

   -------------------------------------------
   -- P_Outgoing_Propagation_Condition_List --
   -------------------------------------------

   --  { outgoing_propagation_condition } +

   function P_Outgoing_Propagation_Condition_List return List_Id is
      List : List_Id;
      Node : Node_Id;
      Loc_Start, Loc_End : Location;
   begin
      List := New_List (K_List_Id, Token_Location);

      loop
         Save_Lexer (Loc_Start);
         Node := P_Outgoing_Propagation_Condition;
         Save_Lexer (Loc_End);
         exit when Node = No_Node;
         Append_Node_To_List (Node, List);
      end loop;

      if Is_Empty (List) then
         if Loc_Start = Loc_End then
            Scan_Token;
            DPE (PC_Component_Error_Behavior, PC_Error_Source_State,
            Expected_Token => T_All);
         end if;
         return No_List;
      end if;

      return List;
   end P_Outgoing_Propagation_Condition_List;

   -----------------------
   -- P_Error_Detection --
   -----------------------

   --  error_detection ::=
   --    [ defining_error_detection_identifier : ]
   --    ( error_source_state | all )
   --    -[ [ error_condition ] ]->
   --    error_detection_effect ;

   function P_Error_Detection return Node_Id is
      Error_Detection : Node_Id;

      Error_Source_State : Node_Id;
      Error_Condition : Node_Id;
      Error_Detection_Effect : Node_Id;
      Identifier : Node_Id;

      Loc_Begin, Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
      First_Node : Boolean := False;
   begin
      Error_Detection := New_Node (K_Error_Detection,
      Token_Location);

      --  [ defining_error_detection_identifier : ]
      Save_Lexer (Loc_Begin);
      Identifier := P_Identifier;
      if Present (Identifier) then
         Set_Identifier (Error_Detection, Identifier);
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Colon then
            Scan_Token;
            First_Node := True;
         else
            Restore_Lexer (Loc_Begin);
            --  because it may be error_source_state
         end if;
      end if;

      --  ( error_source_state | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Error_Source_State := P_Error_Source_State;
         if Present (Error_Source_State) then
            Set_Error_Source_State (
            Error_Detection, Error_Source_State);
         else
            if First_Node then
               Scan_Token;
               DPE (PC_Error_Detection, PC_Error_Source_State,
               Expected_Token => T_All);
            end if;
            return No_Node;
         end if;
      end if;

      --  -[ [ error_condition ] ]->
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Left_Step_Bracket then
         Scan_Token;
         DPE (PC_Error_Detection, Expected_Token => T_Left_Step_Bracket);
         return No_Node;
      else
         Scan_Token;
      end if;

      Save_Lexer (Loc_Start);
      Error_Condition := P_Error_Condition;
      Save_Lexer (Loc_End);
      if Present (Error_Condition) then
         Set_Error_Condition (Error_Detection,
         Error_Condition);
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Right_Step_Bracket then
         Scan_Token;
         DPE (PC_Error_Detection, Expected_Token => T_Right_Step_Bracket);
         return No_Node;
      else
         Scan_Token;
      end if;

      --  error_detection_effect
      Error_Detection_Effect := P_Error_Detection_Effect;
      if Present (Error_Detection_Effect) then
         Set_Error_Detection_Effect (Error_Detection,
         Error_Detection_Effect);
      else
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_Detection, Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Error_Detection;
   end P_Error_Detection;

   ------------------------------
   -- P_Error_Detection_Effect --
   ------------------------------

   --  error_detection_effect ::=
   --    ( outgoing_port_reference | internal_event_reference ) !
   --    [ ( error_code_value ) ]

   function P_Error_Detection_Effect return Node_Id is
      Error_Detection_Effect : Node_Id;
      Outgoing_Port_Reference : Node_Id;
      Internal_Event_Reference : Node_Id;
      Error_Code_Value : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
      First_Node : Boolean := False;
   begin
      Error_Detection_Effect := New_Node (K_Error_Detection_Effect,
      Token_Location);

      Save_Lexer (Loc_Start);
      Outgoing_Port_Reference := P_Port_Reference;
      Save_Lexer (Loc_End);
      if Present (Outgoing_Port_Reference) then
         Set_Outgoing_Port_Reference (Error_Detection_Effect,
         Outgoing_Port_Reference);
         First_Node := True;
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      if First_Node = False then
         Internal_Event_Reference := P_Internal_Event_Reference;
         if Present (Internal_Event_Reference) then
            Set_Internal_Event_Reference (Error_Detection_Effect,
            Internal_Event_Reference);
            First_Node := True;
         end if;
      end if;

      if First_Node = False then
         DPE (PC_Error_Detection_Effect, PC_Outgoing_Port_Reference,
         PC_Internal_Event_Reference);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Exclamation then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Error_Detection_Effect, Expected_Token => T_Exclamation);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
         Error_Code_Value := P_Error_Code_Value;
         if Present (Error_Code_Value) then
            Set_Error_Code_Value (Error_Detection_Effect,
            Error_Code_Value);
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Right_Paren then
               Scan_Token;
            else
               Scan_Token;
               DPE (PC_Error_Detection_Effect,
               Expected_Token => T_Right_Paren);
               return No_Node;
            end if;
         else
            return No_Node;
         end if;
      end if;

      return Error_Detection_Effect;
   end P_Error_Detection_Effect;

   --------------------------------
   -- P_Internal_Event_Reference --
   --------------------------------

   --  internal_event_reference ::= internal_event_identifier

   function P_Internal_Event_Reference return Node_Id is
      Internal_Event_Reference : Node_Id;
      Identifier : Node_Id;
   begin
      Internal_Event_Reference := New_Node (K_Internal_Event_Reference,
      Token_Location);

      Identifier := P_Identifier;
      if Present (Identifier) then
         Set_Identifier (Internal_Event_Reference, Identifier);
      else
         return No_Node;
      end if;

      return Internal_Event_Reference;
   end P_Internal_Event_Reference;

   ------------------------
   -- P_Error_Code_Value --
   ------------------------

   --  error_code_value ::= integer_literal | enumeration_identifier
   --   | property_constant_term

   function P_Error_Code_Value return Node_Id is
      Error_Code_Value : Node_Id := No_Node;
      Enumeration_Identifier : Node_Id;
      Property_Constant_Term : Node_Id;

      Loc_Begin, Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
      Exist_Integer, Exist_Others : Boolean := False;
   begin
      --  integer_literal
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_Integer_Literal =>
            Scan_Token;
            Error_Code_Value := New_Node (K_Literal, Token_Location);
            Set_Value (Error_Code_Value,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));
            Exist_Integer := True;
         when others =>
            null;
      end case;

      --  enumeration_identifier
      if Exist_Integer = False then
         Save_Lexer (Loc_Begin);
         Enumeration_Identifier := P_Identifier;
         if Present (Enumeration_Identifier) then
            Error_Code_Value := New_Node (K_Error_Code_Value, Token_Location);
            --  if the next token is '::' then it's the third node
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Colon_Colon then
               Restore_Lexer (Loc_Begin);
            else
               Set_Enumeration_Identifier (Error_Code_Value,
               Enumeration_Identifier);
               --  and it also may be the third node
               Restore_Lexer (Loc_Begin);
            end if;
         end if;
      end if;

      --  property_constant_term
      if Exist_Integer = False then
         Save_Lexer (Loc_Start);
         Property_Constant_Term := P_Property_Constant_Term;
         Save_Lexer (Loc_End);
         if Present (Property_Constant_Term) then
            Set_Property_Constant_Term (Error_Code_Value,
            Property_Constant_Term);
            Exist_Others := True;
         else
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Error_Code_Value, PC_Property_Constant_Term,
               Expected_Tokens => (T_Integer_Literal, T_Identifier));
            end if;
            return No_Node;
         end if;
      end if;

      if Exist_Integer or else Exist_Others
      then
         return Error_Code_Value;
      else
         return No_Node;
      end if;

   end P_Error_Code_Value;

   ------------------------------
   -- P_Property_Constant_Term --
   ------------------------------

   --  property_constant ::=
   --   single_valued_property_constant | multi_valued_property_constant

   --  single_valued_property_constant ::=
   --   defining_property_constant_identifier : constant
   --   property_type_designator
   --   => constant_property_expression;

   --  multi_valued_property_constant ::=
   --   defining_property_constant_identifier : constant list of
   --   property_type_designator
   --   => ( [constant_property_expression { , constant_property_expression } *
   --   ] ) ;

   --  unique_property_constant_identifier ::=
   --   [ property_set_identifier :: ] property_constant_identifier

   --  A revoir : the same as in aadl

   --  property_constant ::=
   --    [ property_set_identifier :: ] property_constant_identifier

   function P_Property_Constant_Term return Node_Id is
      Property_Cst_Node : Node_Id;
      Property_Cst_Id   : Node_Id;
      Property_Set_Id   : Node_Id := No_Node;

      Start_Loc       : Location;
      Loc             : Location;
   begin
      Property_Cst_Node := New_Node (K_Property_Constant_Term, Token_Location);
      Save_Lexer (Start_Loc);

      Property_Cst_Id := P_Identifier;
      if No (Property_Cst_Id) then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Colon_Colon then
         Property_Set_Id := Property_Cst_Id;
         Property_Cst_Id := P_Identifier;
         if No (Property_Cst_Id) then
            Scan_Token;
            DPE (PC_Property_Constant_Term, Expected_Token => T_Identifier);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Set_Property_Set (Property_Cst_Node, Property_Set_Id);
      Set_Identifier (Property_Cst_Node, Property_Cst_Id);

      if No (Property_Cst_Node) then
         return No_Node;
      else
         return Property_Cst_Node;
      end if;
   end P_Property_Constant_Term;

   ----------------------------
   -- P_Error_Detection_List --
   ----------------------------

   --  { error_detection } +

   function P_Error_Detection_List return List_Id is
      List : List_Id;
      Node : Node_Id;
      Loc_Start, Loc_End : Location;
   begin
      List := New_List (K_List_Id, Token_Location);

      loop
         Save_Lexer (Loc_Start);
         Node := P_Error_Detection;
         Save_Lexer (Loc_End);
         exit when Node = No_Node;
         Append_Node_To_List (Node, List);
      end loop;

      if Is_Empty (List) then
         if Loc_Start = Loc_End then
            Scan_Token;
            DPE (PC_Error_Detection, PC_Error_Source_State,
            Expected_Token => T_All);
         end if;
         return No_List;
      end if;
      return List;
   end P_Error_Detection_List;

   -----------------------------------
   -- P_Error_State_To_Mode_Mapping --
   -----------------------------------

   --  A revoir : lors de l'analyse sémantique
   --  error_behavior_state_identifier is an other node identifier
   --  and error_state_to_mode_mapping is not the reference of that node

   --  error_state_to_mode_mapping ::=
   --     error_behavior_state_identifier [ target_error_type_instance ]
   --     in modes ( mode_name { , mode_name } * ) ;

   function P_Error_State_To_Mode_Mapping return Node_Id is
      Error_State_To_Mode_Mapping : Node_Id;

      Error_Behavior_State_Identifier : Node_Id;
      Target_Error_Type_Instance : Node_Id;
      Mode_Name_List : List_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Error_State_To_Mode_Mapping := New_Node (K_Error_State_To_Mode_Mapping,
      Token_Location);

      --  error_behavior_state_identifier
      Error_Behavior_State_Identifier := P_Identifier;
      if Present (Error_Behavior_State_Identifier) then
         Set_Identifier (Error_State_To_Mode_Mapping,
         Error_Behavior_State_Identifier);
      else
         return No_Node;
      end if;

      --  [ target_error_type_instance ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Target_Error_Type_Instance := P_Target_Error_Type_Instance;
         if Present (Target_Error_Type_Instance) then
            Set_Target_Error_Type_Instance (Error_State_To_Mode_Mapping,
            Target_Error_Type_Instance);
         else
            return No_Node;
         end if;
      end if;

      --  'in modes'
      Scan_Token;
      if Token = T_In then
         Mode_Name_List := P_In_Modes (PC_Error_State_To_Mode_Mapping);
         if Present (Mode_Name_List) then
            Set_Mode_Name_List (Error_State_To_Mode_Mapping,
            Mode_Name_List);
         else
            return No_Node;
         end if;
      else
         DPE (PC_Error_State_To_Mode_Mapping, Expected_Token => T_In);
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next /= T_Semi_Colon then
         Scan_Token;
         DPE (PC_Error_State_To_Mode_Mapping, Expected_Token => T_Semi_Colon);
         return No_Node;
      else
         Scan_Token;
      end if;

      return Error_State_To_Mode_Mapping;
   end P_Error_State_To_Mode_Mapping;

   ----------------------------------------
   -- P_Error_State_To_Mode_Mapping_List --
   ----------------------------------------

   --  { error_state_to_mode_mapping } +

   function P_Error_State_To_Mode_Mapping_List return List_Id is
      List : List_Id;
      Node : Node_Id;

      Loc_Start, Loc_End : Location;
   begin
      List := New_List (K_List_Id, Token_Location);

      loop
         Save_Lexer (Loc_Start);
         Node := P_Error_State_To_Mode_Mapping;
         Save_Lexer (Loc_End);
         exit when Node = No_Node;
         Append_Node_To_List (Node, List);
      end loop;

      if Is_Empty (List) then
         if Loc_Start = Loc_End then
            Scan_Token;
            DPE (PC_Error_State_To_Mode_Mapping,
            Expected_Token => T_Error_Behavior_State_Id);
         end if;
         return No_List;
      end if;
      return List;
   end P_Error_State_To_Mode_Mapping_List;

   --------------------------------
   -- P_Composite_Error_Behavior --
   --------------------------------

   --  composite_error_behavior ::=
   --   composite error behavior
   --     states { composite_error_state } +
   --   end composite;

   function P_Composite_Error_Behavior return List_Id is
      Composite_Error_Behavior : List_Id;
      Composite_Error_State : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
   begin
      Composite_Error_Behavior := New_List (K_List_Id, Token_Location);

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Composite then
         Scan_Token;
      else
         return No_List;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_R_Error then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Composite_Error_Behavior,
         Expected_Token => T_R_Error);
         return No_List;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Behavior then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Composite_Error_Behavior,
         Expected_Token => T_Behavior);
         return No_List;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_States then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Composite_Error_Behavior,
         Expected_Token => T_States);
         return No_List;
      end if;

      --  { composite_error_state } +
      loop
         Save_Lexer (Loc_Start);
         Composite_Error_State := P_Composite_Error_State;
         Save_Lexer (Loc_End);
         if Present (Composite_Error_State) then
            Append_Node_To_List (Composite_Error_State,
            Composite_Error_Behavior);
         else
            if Loc_Start /= Loc_End then
               return No_List;
            end if;
            exit;
         end if;
      end loop;
      if Is_Empty (Composite_Error_Behavior) then
         DPE (PC_Composite_Error_Behavior, PC_Composite_Error_State);
         return No_List;
      end if;

      Scan_Token;
      if Token /= T_End then
         DPE (PC_Composite_Error_Behavior, Expected_Token => T_End);
         return No_List;
      end if;

      Scan_Token;
      if Token /= T_Composite then
         DPE (PC_Composite_Error_Behavior, Expected_Token => T_Composite);
         return No_List;
      end if;

      --  ';'
      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Composite_Error_Behavior,
         Expected_Token => T_Semi_Colon);
         return No_List;
      end if;

      return Composite_Error_Behavior;
   end P_Composite_Error_Behavior;

   -----------------------------
   -- P_Composite_Error_State --
   -----------------------------

   --  composite_error_state ::=
   --   [ defining_composite_error_state_identifier : ]
   --   [ ( subcomponent_state_expression | others ) ]->
   --   composite_state_identifier [ target_error_type_instance ]
   --   ;

   function P_Composite_Error_State return Node_Id is
      Composite_Error_State : Node_Id;

      Identifier : Node_Id;
      Composite_State_Identifier : Node_Id;
      Subcomponent_State_Expression : Node_Id;
      Target_Error_Type_Instance : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
      First_Node : Boolean := False;
   begin
      Composite_Error_State := New_Node (K_Composite_Error_State,
      Token_Location);

      --  [ defining_composite_error_state_identifier : ]
      Identifier := P_Identifier;
      if Present (Identifier) then
         Set_Identifier (Composite_Error_State, Identifier);
         Scan_Token;
         if Token = T_Colon then
            First_Node := True;
         else
            DPE (PC_Composite_Error_State, Expected_Token => T_Colon);
            return No_Node;
         end if;
      end if;

      --  [ ( subcomponent_state_expression | others )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Bracket then
         Scan_Token;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Others then
            Scan_Token;
         else
            Save_Lexer (Loc_Start);
            Subcomponent_State_Expression := P_Composite_State_Expression;
            Save_Lexer (Loc_End);
            if Present (Subcomponent_State_Expression) then
               Set_Subcomponent_State_Expression (Composite_Error_State,
               Subcomponent_State_Expression);
            else
               if Loc_Start = Loc_End then
                  Scan_Token;
                  DPE (PC_Composite_Error_State,
                  PC_Subcomponent_State_Expression,
                  Expected_Token => T_Others);
               end if;
               return No_Node;
            end if;
         end if;
      else
         if First_Node then
            Scan_Token;
            DPE (PC_Composite_Error_State, Expected_Token => T_Left_Bracket);
         end if;
         return No_Node;
      end if;

      --  ']->'
      Scan_Token;
      if Token /= T_Right_Step_Bracket then
         DPE (PC_Composite_Error_State,
         Expected_Token => T_Right_Step_Bracket);
         return No_Node;
      end if;

      --  composite_state_identifier
      Composite_State_Identifier := P_Identifier;
      if Present (Composite_State_Identifier) then
         Set_Composite_State_Identifier (Composite_Error_State,
         Composite_State_Identifier);
      else
         Scan_Token;
         DPE (PC_Composite_Error_State,
         Expected_Token => T_Composite_State_Id);
         return No_Node;
      end if;

      --  [ target_error_type_instance ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Target_Error_Type_Instance := P_Target_Error_Type_Instance;
         if Present (Target_Error_Type_Instance) then
            Set_Target_Error_Type_Instance (Composite_Error_State,
            Target_Error_Type_Instance);
         else
            return No_Node;
         end if;
      end if;

      if Present (Target_Error_Type_Instance) then
         Set_Target_Error_Type_Instance (Composite_Error_State,
         Target_Error_Type_Instance);
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      --  ';'
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Semi_Colon then
         Scan_Token;
      else
         Scan_Token;
         DPE (PC_Composite_Error_State, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Composite_Error_State;
   end P_Composite_Error_State;

   ----------------------------------
   -- P_Composite_State_Expression --
   ----------------------------------

   --  composite_state_expression ::=
   --    composite_state_element
   --    | (composite_state_expression )
   --    | composite_state_expression and composite_state_expression
   --    | composite_state_expression or composite_state_expression
   --    | numeric_literal ormore
   --    (composite_state_element { , composite_state_element } + )
   --    | numeric_literal orless
   --    (composite_state_element { , composite_state_element } + )

   function P_Composite_State_Expression return Node_Id is
      use Ocarina.ME_AADL_EMA;

      Composite_State_Expression : Node_Id;

      Composite_State_Element : Node_Id;
      Composite_State_Expression_Node : Node_Id;
      Numeric_Literal : Node_Id;
      Operator_Node : Node_Id := No_Node;
      Operat_Kind   : Ocarina.ME_AADL_EMA.Operator_Kind;
      Composite_State_Element_List : List_Id;

      Loc, Loc_Start, Loc_End : Location;
      Next : Token_Type;
      Escape : Boolean := False;
   begin
      Composite_State_Expression := New_Node (K_Composite_State_Expression,
      Token_Location);

      --  Composite_State_Element
      Composite_State_Element := P_Composite_State_Element;
      if Present (Composite_State_Element) then
         Set_Composite_State_Element (Composite_State_Expression,
         Composite_State_Element);
         return Composite_State_Expression;
      end if;

      --  ( Composite_State_Expression_Node )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
         Save_Lexer (Loc_Start);
         Composite_State_Expression_Node := P_Composite_State_Expression;
         Save_Lexer (Loc_End);
         Set_Composite_State_Expression_Node (Composite_State_Expression,
         Composite_State_Expression_Node);
         if No (Composite_State_Expression_Node) then
            if Loc_Start = Loc_End then
               DPE (PC_Composite_State_Expression,
               PC_Composite_State_Expression);
            end if;
            return No_Node;
         else
            Scan_Token;
            if Token /= T_Right_Paren then
               DPE (PC_Composite_State_Expression,
               Expected_Token => T_Right_Paren);
               return No_Node;
            else
               return Composite_State_Expression;
            end if;
         end if;
      end if;

      --  Composite_State_Expression and Composite_State_Expression
      --  Composite_State_Expression or Composite_State_Expression
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_And =>
            Scan_Token;
            Operat_Kind := OK_And;
         when T_Or =>
            Scan_Token;
            Operat_Kind := OK_Or;
         when others =>
            Escape := True;
      end case;

      if Escape = False then
         Operator_Node :=  New_Node (K_Operator, Token_Location);
         Set_Operator_Kind (Operator_Node,
                            Ocarina.ME_AADL_EMA.Operator_Kind'Pos
                            (Operat_Kind));
         Set_Operator (Composite_State_Expression, Operator_Node);

         Composite_State_Expression_Node := P_Composite_State_Expression;
         if No (Composite_State_Expression_Node) then
            Scan_Token;
            DPE (PC_Composite_State_Expression,
            PC_Composite_State_Expression,
            PC_Composite_State_Element, Expected_Tokens =>
            (T_Left_Paren, T_Numeric_Literal));
            return No_Node;
         else
            Set_Composite_State_Expression_Node (Composite_State_Expression,
            Composite_State_Expression_Node);
            return Composite_State_Expression;
         end if;
      end if;

      --  numeric_literal ormore
      --  ( Composite_State_Element { , Composite_State_Element } + )
      --  numeric_literal orless
      --  ( Composite_State_Element { , Composite_State_Element } + )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      case Next is
         when T_Real_Literal =>
            Scan_Token;
            Numeric_Literal := New_Node (K_Literal, Token_Location);
            Set_Value (Numeric_Literal,
                       New_Real_Value (Real_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));

         when T_Integer_Literal =>
            Scan_Token;
            Numeric_Literal := New_Node (K_Literal, Token_Location);
            Set_Value (Numeric_Literal,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));
         when others =>
            return No_Node;
      end case;
      Set_Numeric_Literal (Composite_State_Expression, Numeric_Literal);

      --  orless | ormore
      Scan_Token;
      case Token is
         when T_OrLess =>
            Operat_Kind := OK_OrLess;
         when T_OrMore =>
            Operat_Kind := OK_OrMore;
         when others =>
            DPE (PC_Operator, EMC_Operator_Unknown, "");
            return No_Node;
      end case;

      Operator_Node :=  New_Node (K_Operator, Token_Location);
      Set_Operator_Kind (Operator_Node,
                         Ocarina.ME_AADL_EMA.Operator_Kind'Pos
                         (Operat_Kind));
      Set_Operator (Composite_State_Expression, Operator_Node);

      --  '('
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
         --  Composite_State_Element
         Save_Lexer (Loc_Start);
         Composite_State_Element := P_Composite_State_Element;
         Save_Lexer (Loc_End);
         if Present (Composite_State_Element) then
            Composite_State_Element_List := New_List (K_List_Id,
            Token_Location);
            Set_Composite_State_Element_List (Composite_State_Expression
            , Composite_State_Element_List);
            Append_Node_To_List
            (Composite_State_Element, Composite_State_Element_List);

            --  { , composite_state_element } +
            Scan_Token;
            if Token /= T_Comma then
               DPE (PC_Composite_State_Expression,
               Expected_Token => T_Comma);
               return No_Node;
            end if;
            loop
               Save_Lexer (Loc_Start);
               Composite_State_Element := P_Composite_State_Element;
               Save_Lexer (Loc_End);
               if No (Composite_State_Element) then
                  if Loc_Start = Loc_End then
                     DPE (PC_Composite_State_Expression,
                     PC_Composite_State_Element);
                  end if;
                  return No_Node;
               end if;

               Append_Node_To_List
               (Composite_State_Element, Composite_State_Element_List);

               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               --  Composite_State_Element_List must contain at
               --  least 3 elements
               if Next /= T_Comma and then
               Length (Composite_State_Element_List) = 2
               then
                  Scan_Token;
                  DPE (PC_Composite_State_Expression,
                  Expected_Token => T_Comma);
                  return No_Node;
               end if;
               exit when Next /= T_Comma;
               Scan_Token; -- ','
            end loop;

            --  ')'
            Scan_Token;
            if Token /= T_Right_Paren then
               DPE (PC_Composite_State_Expression,
               Expected_Token => T_Right_Paren);
               return No_Node;
            end if;
         else
            if Loc_Start = Loc_End then
               DPE (PC_Composite_State_Expression,
               PC_Composite_State_Element);
            end if;
            return No_Node;
         end if;
      else
         Scan_Token;
         DPE (PC_Composite_State_Expression,
         Expected_Token => T_Left_Paren);
         return No_Node;
      end if;

      return Composite_State_Expression;
   end P_Composite_State_Expression;

   -------------------------------
   -- P_Composite_State_Element --
   -------------------------------

   --  composite_state_element ::=
   --    subcomponent_error_state [ error_type_set ]
   --    | in incoming_error_propagation_point [ error_type_set_or_noerror ]

   function P_Composite_State_Element return Node_Id is
      Composite_State_Element : Node_Id;

      Subcomponent_Error_State : Node_Id;
      Error_Type_Set : List_Id;
      Incoming_Error_Propagation_Point : Node_Id;
      Error_Type_Set_Or_Noerror : Node_Id;

      Next : Token_Type;
      Loc_Start, Loc_End, Loc : Location;
   begin
      Composite_State_Element := New_Node (K_Composite_State_Element,
      Token_Location);

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_In then
         Scan_Token;
         Save_Lexer (Loc_Start);
         Incoming_Error_Propagation_Point :=
         P_Error_Propagation_Point;
         Save_Lexer (Loc_End);
         if Present (Incoming_Error_Propagation_Point) then
            Set_Incoming_Error_Propagation_Point (Composite_State_Element,
            Incoming_Error_Propagation_Point);
            --  '{'
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Left_Brace then
               Scan_Token;
               Error_Type_Set_Or_Noerror := P_Error_Type_Set_Or_Noerror;
               if Present (Error_Type_Set_Or_Noerror) then
                  Set_Error_Type_Set_Or_Noerror (Composite_State_Element,
                  Error_Type_Set_Or_Noerror);
               else
                  return No_Node;
               end if;
            end if;
         else
            if Loc_Start = Loc_End then
               DPE (PC_Composite_State_Element,
               PC_Incoming_Error_Propagation_Point);
            end if;
            return No_Node;
         end if;
      else
         Save_Lexer (Loc_Start);
         Subcomponent_Error_State := P_Subcomponent_Error_State;
         Save_Lexer (Loc_End);
         if Present (Subcomponent_Error_State) then
            Set_Subcomponent_Error_State (Composite_State_Element,
            Subcomponent_Error_State);
            --  '{'
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Left_Brace then
               Scan_Token;
               Error_Type_Set := P_Error_Type_Set;
               if Present (Error_Type_Set) then
                  Set_Error_Type_Set (Composite_State_Element,
                  Error_Type_Set);
               else
                  return No_Node;
               end if;
            end if;
         else
            return No_Node;
         end if;
      end if;

      return Composite_State_Element;
   end P_Composite_State_Element;

   --------------------------------
   -- P_Subcomponent_Error_State --
   --------------------------------

   --  subcomponent_error_state ::=
   --    { subcomponent_identifier . } + error_behavior_state_identifier

   function P_Subcomponent_Error_State return Node_Id is
      Subcomponent_Error_State : Node_Id;

      Subcomponent_Identifier_List : List_Id;
      Subcomponent_Identifier : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Subcomponent_Error_State := New_Node (K_Subcomponent_Error_State,
      Token_Location);

      Subcomponent_Identifier := P_Identifier;
      if No (Subcomponent_Identifier) then
         return No_Node;
      else
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Dot then
            Scan_Token;
            Subcomponent_Identifier_List := New_List
            (K_List_Id, Token_Location);
            Set_Subcomponent_Identifier_List (Subcomponent_Error_State,
            Subcomponent_Identifier_List);
            Append_Node_To_List (Subcomponent_Identifier,
            Subcomponent_Identifier_List);
            loop
               Subcomponent_Identifier := P_Identifier;
               if No (Subcomponent_Identifier) then
                  exit;
               end if;
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               if Next = T_Dot then
                  Scan_Token;
                  Append_Node_To_List (Subcomponent_Identifier,
                  Subcomponent_Identifier_List);
               else
                  exit;
               end if;
            end loop;
            if Present (Subcomponent_Identifier) then
               Set_Identifier (Subcomponent_Error_State,
               Subcomponent_Identifier);
            else
               Scan_Token;
               DPE (PC_Subcomponent_Error_State,
               Expected_Token => T_Error_Behavior_State_Id);
               return No_Node;
            end if;
         else
            Scan_Token;
            DPE (PC_Subcomponent_Error_State,
            Expected_Token => T_Dot);
            return No_Node;
         end if;
      end if;

      return Subcomponent_Error_State;
   end P_Subcomponent_Error_State;

   ---------------------------------
   -- P_Connection_Error_Behavior --
   ---------------------------------

   --  connection_error_behavior ::=
   --   connection error
   --    [ use transformations error_type_transformation_set_reference ; ]
   --    { connection_error_source }*
   --   end connection;

   function P_Connection_Error_Behavior return Node_Id is
      Connection_Error_Behavior : Node_Id;

      Error_Type_Transformation_Set_Reference : Node_Id;
      Connection_Error_Source_List : List_Id;
      Connection_Error_Source : Node_Id;

      Next : Token_Type;
      Loc_Start, Loc_End, Loc : Location;
      Escape : Boolean := True;
   begin
      Connection_Error_Behavior := New_Node (K_Connection_Error_Behavior,
      Token_Location);

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Connection then
         Scan_Token;
         Scan_Token;
         if Token /= T_R_Error then
            DPE (PC_Connection_Error_Behavior,
            Expected_Token => T_R_Error);
            return No_Node;
         end if;
      else
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Use then
         Scan_Token;
         Scan_Token;
         if Token = T_Transformations then
            Escape := False;
         else
            DPE (PC_Connection_Error_Behavior,
            Expected_Token => T_Transformations);
            return No_Node;
         end if;
      end if;

      if Escape = False then
         Error_Type_Transformation_Set_Reference :=
         P_Error_Type_Transformation_Set_Reference;
         if Present (Error_Type_Transformation_Set_Reference) then
            Set_Error_Type_Transformation_Set_Reference
            (Connection_Error_Behavior,
            Error_Type_Transformation_Set_Reference);
            Scan_Token;
            if Token /= T_Semi_Colon then
               DPE (PC_Connection_Error_Behavior,
               Expected_Token => T_Semi_Colon);
               return No_Node;
            end if;
         else
            return No_Node;
         end if;
      end if;

      Save_Lexer (Loc_Start);
      Connection_Error_Source := P_Connection_Error_Source;
      Save_Lexer (Loc_End);
      if Present (Connection_Error_Source) then
         Connection_Error_Source_List := New_List
         (K_List_Id, Token_Location);
         Set_Connection_Error_Source_List (Connection_Error_Behavior,
         Connection_Error_Source_List);
         Append_Node_To_List (Connection_Error_Source,
         Connection_Error_Source_List);
         loop
            Save_Lexer (Loc_Start);
            Connection_Error_Source := P_Connection_Error_Source;
            Save_Lexer (Loc_End);

            if Loc_Start /= Loc_End then
               return No_Node;
            end if;

            exit when Connection_Error_Source = No_Node;
            Append_Node_To_List (Connection_Error_Source,
            Connection_Error_Source_List);
         end loop;
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_End then
         DPE (PC_Connection_Error_Behavior,
         Expected_Token => T_End);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Connection then
         DPE (PC_Connection_Error_Behavior,
         Expected_Token => T_Connection);
         return No_Node;
      end if;

      --  ';'
      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Connection_Error_Behavior,
         Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Connection_Error_Behavior;
   end P_Connection_Error_Behavior;

   -------------------------------
   -- P_Connection_Error_Source --
   -------------------------------

   --  connection_error_source ::=
   --    defining_error_source_identifier :
   --    error source ( connection_identifier | all )
   --    [ effect_error_type_set ]
   --    [ when ( fault_source_error_type_set | failure_mode_description ) ]
   --    [ if fault_condition ] ;

   function P_Connection_Error_Source return Node_Id is
      Connection_Error_Source : Node_Id;

      Effect_Error_Type_Set : List_Id;
      Fault_Source_Error_Type_Set : List_Id;
      Fault_Condition : Node_Id;
      Identifier : Node_Id;
      Connection_Identifier : Node_Id;
      Failure_Mode_Description : Node_Id;

      Loc : Location;
      Next : Token_Type;
   begin
      Connection_Error_Source := New_Node (K_Connection_Error_Source,
      Token_Location);

      --  identifier
      Identifier := P_Identifier;
      if No (Identifier) then
         return No_Node;
      else
         Set_Identifier (Connection_Error_Source, Identifier);
      end if;

      --  ':'
      Scan_Token;
      if Token /= T_Colon then
         DPE (PC_Connection_Error_Source,
         Expected_Token => T_Colon);
         return No_Node;
      end if;

      --  'error'
      Scan_Token;
      if Token /= T_R_Error then
         DPE (PC_Connection_Error_Source,
         Expected_Token => T_R_Error);
         return No_Node;
      end if;

      --  'source'
      Scan_Token;
      if Token /= T_Source then
         DPE (PC_Connection_Error_Source,
         Expected_Token => T_Source);
         return No_Node;
      end if;

      --  ( connection_identifier | all )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_All then
         Scan_Token;
      else
         Connection_Identifier := P_Identifier;
         if Present (Connection_Identifier) then
            Set_Connection_Identifier (Connection_Error_Source,
            Connection_Identifier);
         else
            Scan_Token;
            DPE (PC_Connection_Error_Source,
            Expected_Tokens => (T_All, T_Identifier));
            return No_Node;
         end if;
      end if;

      --  [ effect_error_type_set ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Brace then
         Scan_Token;
         Effect_Error_Type_Set := P_Error_Type_Set;
         if Present (Effect_Error_Type_Set) then
            Set_Effect_Error_Type_Set (Connection_Error_Source,
            Effect_Error_Type_Set);
         else
            return No_Node;
         end if;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_When then
         Scan_Token;
         --  '{'
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Left_Brace then
            Scan_Token;
            Fault_Source_Error_Type_Set := P_Error_Type_Set;
            if Present (Fault_Source_Error_Type_Set) then
               Set_Fault_Source_Error_Type_Set (Connection_Error_Source,
               Fault_Source_Error_Type_Set);
            else
               return No_Node;
            end if;
         else
            Failure_Mode_Description := P_Failure_Mode_Description;
            if Present (Failure_Mode_Description) then
               Set_Failure_Mode_Description (Connection_Error_Source,
               Failure_Mode_Description);
            else
               DPE (PC_Connection_Error_Source, PC_Failure_Mode_Description,
               PC_Fault_Source_Error_Type_Set);
               return No_Node;
            end if;
         end if;
      end if;

      --  [ if fault_condition ]
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_If then
         Scan_Token;
         Fault_Condition := P_Fault_Condition;
         if Present (Fault_Condition) then
            Set_Fault_Condition (Connection_Error_Source, Fault_Condition);
         else
            return No_Node;
         end if;
      end if;

      --  ';'
      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Connection_Error_Source,
         Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Connection_Error_Source;
   end P_Connection_Error_Source;

   -------------------------
   -- P_Propagation_Paths --
   -------------------------

   --  propagation_paths ::=
   --    propagation paths
   --     { propagation_point } *
   --     { propagation_path } *
   --    end paths ;

   function P_Propagation_Paths return Node_Id is
      Propagation_Paths : Node_Id;

      Propagation_Point_List : List_Id;
      Propagation_Point : Node_Id;
      Propagation_Path_List : List_Id;
      Propagation_Path : Node_Id;

      Loc, Loc_Start, Loc_End : Location;
      Next : Token_Type;
   begin
      Propagation_Paths := New_Node (K_Propagation_Paths,
      Token_Location);

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Propagation then
         Scan_Token;
      else
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Paths then
         DPE (PC_Propagation_Paths, Expected_Token => T_Paths);
         return No_Node;
      end if;

      --  { propagation_point } *
      Save_Lexer (Loc_Start);
      Propagation_Point := P_Propagation_Point;
      Save_Lexer (Loc_End);
      if Present (Propagation_Point) then
         Propagation_Point_List := New_List
         (K_List_Id, Token_Location);
         Set_Propagation_Point_List (Propagation_Paths,
         Propagation_Point_List);
         Append_Node_To_List (Propagation_Point,
         Propagation_Point_List);
         loop
            Save_Lexer (Loc_Start);
            Propagation_Point := P_Propagation_Point;
            Save_Lexer (Loc_End);

            if Loc_Start /= Loc_End then
               return No_Node;
            end if;

            exit when Propagation_Point = No_Node;
            Append_Node_To_List (Propagation_Point,
            Propagation_Point_List);
         end loop;
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      --  { propagation_path } *
      Save_Lexer (Loc_Start);
      Propagation_Path := P_Propagation_Path;
      Save_Lexer (Loc_End);
      if Present (Propagation_Path) then
         Propagation_Path_List := New_List
         (K_List_Id, Token_Location);
         Set_Propagation_Path_List (Propagation_Paths,
         Propagation_Path_List);
         Append_Node_To_List (Propagation_Path,
         Propagation_Path_List);
         loop
            Save_Lexer (Loc_Start);
            Propagation_Path := P_Propagation_Path;
            Save_Lexer (Loc_End);

            if Loc_Start /= Loc_End then
               return No_Node;
            end if;

            exit when Propagation_Path = No_Node;
            Append_Node_To_List (Propagation_Path,
            Propagation_Path_List);
         end loop;
      elsif Loc_Start /= Loc_End then
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_End then
         DPE (PC_Propagation_Paths, Expected_Token => T_End);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Paths then
         DPE (PC_Propagation_Paths, Expected_Token => T_Paths);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Propagation_Paths, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Propagation_Paths;
   end P_Propagation_Paths;

   -----------------------
   -- Propagation_Point --
   -----------------------

   --  propagation_point ::=
   --    defining_propagation_point_identifier : propagation point ;

   function P_Propagation_Point return Node_Id is
      Propagation_Point : Node_Id;
      Identifier : Node_Id;

      Loc, Loc_Start : Location;
      Next : Token_Type;
   begin
      Propagation_Point := New_Node (K_Propagation_Point,
      Token_Location);

      Save_Lexer (Loc_Start);

      Identifier := P_Identifier;
      if Present (Identifier) then
         Set_Identifier (Propagation_Point, Identifier);
      else
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Colon then
         DPE (PC_Propagation_Point, Expected_Token => T_Colon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Propagation then
         Scan_Token;
      else
         --  it may be propagation_path
         Restore_Lexer (Loc_Start);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Point then
         DPE (PC_Propagation_Point, Expected_Token => T_Point);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Propagation_Point, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Propagation_Point;
   end P_Propagation_Point;

   ----------------------
   -- Propagation_Path --
   ----------------------

   --  propagation_path ::=
   --    defining_observable_propagation_path_identifier :
   --    source_qualified_propagation_point ->
   --    target_qualified_propagation_point ;

   function P_Propagation_Path return Node_Id is
      Propagation_Path : Node_Id;

      Identifier : Node_Id;
      Source, Target : Node_Id;

      Loc_Start, Loc_End, Loc : Location;
      Next : Token_Type;
   begin
      Propagation_Path := New_Node (K_Propagation_Path,
      Token_Location);

      Identifier := P_Identifier;
      if Present (Identifier) then
         Set_Identifier (Propagation_Path, Identifier);
      else
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Colon then
         Scan_Token;
      else
         return No_Node;
      end if;

      Save_Lexer (Loc_Start);
      Source := P_Qualified_Propagation_Point
      (PC_Source_Qualified_Propagation_Point);
      Save_Lexer (Loc_End);
      if Present (Source) then
         Set_Source_Qualified_Propagation_Point
         (Propagation_Path, Source);
      else
         if Loc_Start = Loc_End then
            Scan_Token;
            DPE (PC_Propagation_Paths, PC_Source_Qualified_Propagation_Point,
            Expected_Token => T_Propagation);
         end if;
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Direct_Connection then
         DPE (PC_Propagation_Path,
         Expected_Token => T_Direct_Connection);
         return No_Node;
      end if;

      Save_Lexer (Loc_Start);
      Target := P_Qualified_Propagation_Point
      (PC_Target_Qualified_Propagation_Point);
      Save_Lexer (Loc_End);
      if Present (Target) then
         Set_Target_Qualified_Propagation_Point
         (Propagation_Path, Target);
      else
         if Loc_Start = Loc_End then
            DPE (PC_Propagation_Path, PC_Target_Qualified_Propagation_Point);
         end if;
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Propagation_Path, Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Propagation_Path;
   end P_Propagation_Path;

   -----------------------------------
   -- P_Qualified_Propagation_Point --
   -----------------------------------

   function P_Qualified_Propagation_Point (Code_Expected  : Parsing_Code)
   return Node_Id is
      Qualified_Propagation_Point : Node_Id;

      Subcomponent_Identifier_List : List_Id;
      Subcomponent_Identifier : Node_Id;

      Next : Token_Type;
      Loc : Location;
   begin
      Qualified_Propagation_Point := New_Node (K_Qualified_Propagation_Point,
      Token_Location);

      Subcomponent_Identifier := P_Identifier;
      if No (Subcomponent_Identifier) then
         return No_Node;
      else
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Dot then
            Scan_Token;
            Subcomponent_Identifier_List := New_List
            (K_List_Id, Token_Location);
            Set_Subcomponent_Identifier_List (Qualified_Propagation_Point,
            Subcomponent_Identifier_List);
            Append_Node_To_List (Subcomponent_Identifier,
            Subcomponent_Identifier_List);
            loop
               Subcomponent_Identifier := P_Identifier;
               if No (Subcomponent_Identifier) then
                  exit;
               end if;
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               if Next = T_Dot then
                  Scan_Token;
                  Append_Node_To_List (Subcomponent_Identifier,
                  Subcomponent_Identifier_List);
               else
                  exit;
               end if;
            end loop;
            if Present (Subcomponent_Identifier) then
               Set_Identifier (Qualified_Propagation_Point,
               Subcomponent_Identifier);
            else
               Scan_Token;
               DPE (Code_Expected, Expected_Token => T_Propagation_Point_Id);
               return No_Node;
            end if;
         else
            Scan_Token;
            DPE (Code_Expected, Expected_Token => T_Dot);
            return No_Node;
         end if;
      end if;

      return Qualified_Propagation_Point;
   end P_Qualified_Propagation_Point;

   -------------------------------
   -- P_EMV2_Properties_Section --
   -------------------------------

   --  EMV2_properties_section ::=
   --    properties
   --    { emv2_contained_property_association }+

   --  properties is scanned before entering the function

   function P_EMV2_Properties_Section return List_Id is
      EMV2_Properties_Section : List_Id := No_List;
      Emv2_Contained_Property_Association : Node_Id;

      Loc_Start, Loc_End : Location;
   begin
      EMV2_Properties_Section := New_List
      (K_List_Id, Token_Location);
      loop
         Save_Lexer (Loc_Start);
         Emv2_Contained_Property_Association :=
         P_Emv2_Contained_Property_Association;
         Save_Lexer (Loc_End);
         if Present (Emv2_Contained_Property_Association) then
            Append_Node_To_List (Emv2_Contained_Property_Association,
            EMV2_Properties_Section);
         else
            if Loc_Start = Loc_End and then
            Is_Empty (EMV2_Properties_Section)
            then
               DPE (PC_Emv2_Contained_Property_Association,
               Expected_Token => T_Property_Identifier);
               return No_List;
            end if;
         end if;
         exit when Emv2_Contained_Property_Association = No_Node;
      end loop;

      return EMV2_Properties_Section;
   end P_EMV2_Properties_Section;

   -------------------------------------------
   -- P_Emv2_Contained_Property_Association --
   -------------------------------------------

   --  emv2_contained_property_association ::=
   --   unique_property_identifier => [ constant ] assignment applies to
   --   emv2_containment_path { , emv2_containment_path }* ;

   --  The errpr message of the first token is taken care by the lexer
   function P_Emv2_Contained_Property_Association return Node_Id is
      Emv2_Contained_Property_Association : Node_Id;

      Property_Identifier : Node_Id;
      Emv2_Containment_Path_List : List_Id;
      Emv2_Containment_Path : Node_Id;
      Assignment : Node_Id;
      Is_Constant : Boolean := False;

      Loc, Loc_Start, Loc_End : Location;
      Next : Token_Type;
   begin
      Emv2_Contained_Property_Association := New_Node
      (K_Emv2_Contained_Property_Association, Token_Location);

      Save_Lexer (Loc_Start);
      Property_Identifier := P_Property_Identifier;
      if Present (Property_Identifier) then
         Set_Property_Identifier (Emv2_Contained_Property_Association,
         Property_Identifier);
      else
         Restore_Lexer (Loc_Start);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Association then
         DPE (PC_Emv2_Contained_Property_Association,
         Expected_Token => T_Association);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Constant then
         Scan_Token;
         Is_Constant := True;
         Set_Is_Constant (Emv2_Contained_Property_Association,
         Is_Constant);
      end if;

      Assignment := P_Assignment;
      if Present (Assignment) then
         Set_Assignment (Emv2_Contained_Property_Association,
         Assignment);
      else
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Applies then
         DPE (PC_Emv2_Contained_Property_Association,
         Expected_Token => T_Applies);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_To then
         DPE (PC_Emv2_Contained_Property_Association,
         Expected_Token => T_To);
         return No_Node;
      end if;

      Emv2_Containment_Path_List := New_List (K_List_Id,
      Token_Location);
      loop
         Save_Lexer (Loc_Start);
         Emv2_Containment_Path := P_Emv2_Containment_Path;
         Save_Lexer (Loc_End);
         if Present (Emv2_Containment_Path) then
            Append_Node_To_List (Emv2_Containment_Path,
            Emv2_Containment_Path_List);
         else
            if Loc_Start = Loc_End then
               Scan_Token;
               DPE (PC_Emv2_Containment_Path,
               Expected_Token => T_Identifier);
            end if;
            return No_Node;
         end if;

         Save_Lexer (Loc);
         Next := Next_Token;
         exit when Next /= T_Comma;
         Scan_Token; -- ','
      end loop;

      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Emv2_Contained_Property_Association,
         Expected_Token => T_Semi_Colon);
         return No_Node;
      end if;

      return Emv2_Contained_Property_Association;
   end  P_Emv2_Contained_Property_Association;

   -----------------------------
   -- P_Emv2_Containment_Path --
   -----------------------------

   --  emv2_containment_path ::=
   --    [ aadl2_core_path ] emv2_annex_specific_path

   --  aadl2_core_path | emv2_annex_specific_path ::=
   --    named_element_identifier { . named_element_identifier }*

   function P_Emv2_Containment_Path return Node_Id is
      Emv2_Containment_Path : Node_Id;
      Aadl2_Core_Path_List : List_Id;
      Emv2_Annex_Specific_Path_List : List_Id;
      Identifier : Node_Id;

      Loc_Start, Loc : Location;
      Next : Token_Type;
      Both_Lists : Boolean := False;
   begin
      Emv2_Containment_Path := New_Node
      (K_Emv2_Containment_Path, Token_Location);

      --  Between the 2 lists we must find a missing dot
      --  else it's emv2_annex_specific_path_list

      Save_Lexer (Loc_Start);

      Identifier := P_Identifier;
      if Present (Identifier) then
         Emv2_Annex_Specific_Path_List := New_List
         (K_List_Id, Token_Location);
         Append_Node_To_List (Identifier,
         Emv2_Annex_Specific_Path_List);
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Dot then
            Scan_Token;
            loop
               Identifier := P_Identifier;
               if Present (Identifier) then
                  Append_Node_To_List (Identifier,
                  Emv2_Annex_Specific_Path_List);
               else
                  DPE (PC_Emv2_Containment_Path,
                  Expected_Token => T_Identifier);
                  return No_Node;
               end if;
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               exit when Next /= T_Dot;
               Scan_Token;
            end loop;
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            if Next = T_Identifier then
               Both_Lists := True;
            end if;
         elsif Next = T_Identifier then
            Both_Lists := True;
         end if;
      else
         return No_Node;
      end if;

      if Both_Lists then
         Restore_Lexer (Loc_Start);
         --  Reset the list
         Emv2_Annex_Specific_Path_List := No_List;

         Aadl2_Core_Path_List := New_List (K_List_Id,
         Token_Location);
         Set_Aadl2_Core_Path_List (Emv2_Containment_Path,
         Aadl2_Core_Path_List);
         loop
            Identifier := P_Identifier;
            if Present (Identifier) then
               Append_Node_To_List (Identifier,
               Aadl2_Core_Path_List);
            else
               Scan_Token;
               DPE (PC_Emv2_Containment_Path,
               Expected_Token => T_Identifier);
               return No_Node;
            end if;
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            exit when Next /= T_Dot;
            Scan_Token;
         end loop;
         Save_Lexer (Loc);
         Next := Next_Token;
         Restore_Lexer (Loc);
         if Next = T_Identifier then
            Emv2_Annex_Specific_Path_List :=
            New_List (K_List_Id, Token_Location);
            Set_Emv2_Annex_Specific_Path_List (Emv2_Containment_Path,
            Emv2_Annex_Specific_Path_List);
            loop
               Identifier := P_Identifier;
               if Present (Identifier) then
                  Append_Node_To_List (Identifier,
                  Emv2_Annex_Specific_Path_List);
               else
                  Scan_Token;
                  DPE (PC_Emv2_Containment_Path,
                  Expected_Token => T_Identifier);
                  return No_Node;
               end if;
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               exit when Next /= T_Dot;
               Scan_Token;
            end loop;
         else
            Scan_Token;
            DPE (PC_Emv2_Containment_Path,
            Expected_Token => T_Identifier);
            return No_Node;
         end if;
      end if;

      return Emv2_Containment_Path;
   end P_Emv2_Containment_Path;

   ---------------------------
   -- P_Property_Identifier --
   ---------------------------

   --  Once the lexer finds 'EMV2::' or 'MILSTD882'
   --  or 'ARP4761'
   --  it stores it as a property set and
   --  moves to the property name to store it

   function P_Property_Identifier return Node_Id is
      Identifier    : Node_Id;

      Loc_Start     : Location;
      Property_Set  : Node_Id;
      Property_Set_Name_Id : Name_Id;
      Property_Name : Node_Id;
      Prop_Set, Prop_Name : Token_Type;
   begin
      Save_Lexer (Loc_Start);
      Scan_Property_Identifier (Prop_Set, Property_Set_Name_Id);
      Prop_Name := Token;

      if Prop_Set = T_Error or else Prop_Name = T_Error
      then
         return No_Node;
      elsif Token = T_Error_Property_Id then
         Restore_Lexer (Loc_Start);
         Scan_Token;
         return No_Node;
      else
         Property_Set := New_Node (K_Identifier, Token_Location);
         Set_Name (Property_Set, Property_Set_Name_Id);

         Property_Name := New_Node (K_Identifier, Token_Location);
         Set_Name (Property_Name, Token_Name);

         Identifier := New_Node (K_Property_Identifier,
         Token_Location);
         Set_Property_Set (Identifier, Property_Set);
         Set_Property_Name (Identifier, Property_Name);
         return Identifier;
      end if;

   end P_Property_Identifier;

   ------------------
   -- P_Assignment --
   ------------------

   --  assignment ::=
   --   ( ([ {Property_Name => Property_Value ;} + ]) | Property_Value )
   --   [ in_modes ]

   --  in_modes ::=
   --    in modes ( {modes_name}+ )

   --  http://repository.cmu.edu/cgi/viewcontent.cgi?article=1808&context=sei
   --  it indicates that '()' are used in the syntax of assignment
   --  if we have many properties
   --  '()' is used with the type record

   function P_Assignment return Node_Id is
      Assignment : Node_Id;

      Prop_Value               : Node_Id;
      Prop_Value_List          : List_Id := No_List;
      Prop_Name_List           : List_Id := No_List;
      In_Modes_Properties      : List_Id := No_List;

      Loc                      : Location;
      Loc_Start, Loc_End       : Location;
      Identifier : Node_Id; --  the name of the property in a record
      Parse_List_Of_Properties : boolean := False;
      Delimiter : Token_Type;
   begin
      Assignment := New_Node (K_Assignment, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Paren then
         Scan_Token;
         if Token = T_Left_Bracket then
            Parse_List_Of_Properties := True;
            Delimiter := T_Right_Paren;
         else
            DPE (PC_Assignment, Expected_Token => T_Left_Bracket);
            return No_Node;
         end if;
      elsif Token = T_Left_Bracket then
         Parse_List_Of_Properties := True;
         Delimiter := T_Right_Bracket;
      end if;

      if Parse_List_Of_Properties then

         Prop_Name_List := New_List (K_List_Id, Token_Location);
         Prop_Value_List := New_List (K_List_Id, Token_Location);

         loop
            Identifier := P_Identifier;
            if Present (Identifier) then
               Append_Node_To_List (Identifier, Prop_Name_List);
            else
               exit;
            end if;

            --  '=>'
            Scan_Token;
            if Token /= T_Association then
               DPE (PC_Assignment, Expected_Token => T_Association);
               return No_Node;
            end if;

            Save_Lexer (Loc_Start);
            Prop_Value := P_Property_Value;
            Save_Lexer (Loc_End);
            if Present (Prop_Value) then
               Integer_Literal_Value := Integer_Literal_Value + 1;
               Append_Node_To_List (Prop_Value, Prop_Value_List);
            else
               if Loc_Start = Loc_End then
                  DPE (PC_Assignment, PC_Property_Value);
               end if;
               return No_Node;
            end if;

            --  ';'
            Scan_Token;
            if Token /= T_Semi_Colon then
               DPE (PC_Assignment, Expected_Token => T_Semi_Colon);
               return No_Node;
            end if;

            Save_Lexer (Loc);
            Scan_Token;
            exit when Token = T_Right_Bracket;
            Restore_Lexer (Loc);
         end loop;

         if Is_Empty (Prop_Name_List) then
            DPE (PC_Assignment, Expected_Token => T_Property_Name);
            return No_Node;
         end if;

         if Token /= T_Right_Bracket then
            DPE (PC_Assignment, Expected_Token => T_Right_Bracket);
            return No_Node;
         end if;

         if Delimiter = T_Right_Paren then
            Scan_Token;
            if Token /= Delimiter then
               DPE (PC_Assignment, Expected_Token => T_Right_Paren);
               return No_Node;
            end if;
         end if;

         Set_Prop_Name_List (Assignment, Prop_Name_List);
         Set_Prop_Value_List (Assignment, Prop_Value_List);
      else
         Restore_Lexer (Loc);
         Save_Lexer (Loc_Start);
         Prop_Value := P_Property_Value;
         Save_Lexer (Loc_End);
         if No (Prop_Value) then
            if Loc_Start = Loc_End then
               DPE (PC_Assignment, PC_Property_Value);
            end if;
            return No_Node;
         end if;
      end if;

      --  Parse In_Modes

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_In then
         In_Modes_Properties := P_In_Modes (PC_Assignment);
         if Present (In_Modes_Properties) then
            Set_In_Modes_Properties (Assignment,
            In_Modes_Properties);
         else
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      return Assignment;

   end P_Assignment;

   ----------------
   -- P_In_Modes --
   ----------------

   function P_In_Modes (Code : Parsing_Code) return List_Id
   is
      Mode_Name_List : List_Id;
      Mode_Name : Node_Id;

      Next : Token_Type;
      Loc : Location;
   begin
      Scan_Token;
      if Token /= T_Modes then
         DPE (Code, Expected_Token => T_Modes);
         return No_List;
      end if;

      --  ( mode_name { , mode_name } * )
      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Left_Paren then
         Scan_Token;
         Mode_Name_List := New_List (K_List_Id, Token_Location);
         Mode_Name := P_Identifier;
         if Present (Mode_Name) then
            Append_Node_To_List (Mode_Name, Mode_Name_List);
            loop
               Save_Lexer (Loc);
               Next := Next_Token;
               Restore_Lexer (Loc);
               exit when Next /= T_Comma;
               Scan_Token;
               Mode_Name := P_Identifier;
               if Present (Mode_Name) then
                  Append_Node_To_List (Mode_Name, Mode_Name_List);
               else
                  Scan_Token;
                  DPE (Code, Expected_Token => T_Mode_Name);
                  return No_List;
               end if;
            end loop;
            --  ')'
            Scan_Token;
            if Token /= T_Right_Paren then
               DPE (Code, Expected_Token => T_Right_Paren);
               return No_List;
            end if;
         else
            Scan_Token;
            DPE (Code, Expected_Token => T_Mode_Name);
            return No_List;
         end if;
      else
         Scan_Token;
         DPE (Code, Expected_Token => T_Left_Paren);
         return No_List;
      end if;

      return Mode_Name_List;
   end P_In_Modes;

   ----------------------
   -- P_Property_Value --
   ----------------------

   --  Property_Value ::= enumeration | aadlstring
   --     | aadlreal | list of aadlstring | aadlinteger
   --     | range of integer with unit

   --  list of aadlstring ::=
   --      ( aadlstring {, aadlstring}* )

   function P_Property_Value return Node_Id is
      Property              : Node_Id := No_Node;
      Property_With_Unit    : Node_Id;
      List_Of_Strings       : List_Id;
      String_Item           : Node_Id;
      Loc, Loc_Inter, Loc_1 : Location;
   begin
      Save_Lexer (Loc);
      Scan_Token;
      case Token is
         when T_Real_Literal | T_Integer_Literal | T_Plus | T_Minus =>
            Restore_Lexer (Loc);

            --  The time range may have a unit or not
            --  the numeric value cannot have a unit
            Loc_Inter := Loc;
            Property_With_Unit := P_Numeric_Term (True);
            Scan_Token;
            Restore_Lexer (Loc_Inter);
            if Token = T_Interval and then Present (Property_With_Unit)
            then
               Property := P_Time_Range;
            else
               Property := P_Numeric_Term (False);
               Save_Lexer (Loc_1);
               Scan_Token;
               if Token = T_Interval and then Present (Property)
               then
                  Restore_Lexer (Loc_Inter);
                  Property := P_Time_Range;
               else
                  Restore_Lexer (Loc_1);
               end if;
            end if;

            if No (Property) then
               return No_Node;
            else
               return Property;
            end if;

         when T_String_Literal =>
            Property := New_Node (K_Literal, Token_Location);
            Set_Value (Property, New_String_Value (String_Literal_Value));
            return Property;

         when T_Identifier =>
            Property := New_Node (K_Literal, Token_Location);
            Set_Value (Property, New_Enum_Value (Token_Name));
            return Property;

         when T_Left_Paren =>
            Property := New_Node (K_Property_Value, Token_Location);
            List_Of_Strings := New_List (K_List_Id, Token_Location);
            loop
               Scan_Token;
               if Token = T_String_Literal then
                  String_Item := New_Node (K_Literal, Token_Location);
                  Set_Value (String_Item,
                  New_String_Value (String_Literal_Value));
                  Append_Node_To_List (String_Item, List_Of_Strings);
               else
                  DPE (PC_Property_Value, Expected_Token => T_AADLString);
                  return No_Node;
               end if;
               Scan_Token;
               exit when Token /= T_Comma;
            end loop;
            if Is_Empty (List_Of_Strings) or else Token = T_Comma
            then
               return No_Node;
            else
               Set_List_Of_Strings (Property, List_Of_Strings);
               if Token /= T_Right_Paren then
                  DPE (PC_Property_Value, Expected_Token => T_Right_Paren);
                  return No_Node;
               else
                  return Property;
               end if;
            end if;

         when others =>
            return No_Node;
      end case;
   end P_Property_Value;

   ------------------
   -- P_Time_Range --
   ------------------

   function P_Time_Range return Node_Id is
      Range_Type  : Node_Id;
      Lower_Bound : Node_Id;
      Upper_Bound : Node_Id;
   begin
      Range_Type := New_Node (K_Range_Type, Token_Location);

      Lower_Bound := P_Numeric_Term (True);
      if No (Lower_Bound) then
         DPE (PC_Time_Range, Expected_Token => T_AADLInteger);
         return No_Node;
      elsif Token /= T_Integer_Literal and then Token /= T_Identifier
      then
         DPE (PC_Time_Range, Expected_Token => T_AADLInteger);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Interval then
         DPE (PC_Time_Range, Expected_Token => T_Interval);
         return No_Node;
      end if;

      Upper_Bound := P_Numeric_Term (True);
      if No (Upper_Bound) then
         DPE (PC_Time_Range, Expected_Token => T_AADLInteger);
         return No_Node;
      elsif Token /= T_Integer_Literal and then Token /= T_Identifier
      then
         DPE (PC_Time_Range, Expected_Token => T_AADLInteger);
         return No_Node;
      end if;

      Set_Lower_Bound (Range_Type, Lower_Bound);
      Set_Upper_Bound (Range_Type, Upper_Bound);

      return Range_Type;
   end P_Time_Range;

   --------------------
   -- P_Numeric_Term --
   --------------------

   --  signed_aadlreal_or_constant ::=
   --      ( signed_aadlreal | [ sign ] real_property_constant_term )

   --  signed_aadlinteger_or_constant ::=
   --      ( signed_aadlinteger | [ sign ] integer_property_constant_term )

   --  sign ::= + | -

   --  signed_aadlinteger ::=
   --      [ sign ] integer_literal  [ unit_identifier ]

   --  signed_aadlreal ::=
   --      [ sign ] real_literal [ unit_identifier ]

   function P_Numeric_Term (Exist_Unit : in Boolean) return Node_Id is
   begin
      Scan_Token;
      case Token is

         when T_Plus | T_Minus =>
            Scan_Numeric_Literal_Value;

            case Token is
               when T_Real_Literal
                 | T_Integer_Literal =>
                  return P_AADLNumber (Exist_Unit);

               when others =>
                  DPE (PC_Property_Value);
                  return No_Node;
            end case;

         when T_Real_Literal
           | T_Integer_Literal =>
            return P_AADLNumber (Exist_Unit);

         when others =>
            return No_Node;
      end case;

   end P_Numeric_Term;

   ------------------
   -- P_AADLNumber --
   ------------------

   function P_AADLNumber (Exist_Unit : Boolean) return Node_Id
   is
      Numeric_Term  : Node_Id;       --  output
      Number_Value  : Node_Id;
      Unit_Ident    : Node_Id;
      Unitable      : Boolean := False;
   begin
      Numeric_Term := New_Node (K_Numeric_Term, Token_Location);

      case Token is
         when T_Real_Literal =>
            Number_Value := New_Node (K_AADL_Real, Token_Location);

         when T_Integer_Literal =>
            Number_Value := New_Node (K_AADL_Integer, Token_Location);

         when others =>
            return No_Node;
      end case;

      --  if we have a T_Integer_Literal or T_Real_Literal
      if Real_Literal_Value /= 0.0 then
         Set_Value (Number_Value,
                    New_Real_Value (Real_Literal_Value,
                                    False,
                                    Numeric_Literal_Base,
                                    Numeric_Literal_Exp));
      elsif Integer_Literal_Value /= 0 then
         Set_Value (Number_Value,
                    New_Integer_Value (Integer_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));
      end if;
      Unitable := Exist_Unit;

      if Unitable then
         --  try to parse unit_identifier
         Unit_Ident := P_Identifier;
      else
         --  no unit for property_constant_term
         Unit_Ident := No_Node;
      end if;

      Set_Number_Value (Numeric_Term, Number_Value);
      Set_Unit_Identifier (Numeric_Term, Unit_Ident);

      return Numeric_Term;
   end P_AADLNumber;

   ------------------
   -- P_Identifier --
   ------------------

   --  This function manages the pointer position

   function P_Identifier return Node_Id is
      Identifier : Node_Id;
      Loc : Location;
   begin
      Save_Lexer (Loc);
      if Next_Token /= T_Identifier then
         Restore_Lexer (Loc);
         return No_Node;
      end if;
      Restore_Lexer (Loc);
      Scan_Token;
      Identifier := New_Node (K_Identifier, Token_Location);
      Set_Name (Identifier, Token_Name);
      return Identifier;
   end P_Identifier;

   ---------------------------
   -- P_Composed_Identifier --
   ---------------------------

   --  [ identifiant1 :: ] identifiant2

   procedure P_Composed_Identifier
   (Identifier_1 : out  Node_Id; Identifier_2 : out Node_Id;
    Parent_Node : in out Node_Id; Code : in Parsing_Code) is
      Inter_Node_For_Affectation : Node_Id;
      Next : Token_Type;
      Loc : Location;
   begin
      Inter_Node_For_Affectation := P_Identifier;
      if No (Inter_Node_For_Affectation) then
         Scan_Token;
         DPE (Code, Expected_Token => T_Identifier);
         Parent_Node := No_Node;
      end if;

      Save_Lexer (Loc);
      Next := Next_Token;
      Restore_Lexer (Loc);
      if Next = T_Colon_Colon then
         Scan_Token;
         Identifier_1 := Inter_Node_For_Affectation;
         Identifier_2 := P_Identifier;
         if No (Identifier_2) then
            Scan_Token;
            DPE (Code, Expected_Token => T_Identifier);
            Parent_Node := No_Node;
         end if;
      else
         Identifier_2 := Inter_Node_For_Affectation;
         Identifier_1 := No_Node;
      end if;
   end P_Composed_Identifier;

   --------------------------------------
   -- P_Composed_Referenced_Identifier --
   --------------------------------------

   --  [ error_model_library_reference :: ] id
   --  or [ package_reference :: ] id

   procedure P_Composed_Referenced_Identifier
   (List_Identifier : out  List_Id; Identifier_2 : out Node_Id;
    Parent_Node : in out Node_Id; Code : in Parsing_Code)
   is
      Node_1 : Node_Id;
      Node_2 : Node_Id;
      Next : Token_Type;
      Loc, Loc_Start : Location;
      Id_Package : Node_Id;
   begin
      Node_1 := P_Identifier;
      if No (Node_1) then
         Scan_Token;
         DPE (Code, Expected_Token => T_Identifier);
         Parent_Node := No_Node;
      end if;

      Save_Lexer (Loc_Start);
      Next := Next_Token;
      Restore_Lexer (Loc_Start);
      if Next = T_Colon_Colon then
         Scan_Token;
         Node_2 := P_Identifier;
         if No (Node_2) then
            Scan_Token;
            DPE (Code, Expected_Token => T_Identifier);
            Parent_Node := No_Node;
         else
            Save_Lexer (Loc);
            Next := Next_Token;
            Restore_Lexer (Loc);
            List_Identifier := New_List (K_List_Id,
                               Token_Location);
            if Next = T_Colon_Colon then
               Restore_Lexer (Loc_Start);
               --  The package name contains more than one
               --  identifier
               Append_Node_To_List (Node_1, List_Identifier);
               Id_Package := P_Identifier;
               Append_Node_To_List (Id_Package, List_Identifier);
               Scan_Token;
               while Token = T_Colon_Colon loop
                  Id_Package := P_Identifier;
                  if No (Id_Package) then
                     Scan_Token;
                     DPE (Code, Expected_Token => T_Identifier);
                     Parent_Node := No_Node;
                     exit;
                  else
                     Save_Lexer (Loc);
                     Scan_Token;
                     if Token /= T_Colon_Colon then
                        Restore_Lexer (Loc);
                        Identifier_2 := Id_Package;
                        exit;
                     else
                        Append_Node_To_List (Id_Package, List_Identifier);
                     end if;
                  end if;
               end loop;
            else
               Append_Node_To_List (Node_1, List_Identifier);
               Identifier_2 := Node_2;
            end if;
         end if;
      else
         Identifier_2 := Node_1;
         List_Identifier := No_List;
      end if;

   end P_Composed_Referenced_Identifier;

end Ocarina.FE_AADL_EMA.Parser;
