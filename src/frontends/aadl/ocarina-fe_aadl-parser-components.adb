------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L . P A R S E R . C O M P O N E N T S     --
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

--  This package gathers all the functions that are related to
--  component parsing.

with Locations; use Locations;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Namet;

with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Lexer;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Annexes;
with Ocarina.FE_AADL.Parser.Components.Connections;
with Ocarina.FE_AADL.Parser.Components.Flows;
with Ocarina.FE_AADL.Parser.Components.Features;
with Ocarina.FE_AADL.Parser.Components.Subcomponents;
with Ocarina.FE_AADL.Parser.Components.Modes;
with Ocarina.FE_AADL.Parser.Components.Subprogram_Calls;
with Ocarina.FE_AADL.Parser.Components.Prototypes;
with Ocarina.FE_AADL.Parser.Properties;

with Ocarina.Builder.AADL.Components;

package body Ocarina.FE_AADL.Parser.Components is

   function P_Expected_Component_Implementation_Name
     (Expected_Id : Node_Id) return Boolean;

   function P_Component_Implementation
     (Namespace           : Ocarina.Types.Node_Id;
      Start_Loc           : Location;
      Category            : Component_Category;
      Private_Declaration : Boolean) return Node_Id;
   --  Parse Component_Implementation, Component_Implementation_Extension

   function P_Component_Type
     (Namespace           : Ocarina.Types.Node_Id;
      Start_Loc           : Location;
      Category            : Component_Category;
      Private_Declaration : Boolean) return Node_Id;
   --  Parse Component_Type, Component_Type_Extension

   function P_Unique_Component_Impl_Name return Node_Id;

   function Is_A_Component_Implementation_Name
     (Identifier : Ocarina.Types.Node_Id) return Boolean;
   --  Check if the identifier name contains a T_Dot

   ----------------------------------------------
   -- P_Expected_Component_Implementation_Name --
   ----------------------------------------------

   function P_Expected_Component_Implementation_Name
     (Expected_Id : Node_Id) return Boolean
   is
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Namet;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      Loc2, Start_Loc : Location;
      Node : constant Node_Id := New_Node (K_Identifier, Token_Location);
   begin
      Set_Corresponding_Entity (Node, No_Node);

      Save_Lexer (Start_Loc);
      Scan_Token;
      if Token = T_Identifier then
         Set_Name (Node, Token_Name);
         Set_Display_Name (Node, Token_Display_Name);
      else
         Restore_Lexer (Start_Loc);
         return False;
      end if;

      Save_Lexer (Loc2);
      Scan_Token;
      if Token = T_Dot then
         Get_Name_String (Name (Node));
         Add_Str_To_Name_Buffer (Image (T_Dot));
         Set_Name (Node, Name_Find);
         Get_Name_String (Display_Name (Node));
         Add_Str_To_Name_Buffer (Image (T_Dot));
         Set_Display_Name (Node, Name_Find);
      else
         Restore_Lexer (Start_Loc);
         return False;
      end if;

      Scan_Token;
      if Token = T_Identifier then
         Get_Name_String (Name (Node));
         Get_Name_String_And_Append (Token_Name);
         Set_Name (Node, Name_Find);
         Get_Name_String (Display_Name (Node));
         Get_Name_String_And_Append (Token_Display_Name);
         Set_Display_Name (Node, Name_Find);
      else
         Restore_Lexer (Start_Loc);
         return False;
      end if;

      if Name (Node) = Name (Expected_Id) then
         return True;
      else
         DPE (PC_Defining_Identifier, Display_Name (Expected_Id));
         Restore_Lexer (Start_Loc);
         return False;
      end if;
   end P_Expected_Component_Implementation_Name;

   -----------------
   -- P_Component --
   -----------------

   function P_Component
     (Namespace           : Ocarina.Types.Node_Id;
      Private_Declaration : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.Tokens;
      use Lexer;

      Category  : Component_Category;
      Loc       : Location;
      Start_Loc : Location;

   begin
      Start_Loc := Token_Location;
      Category  := P_Component_Category;
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Implementation then
         return P_Component_Implementation
             (Namespace,
              Start_Loc,
              Category,
              Private_Declaration);
      else
         Restore_Lexer (Loc);
         return P_Component_Type
             (Namespace,
              Start_Loc,
              Category,
              Private_Declaration);
      end if;
   end P_Component;

   --------------------------
   -- P_Component_Category --
   --------------------------

   --  AADL v1.0
   --  component_category ::= software_category | platform_category
   --                                    | composite_category
   --  software_category  ::= data | subprogram | thread | thread group
   --                                    | process
   --  platform_category  ::= memory | processor | bus | device
   --  composite_category ::= system

   --  AADL v2.0
   --  component_category ::= abstract_category | software_category
   --                         | platform_category | composite_category
   --  abstract_category  ::= abstract
   --  software_category  ::= data | subprogram | subprogram group | thread
   --                         | thread group | process
   --  platform_category  ::= memory | processor | bus | device
   --                         | virtual processor | virtual bus
   --  composite_category ::= system

   function P_Component_Category return Component_Category is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc : Location;

   begin
      case Token is
         when T_Abstract =>
            return CC_Abstract;

         when T_Data =>
            return CC_Data;

         when T_Subprogram =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Group then
               return CC_Subprogram_Group;
            else
               Restore_Lexer (Loc);
               return CC_Subprogram;
            end if;

         when T_Thread =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Group then
               return CC_Thread_Group;
            else
               Restore_Lexer (Loc);
               return CC_Thread;
            end if;

         when T_Process =>
            return CC_Process;

         when T_Memory =>
            return CC_Memory;

         when T_Processor =>
            return CC_Processor;

         when T_Virtual =>
            Scan_Token;

            if Token = T_Processor then
               return CC_Virtual_Processor;
            elsif Token = T_Bus then
               return CC_Virtual_Bus;
            else
               return CC_Unknown;
            end if;

         when T_Bus =>
            return CC_Bus;

         when T_Device =>
            return CC_Device;

         when T_System =>
            return CC_System;

         when others =>
            DPE (PC_Component_Category);
            return CC_Unknown;
      end case;
   end P_Component_Category;

   --------------------------
   -- P_Component_Category --
   --------------------------

   function P_Component_Category (Container : Ocarina.Types.Node_Id)
                                 return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;

      pragma Unreferenced (Container);

      Comp_Cat : Node_Id;
      Category : Component_Category;

   begin
      Comp_Cat := New_Node (K_Component_Category, Token_Location);

      Scan_Token;
      Category := P_Component_Category;
      if Category = CC_Unknown then
         return No_Node;
      end if;

      Set_Category (Comp_Cat, Component_Category'Pos (Category));
      return Comp_Cat;
   end P_Component_Category;

   --------------------------------
   -- P_Component_Implementation --
   --------------------------------

   --  AADL_V1
   --  component_implementation ::=
   --     component_category implementation component_type_identifier .
   --                          defining_component_implementation_identifier
   --     [ refines type ( { feature_refinement }+ | none_statement ) ]
   --     [ subcomponents ( { subcomponent }+ | none_statement ) ]
   --     [ calls ( { subprogram_call_sequence }+ | none_statement ) ]
   --     [ connections ( { connection }+ | none_statement ) ]
   --     [ flows ( { flow_implementation | end_to_end_flow_spec }+ |
   --               none_statement ) ]
   --     [ modes ( { mode }+ { mode_transition }* | none_statement ) ]
   --     [ properties ( { property_association }+ | none_statement ) ]
   --     { annex_subclause }*
   --  end component_type_identifier .
   --                          defining_component_implementation_identifier ;

   --  component_implementation_extension ::=
   --     component_category implementation component_type_identifier .
   --                          defining_component_implementation_identifier
   --     extends unique_component_implementation_name
   --     [ refines type ( { feature_refinement }+ | none_statement ) ]
   --     [ subcomponents ( { subcomponent | subcomponent_refinement }+ |
   --                       none_statement ) ]
   --     [ calls ( { subprogram_call_sequence }+ | none_statement ) ]
   --     [ connections ( { connection | connection_refinement }+ |
   --                     none_statement ) ]
   --     [ flows ( { flow_implementation | flow_implementation_refinement |
   --                 end_to_end_flow_spec }+ | none_statement ) ]
   --     [ modes ( { mode | mode_refinement | mode_transition }+ |
   --               none_statement ) ]
   --     [ properties ( { property_association }+ | none_statement ) ]
   --     { annex_subclause }*
   --  end component_type_identifier .
   --                          defining_component_implementation_identifier ;

   --  AADL_V2
   --  component_implementation ::=
   --     component_category implementation
   --        defining_component_implementation_name
   --     [ prototypes ( ( prototype }+ | none_statement ) ]
   --     [ subcomponents ( { subcomponent }+ | none_statement ) ]
   --     [ calls ( { subprogram_call_sequence }+ | none_statement ) ]
   --     [ connections ( { connection }+ | none_statement ) ]
   --     [ flows ( { flow_implementation | end_to_end_flow_spec }+
   --               | none_statement ) ]
   --     [ modes ( { mode }+ { mode_transition }* | none_statement ) ]
   --     [ properties ( { property_association
   --                      | Contained_Property_Association }+
   --                    | none_statement ) ]
   --     { annex_subclause }*
   --  end defining_component_implementation_name ;

   --  component_implementation_name ::=
   --     component_type_identifier . component_implementation_identifier

   --  component_implementation_extension ::=
   --     component_category implementation
   --        defining_component_implementation_name
   --     extends unique_component_implementation_reference
   --     [ prototype_bindings ]
   --     [ prototypes ( ( prototype | prototype_refinement }+
   --                    | none_statement ) ]
   --     [ subcomponents ( { subcomponent | subcomponent_refinement }+
   --                       | none_statement ) ]
   --     [ calls ( { subprogram_call_sequence }+ | none_statement ) ]
   --     [ connections ( { connection | connection_refinement }+
   --                     | none_statement ) ]
   --     [ flows ( { flow_implementation
   --                 | flow_implementation_refinement
   --                 | end_to_end_flow_spec
   --                 | end_to_end_flow_spec_refinement }+
   --               | none_statement ) ]
   --     [ modes ( { mode | mode_transition }+ | none_statement ) ]
   --     [ properties ( { property_association
   --                      | contained_property_association }+
   --                    | none_statement ) ]
   --     { annex_subclause }*
   --  end   defining_component_implementation_name ;

   function P_Component_Implementation
     (Namespace           : Ocarina.Types.Node_Id;
      Start_Loc           : Location;
      Category            : Component_Category;
      Private_Declaration : Boolean) return Node_Id
   is
      use Ocarina.Namet;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.Builder.AADL.Components;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Parser.Identifiers;
      use Parser.Annexes;
      use Parser.Components.Connections;
      use Parser.Components.Flows;
      use Parser.Components.Features;
      use Parser.Components.Subcomponents;
      use Parser.Components.Prototypes;
      use Parser.Components.Modes;
      use Parser.Properties;
      use Parser.Components.Subprogram_Calls;

      Impl    : Node_Id;    --  output
      Loc     : Location;
      Success : Boolean;

      Comp_Identifier : Node_Id;    --  component_identifier
      Impl_Identifier : Node_Id;    --  implementation_identifier
      Parent          : Node_Id;
      Refinable       : Boolean;    --  Is Component_Implementation_Extension ?

      Code          : Parsing_Code := PC_Component_Implementation;
      Current_Annex : Node_Id;

      Nb_Items : Integer;
   begin
      Comp_Identifier := P_Identifier (No_Node);
      if No (Comp_Identifier) then
         DPE (PC_Component_Implementation, T_Identifier);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Dot then
         DPE (PC_Component_Implementation, T_Dot);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Impl_Identifier := P_Identifier (No_Node);
      if No (Impl_Identifier) then
         DPE (PC_Component_Implementation, T_Identifier);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Get_Name_String (Name (Comp_Identifier));
      Add_Str_To_Name_Buffer (Image (T_Dot));
      Add_Str_To_Name_Buffer (Get_Name_String (Name (Impl_Identifier)));
      Set_Name (Impl_Identifier, Name_Find);
      Get_Name_String (Display_Name (Comp_Identifier));
      Add_Str_To_Name_Buffer (Image (T_Dot));
      Add_Str_To_Name_Buffer
        (Get_Name_String (Display_Name (Impl_Identifier)));
      Set_Display_Name (Impl_Identifier, Name_Find);
      --  The implementation name is actually the concatenation of the
      --  type and the implementation. Thus we have a unique
      --  identifier.

      Impl :=
        Add_New_Component_Implementation
          (Start_Loc,
           Impl_Identifier,
           Namespace,
           Category,
           Is_Private => Private_Declaration);

      if Impl = No_Node then
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Set_Component_Type_Identifier (Impl, Comp_Identifier);
      Save_Lexer (Loc);
      Scan_Token;

      --  Component_Implementation or Component_Implementation_Extension ?

      if Token = T_Extends then
         Parent := P_Unique_Component_Impl_Name;

         if No (Parent) then
            --  Error when parsing Unique_Component_Implementation_Name, quit
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
         Refinable := True;
         Code      := PC_Component_Implementation_Extension;
      else
         Restore_Lexer (Loc);
         Parent    := No_Node;
         Refinable := False;
      end if;

      --  Parse component implementation elements

      --
      --  Prototypes Bindings
      --

      if AADL_Version = AADL_V2 and then Refinable then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Left_Parenthesis then
            Success := P_Prototype_Bindings (Impl);

            if not Success then
               DPE (Code);
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      --
      --  Prototypes
      --

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Prototypes then
         Nb_Items :=
           P_Items_List
             (P_Prototype_Or_Prototype_Refinement'Access,
              Impl,
              Refinable,
              PC_Prototype_Or_Prototype_Refinement,
              True);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --
      --  Refines types, only AADL_V1
      --

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Refines and then AADL_Version = AADL_V1 then
         Scan_Token;     --  parse 'type'
         if Token /= T_Type then
            DPE (PC_Refines_Type, T_Type);
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

         Nb_Items :=
           P_Items_List (P_Feature_Refinement'Access, Impl, PC_Refines_Type);

         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

      elsif Token = T_Refines and then AADL_Version = AADL_V2 then
         DPE (Code, EMC_Not_Allowed_In_AADL_V2);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Subcomponents
      --

      if Token = T_Subcomponents then
         Nb_Items :=
           P_Items_List
             (P_Subcomponent'Access,
              Impl,
              Refinable,
              PC_Subcomponents);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Call sequences
      --

      if Token = T_Calls then
         Nb_Items :=
           P_Items_List
             (P_Subprogram_Call_Sequence'Access,
              Impl,
              PC_Subprogram_Call_Sequences);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Connections
      --

      if Token = T_Connections then
         Nb_Items :=
           P_Items_List (P_Connection'Access, Impl, Refinable, PC_Connections);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Flows
      --

      if Token = T_Flows then
         Nb_Items :=
           P_Items_List
             (P_Flow_Implementation_Or_End_To_End_Flow_Spec'Access,
              Impl,
              Refinable,
              PC_Flow_Implementations);

         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Modes
      --

      if Token = T_Modes or else Token = T_Requires then
         Nb_Items :=
           P_Items_List
             (Func         => P_Mode_Or_Mode_Transition'Access,
              Container    => Impl,
              Refinable    => Refinable,
              Code         => PC_Mode_Or_Mode_Transition,
              At_Least_One => True);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Properties
      --

      if Token = T_Properties then
         Nb_Items :=
           P_Items_List
             (P_Property_Association_In_Component_Implementation'Access,
              Impl,
              PC_Properties);

         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --
      --  Annexes
      --

      --  we make a loop, as there can be several annex declarations

      loop
         Scan_Token;

         if Token = T_Annex then
            Current_Annex := P_Annex_Subclause (Impl);
            if No (Current_Annex) then
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
            end if;

         elsif Token = T_End then
            exit;

         else
            DPE (Code);

            --  We skip all the tokens until finding a semicolon or an
            --  'end'. We do this only if the erronous token is
            --  different from 'semicolon'.

            if Token /= T_Semicolon then
               Skip_Tokens ((T_End, T_Semicolon));
            end if;

            return No_Node;
         end if;
      end loop;

      Save_Lexer (Loc);
      if not P_Expected_Component_Implementation_Name (Impl_Identifier) then
         Restore_Lexer (Loc);
         DPE (Code, Name (Impl_Identifier));
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;     --  parse ';'
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Parent (Impl, Parent);
      return Impl;
   end P_Component_Implementation;

   ----------------------
   -- P_Component_Type --
   ----------------------

   --  AADL_V1
   --  component_type ::=
   --     component_category defining_component_type_identifier
   --     [ features ( { feature }+ | none_statement ) ]
   --     [ flows ( { flow_spec }+ | none_statement ) ]
   --     [ properties ( { component_type_property_association }+ |
   --                    none_statement ) ]
   --     { annex_subclause }*
   --  end defining_component_type_identifier ;

   --  component_type_extension ::=
   --     component_category defining_component_type_identifier
   --     extends unique_component_type_identifier
   --     [ features ( { feature | feature_refinement }+ | none_statement ) ]
   --     [ flows ( { flow_spec | flow_spec_refinement }+ | none_statement ) ]
   --     [ properties ( { component_type_property_association }+ |
   --                    none_statement ) ]
   --     { annex_subclause }*
   --  end defining_component_type_identifier ;

   --  AADL_V2
   --  component_type ::=
   --     component_category defining_component_type_identifier
   --     [ prototypes ( ( prototype }+ | none_statement ) ]
   --     [ features ( { feature }+ | none_statement ) ]
   --     [ flows ( { flow_spec }+ | none_statement ) ]
   --     [ modes ( { mode }+ { mode_transition }* | none_statement ) ]
   --     [ properties ( { component_type_property_association
   --                      | contained_property_association }+
   --                    | none_statement ) ]
   --    { annex_subclause }*
   --  end defining_component_type_identifier ;

   --  component_type_extension ::=
   --     component_category defining_component_type_identifier
   --     extends unique_component_type_reference [ prototype_bindings ]
   --     [ prototypes ( ( prototype | prototype_refinement }+
   --                    | none_statement ) ]
   --     [ features ( { feature | feature_refinement }+ | none_statement ) ]
   --     [ flows ( { flow_spec | flow_spec_refinement }+ | none_statement ) ]
   --     [ modes ( { mode | mode_transition }+ | none_statement ) ]
   --     [ properties ( { component_type_property_association
   --                      | contained_property_association
   --                    | none_statement ) ]
   --    { annex_subclause }*
   --  end defining_component_type_identifier ;

   function P_Component_Type
     (Namespace           : Ocarina.Types.Node_Id;
      Start_Loc           : Location;
      Category            : Component_Category;
      Private_Declaration : Boolean) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Parser.Identifiers;
      use Parser.Annexes;
      use Parser.Components.Flows;
      use Parser.Components.Features;
      use Parser.Components.Prototypes;
      use Parser.Components.Modes;
      use Parser.Properties;
      use Ocarina.Builder.AADL.Components;

      Component : Node_Id;
      Loc       : Location;
      Success   : Boolean;

      Identifier : Node_Id;     --  component identifier
      Parent     : Node_Id;
      Refinable  : Boolean;     --  Is Component_Type_Extension ?

      Current_Annex : Node_Id;
      Code          : Parsing_Code := PC_Component_Type;

      Nb_Items : Integer;
   begin
      Identifier := P_Identifier (No_Node);
      if No (Identifier) then
         DPE (Code, T_Identifier);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Component :=
        Add_New_Component_Type
          (Start_Loc,
           Identifier,
           Namespace,
           Category,
           Is_Private => Private_Declaration);

      if No_Node = Component then
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --  Component_Type or Component_Type_Extension ?

      if Token = T_Extends then
         Parent := P_Entity_Reference (PC_Unique_Component_Type_Identifier);

         --         if Second_Id (Parent) /= No_Node then
         if Next_Node (First_Node (Path (Parent))) /= No_Node then
            Display_Parsing_Error
              (PC_Unique_Component_Type_Identifier,
               EMC_Extends_Incompatible_Entity);
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

         if No (Parent) then
            --  Error when parsing Unique_Component_Type_Identifier, quit
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
         Refinable := True;
         Code      := PC_Component_Type_Extension;
      else
         Restore_Lexer (Loc);
         Parent    := No_Node;
         Refinable := False;
      end if;

      --  Parse prototypes bindings

      if AADL_Version = AADL_V2 and then Refinable then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Left_Parenthesis then
            Success := P_Prototype_Bindings (Component);

            if not Success then
               DPE (Code);
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      --  Parse prototypes

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Prototypes then
         Nb_Items :=
           P_Items_List
             (Func         => P_Prototype_Or_Prototype_Refinement'Access,
              Container    => Component,
              Refinable    => Refinable,
              Code         => PC_Prototype_Or_Prototype_Refinement,
              At_Least_One => True);

         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Parse component elements

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Features
      --

      if Token = T_Features then
         Nb_Items :=
           P_Items_List
             (Func         => P_Feature'Access,
              Container    => Component,
              Refinable    => Refinable,
              Code         => PC_Features,
              At_Least_One => True);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Flow specifications
      --

      if Token = T_Flows then
         Nb_Items :=
           P_Items_List
             (Func         => P_Flow_Spec'Access,
              Container    => Component,
              Refinable    => Refinable,
              Code         => PC_Flow_Specifications,
              At_Least_One => True);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Modes, only in AADL_V2
      --

      if Token = T_Modes then
         Nb_Items :=
           P_Items_List
             (Func         => P_Mode_Or_Mode_Transition'Access,
              Container    => Component,
              Refinable    => Refinable,
              Code         => PC_Mode_Or_Mode_Transition,
              At_Least_One => True);
         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --
      --  Properties
      --

      if Token = T_Properties then
         Nb_Items :=
           P_Items_List
             (P_Property_Association'Access,
              Component,
              PC_Properties);

         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --
      --  Annexes
      --

      --  We make a loop, as there can be several annex declarations

      loop
         Scan_Token;

         if Token = T_Annex then
            Current_Annex := P_Annex_Subclause (Component);
            if not Present (Current_Annex) then
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
            end if;

         elsif Token = T_End then
            exit;

         else
            DPE (Code);

            --  We skip all the tokens until finding a semicolon or an
            --  'end'. We do this only if the erronous token is
            --  different from 'semicolon'.

            if Token /= T_Semicolon then
               Skip_Tokens ((T_End, T_Semicolon));
            end if;

            return No_Node;
         end if;
      end loop;

      if not P_Expected_Identifier (Identifier) then
         --  Error when parsing Defining_Identifier, quit
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Parent (Component, Parent);
      return Component;
   end P_Component_Type;

   ----------------------------------
   -- P_Unique_Component_Impl_Name --
   ----------------------------------

   --  unique_component_implementation_name ::=
   --    [package_name :: ] component_type_identifier .
   --                                component_implementation_identifier

   function P_Unique_Component_Impl_Name return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.Tokens;
      use Parser.Identifiers;

      Comp_Impl_Name : Node_Id;
   begin
      Comp_Impl_Name := P_Entity_Reference (PC_Unique_Component_Impl_Name);

      if No (Comp_Impl_Name) then
         --  Error when parsing Identifier_With_Package_Name, quit
         DPE (PC_Unique_Component_Impl_Name, T_Identifier);
         return No_Node;

      else
         --  We must ensure the name is something like
         --  [package::]type.implementation

         if Is_A_Component_Implementation_Name
             (Identifier (Comp_Impl_Name))
         then
            return Comp_Impl_Name;
         else
            DPE (PC_Unique_Component_Impl_Name, T_Identifier);
            return No_Node;
         end if;
      end if;
   end P_Unique_Component_Impl_Name;

   ----------------------------------------
   -- Is_A_Component_Implementation_Name --
   ----------------------------------------

   function Is_A_Component_Implementation_Name
     (Identifier : Ocarina.Types.Node_Id) return Boolean
   is
      use Ocarina.Namet;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.Tokens;

      pragma Assert
        (Identifier /= No_Node and then Kind (Identifier) = K_Identifier);

      Name : constant Name_Id :=
        Ocarina.ME_AADL.AADL_Tree.Nodes.Name (Identifier);
   begin
      Get_Name_String (Name);

      for Index in 1 .. Name_Len loop
         if Name_Buffer (Index .. Index) = Image (T_Dot) then
            return True;
         end if;
      end loop;

      return False;
   end Is_A_Component_Implementation_Name;

   --------------------------
   -- P_Feature_Group_Type --
   --------------------------

   --  AADL_V1
   --
   --  port_group_type ::= port group defining_identifier
   --     ( features { port_spec | port_group_spec }*
   --       [ inverse of unique_port_group_type_reference ]
   --     |
   --       inverse of unique_port_group_type_reference )
   --     [ properties
   --         ( { portgroup_property_association }+ | none_statement ) ]
   --     ( annex_subclause )*
   --  end defining_identifier ;

   --  port_group_type_extension ::= port group defining_identifier
   --     extends unique_port_group_type_reference
   --     ( features { port_spec | port_refinement |
   --                  port_group_spec | port_group_refinement }*
   --       [ inverse of unique_port_group_type_reference ]
   --     |
   --       inverse of unique_port_group_type_reference )
   --     [ properties
   --         ( { portgroup_property_association }+ | none_statement ) ]
   --     ( annex_subclause )*
   --  end defining_identifier ;

   --  AADL_V2
   --
   --  feature_group_type ::=
   --     feature group defining_identifier
   --     [ prototypes ( { prototype }+ | none_statement ) ]
   --     [ features { feature }*  ]
   --     [ inverse of unique_feature_group_type ]
   --     [ properties
   --         ( { featuregroup_property_association }+ | none_statement ) ]
   --     ( annex_subclause )*
   --  end defining_identifier ;

   --  feature_group_type_extension ::=
   --     feature group defining_identifier
   --     extends unique_feature_group_type_reference
   --     [ prototypes ( { prototype }+ | none_statement ) ]
   --     ( [ features
   --       { feature | feature_refinement }+
   --       [ inverse of unique_feature_group_type ] ]
   --     [ properties
   --         ( { featuregroup_property_association }+ | none_statement ) ]
   --     ( annex_subclause )*
   --  end defining_identifier ;

   function P_Feature_Group_Type
     (Namespace           : Ocarina.Types.Node_Id;
      Start_Loc           : Location;
      Private_Declaration : Boolean := False) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Annexes;
      use Ocarina.FE_AADL.Parser.Properties;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Components.Features;
      use Ocarina.FE_AADL.Parser.Components.Prototypes;
      use Ocarina.Builder.AADL.Components;

      Feature_Group_Type : Node_Id;
      Loc                : Location;

      Identifier : Node_Id;
      Inverse_Of : Node_Id := No_Node;
      Parent     : Node_Id;
      Refinable  : Boolean;

      Code          : Parsing_Code := PC_Feature_Group_Type;
      Current_Annex : Node_Id;
      Nb_Items      : Integer;
      Success       : Boolean;
   begin
      Scan_Token;

      if Token /= T_Identifier then
         DPE (Code, T_Identifier);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Identifier := New_Node (K_Identifier, Token_Location);
      Set_Name (Identifier, Token_Name);
      Set_Display_Name (Identifier, Token_Display_Name);

      Feature_Group_Type :=
        Add_New_Feature_Group
          (Loc        => Start_Loc,
           Name       => Identifier,
           Namespace  => Namespace,
           Is_Private => Private_Declaration);

      if Feature_Group_Type = No_Node then
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      --  Feature_Group_Type or Feature_Group_Type_Extension ?

      if Token = T_Extends then
         Parent := P_Entity_Reference (PC_Unique_Feature_Group_Type_Reference);

         if No (Parent) then
            --  Error when parsing Unique_Feature_Group_Type_Reference, quit
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
         Refinable := True;
         Code      := PC_Feature_Group_Type_Extension;
      else
         Restore_Lexer (Loc);
         Parent    := No_Node;
         Refinable := False;
      end if;

      --  Parse prototypes bindings

      if AADL_Version = AADL_V2 and then Refinable then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Left_Parenthesis then
            Success := P_Prototype_Bindings (Feature_Group_Type);

            if not Success then
               DPE (Code);
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      --  Parse prototype elements

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Prototypes then
         Nb_Items :=
           P_Items_List
             (Func         => P_Prototype_Or_Prototype_Refinement'Access,
              Container    => Feature_Group_Type,
              Refinable    => Refinable,
              Code         => PC_Prototype_Or_Prototype_Refinement,
              At_Least_One => True);

         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      --  Parse feature group elements

      Save_Lexer (Loc);
      Scan_Token;

      --  Features
      if Token = T_Features then
         Nb_Items :=
           P_Items_List
             (Func => P_Feature'Access,
              Container    => Feature_Group_Type,
              Refinable    => Refinable,
              Code         => Code,
              At_Least_One => False);

         if Nb_Items < 0 then
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Inverse then
         Scan_Token;
         if Token /= T_Of then
            DPE (Code, T_Of);
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

         Inverse_Of := P_Entity_Reference (Code);

         if No (Inverse_Of) then
            --  Error when parsing
            --  Unique_Feature_Group_Type_Reference, quit
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

      elsif AADL_Version = AADL_V1 then
         if Nb_Items < 0 then
            --   no features, no inverse_of, error

            DPE (Code, (T_Features, T_Inverse));
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

         Restore_Lexer (Loc);
      else
         Restore_Lexer (Loc);
      end if;

      loop
         Scan_Token;
         case Token is
            when T_Properties =>
               Nb_Items :=
                 P_Items_List
                   (Func      => P_Property_Association'Access,
                    Container => Feature_Group_Type,
                    Code      => PC_Properties);

               if Nb_Items <= 0 then
                  Skip_Tokens ((T_End, T_Semicolon));
                  return No_Node;
               end if;

            when T_Annex =>
               Current_Annex := P_Annex_Subclause (Feature_Group_Type);
               if not Present (Current_Annex) then
                  Skip_Tokens ((T_End, T_Semicolon));
                  return No_Node;
               end if;

            when T_End =>
               exit;

            when others =>
               if No (Inverse_Of) then
                  DPE (Code, (T_Inverse, T_Properties, T_Annex, T_End));
               else
                  DPE (Code, (T_Properties, T_Annex, T_End));
               end if;
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
         end case;
      end loop;

      if not P_Expected_Identifier (Identifier) then
         --  Error when parsing defining_identifier, quit
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Inverse_Of (Feature_Group_Type, Inverse_Of);
      Set_Parent (Feature_Group_Type, Parent);

      return Feature_Group_Type;
   end P_Feature_Group_Type;

end Ocarina.FE_AADL.Parser.Components;
