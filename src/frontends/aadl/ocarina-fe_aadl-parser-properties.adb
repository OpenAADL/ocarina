------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L . P A R S E R . P R O P E R T I E S     --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Property_Sets; use Ocarina.Property_Sets;

with Ocarina.FE_AADL.Lexer;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Namespaces;
with Ocarina.FE_AADL.Parser.Components;
with Ocarina.FE_AADL.Parser.Components.Modes;
with Ocarina.FE_AADL.Parser.Properties.Values;

with Ocarina.Builder.AADL.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties; use Ocarina.ME_AADL;
use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

package body Ocarina.FE_AADL.Parser.Properties is

   function P_Property_Definition_Declaration
     (Identifier   : Node_Id;
      Property_Set : Node_Id) return Node_Id;
   --  Current token is ':'

   function P_Property_Owner_Or_Category (Container : Node_Id) return Node_Id;

   function P_Property_Owner_Category return Node_Id;

   function P_Property_Value (Container : Node_Id) return Node_Id;

   ----------------------
   -- P_Property_Value --
   ----------------------

   --  property_value ::= single_property_value | property_list_value

   function P_Property_Value (Container : Node_Id) return Node_Id is
      pragma Unreferenced (Container);
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.Builder.AADL.Properties;

      use Ocarina.FE_AADL.Parser.Components.Modes;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Properties.Values;

      Loc                      : Location;
      Prop_Value               : Node_Id;

   begin
      --  Parse Property_Value

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Parenthesis then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Right_Parenthesis then
            --  Property_List_Value is empty
            Prop_Value := Node_Id (New_List (K_List_Id, Loc));
            Set_Kind (Prop_Value, K_Property_List_Value);
            Set_First_Node (List_Id (Prop_Value), No_Node);
            Set_Last_Node (List_Id (Prop_Value), No_Node);
         else
            Restore_Lexer (Loc);

            --  Prop_Value :=
            --  Node_Id (P_Items_List (P_Property_Value'Access,

            Prop_Value :=
              Node_Id
                (P_Items_List
                   (P_Property_Expression'Access,
                    No_Node,
                    T_Comma,
                    T_Right_Parenthesis,
                    PC_Property_List_Value));
            if No (Prop_Value) then
               --  error when parsing Property_Expression list, quit
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Set_Kind (Prop_Value, K_Property_List_Value);
         end if;

      else
         Restore_Lexer (Loc);
         Prop_Value := P_Property_Expression (No_Node);

         if No (Prop_Value) then
            --  error when parsing Property_Expression, quit
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      return Prop_Value;
   end P_Property_Value;

   ----------------------------
   -- P_Property_Association --
   ----------------------------

   function P_Property_Association (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.Tokens;

   begin
      if AADL_Version = AADL_V1 then
         return P_Property_Association
             (Container     => Container,
              Property_Type => PAT_Simple);

      else
         return P_Property_Association
             (Container     => Container,
              Property_Type => PAT_Simple_Or_Contained);
      end if;

   end P_Property_Association;

   --------------------------------------------------------
   -- P_Property_Association_In_Component_Implementation --
   --------------------------------------------------------

   function P_Property_Association_In_Component_Implementation
     (Container : Node_Id) return Node_Id
   is
   begin
      return P_Property_Association
          (Container     => Container,
           Property_Type => PAT_Simple_Or_Contained);
   end P_Property_Association_In_Component_Implementation;

   ----------------------------
   -- P_Property_Association --
   ----------------------------

   --  AADL_V1
   --  property_association ::=
   --     [ property_set_identifier :: ] property_name_identifier ( => | +=> )
   --     [ constant ] property_value [ in_binding ] [ in_modes ] ;

   --  access_property_association ::=
   --     [ property_set_identifier :: ] property_name_identifier ( => | +=> )
   --     [ constant ] access property_value [ in_binding ] [ in_modes ] ;

   --  contained_property_association ::=
   --     [ property_set_identifier :: ] property_name_identifier ( => | +=> )
   --     [ constant ] property_value
   --     applies to contained_unit_identifier { . contained_unit_identifier }*
   --     [ in_binding ] [ in_modes ] ;

   --  property_value ::= single_property_value | property_list_value

   --  single_property_value ::= property_expression

   --  property_list_value ::=
   --     ( [ property_expression { , property_expression }* ] )

   --  in_binding ::= in binding ( platform_classifier_reference
   --                                 { , platform_classifier_reference }* )

   --  platform_classifier_reference ::=   processor_classifier_reference
   --                                    | memory_classifier_reference
   --                                    | bus_classsifer_reference

   --  AADL_V2
   --  property_association ::=
   --     [ property_set_identifier :: ] property_name_identifier ( => | +=> )
   --     [ constant ] property_value [ in_binding ] [ in_modes ] ;

   --  contained_property_association ::=
   --     [ property_set_identifier :: ] property_name_identifier ( => | +=> )
   --     [ constant ] property_value
   --     applies to contained_model_element_path
   --                   { , contained_model_element_path }*
   --     [ in_binding ] [ in_modes ] ;

   --  contained_model_element_path ::=
   --     ( contained_model_element { . contained_model_element }*
   --       [ annex_path ] )
   --     | annex_path

   --  contained_model_element ::=
   --     named_element_identifier | named_element_array_selection_identifier

   --  annex_path ::=
   --     [ { annex_identifier } ] { ** model_element_identifier }+

   --  single_property_value ::= property_expression

   --  property_list_value ::=
   --     ( [ property_expression { , property_expression }* ] )

   --  in_binding ::=
   --    in binding ( platform_classifier_reference
   --                    { , platform_classifier_reference }* )

   --  platform_classifier_reference ::=
   --       processor_classifier_reference
   --     | virtual_processor_classifier_reference
   --     | bus_classifier_reference
   --     | virtual_bus_classifier_reference
   --     | memory_classifier_reference

   function P_Property_Association
     (Container     : Node_Id;
      Property_Type : Property_Association_Type) return Node_Id
   is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.Builder.AADL.Properties;

      use Ocarina.FE_AADL.Parser.Components.Modes;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Properties.Values;

      pragma Assert (Container /= No_Node);

      Property                 : Node_Id;   --  output
      Property_Name_Identifier : Node_Id;
      Contained_Elt            : Node_Id;
      Is_Additive              : Boolean;
      Is_Constant              : Boolean;
      Is_Access                : Boolean;
      Prop_Value               : Node_Id;
      Applies                  : List_Id := No_List;
      In_Binding               : Node_Id := No_Node;
      In_Modes                 : Node_Id := No_Node;
      Code                     : Parsing_Code;
      Binding_Loc              : Location;
      Loc                      : Location;
      Property_Loc             : Location;
      Item                     : Node_Id;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      Save_Lexer (Property_Loc);

      if Token /= T_Identifier then
         --  can not parse Property_Association, try another stuff
         Restore_Lexer (Loc);
         return No_Node;
      else
         Restore_Lexer (Loc);
      end if;

      case Property_Type is
         when PAT_Simple =>
            Code := PC_Property_Association;
         when PAT_Access =>
            Code := PC_Access_Property_Association;
         when PAT_Simple_Or_Contained =>
            Code := PC_Property_Association_Or_Contained_Property_Association;
      end case;

      Property_Name_Identifier := P_Entity_Reference (Code);

      if No (Property_Name_Identifier) then
         --  error when parsing identifiers, quit
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;

      if Token = T_Association then
         Is_Additive := False;
      elsif Token = T_Additive_Association then
         Is_Additive := True;
      else
         DPE (Code, (T_Association, T_Additive_Association));
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Constant then
         Is_Constant := True;
      else
         Restore_Lexer (Loc);
         Is_Constant := False;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Access then
         case AADL_Version is
            when AADL_V1 =>
               if Property_Type = PAT_Access then
                  Is_Access := True;
               else
                  DPE (Code, EMC_Access_Property_Association_Is_Not_Allowed);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            when AADL_V2 =>
               DPE (Code, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;
      else
         if AADL_Version = AADL_V1 and then Property_Type = PAT_Access then
            if Is_Constant then
               DPE (Code, T_Access);
            else
               DPE (Code, (T_Constant, T_Access));
            end if;

            Skip_Tokens (T_Semicolon);
            return No_Node;
         else
            Restore_Lexer (Loc);
            Is_Access := False;
         end if;
      end if;

      --  Parse Property_Value

      Prop_Value := P_Property_Value (Container);

      --  Parse 'applies to ...'

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Applies then
         if Property_Type /= PAT_Simple_Or_Contained then
            DPE (Code, EMC_Contained_Property_Association_Is_Not_Allowed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         --  now we know that Contained_Property_Association is being parsed
         Code := PC_Contained_Property_Association;

         Scan_Token;

         if Token /= T_To then
            DPE (Code, T_To);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         case AADL_Version is
            when AADL_V1 =>
               Save_Lexer (Loc);

               Contained_Elt := P_Contained_Element_Path (Container);

               if Contained_Elt /= No_Node then
                  Applies := New_List (K_List_Id, Loc);
                  Append_Node_To_List (Contained_Elt, Applies);
               else
                  DPE (Code, T_Identifier);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            when AADL_V2 =>
               Applies :=
                 P_Items_List
                   (P_Contained_Element_Path'Access,
                    Container,
                    T_Comma);
         end case;

         if No (Applies) then
            DPE (Code, T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         Applies := No_List;
      end if;

      --  Parse In_Binding

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_In then
         Save_Lexer (Binding_Loc);
         Scan_Token;

         if Token = T_Binding then
            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (Code, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            In_Binding := New_Node (K_In_Binding, Binding_Loc);
            Set_Binding (In_Binding, New_List (K_List_Id, Binding_Loc));

            loop
               Save_Lexer (Loc);
               Scan_Token;

               if Token = T_Right_Parenthesis then
                  Restore_Lexer (Loc);
                  exit;
               end if;

               Restore_Lexer (Loc);
               Item := P_Entity_Reference (Code);

               if Present (Item) then
                  Append_Node_To_List (Item, Binding (In_Binding));
               else
                  Skip_Tokens (T_Right_Parenthesis, False);
                  In_Binding := No_Node;
                  exit;
               end if;

               Scan_Token;

               if Token = T_Right_Parenthesis then
                  exit;
               end if;

               if Token /= T_Comma then
                  Skip_Tokens (T_Right_Parenthesis, False);
                  In_Binding := No_Node;
               end if;
            end loop;

            if No (In_Binding) then
               --  error when parsing In_Binding, quit
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         elsif Token = T_Modes then
            --  In_Modes will be parsed in next section
            Restore_Lexer (Loc);
            In_Binding := No_Node;

         else
            DPE (Code, (T_Binding, T_Modes));
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         In_Binding := No_Node;
      end if;

      --  Parse In_Modes

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_In then
         In_Modes := P_In_Modes (PC_In_Modes);

         if No (In_Modes) then
            --  error when parsing In_Modes, quit
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         In_Modes := No_Node;
      end if;

      --  Parse ';'

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      --  The actual name of the property association is the
      --  concatenation of its property set and its name inside
      --  this property set.

      declare
         Full_Name_Identifier : constant Node_Id :=
           New_Node (K_Identifier, Property_Loc);
         Prop_Set_Id : constant Node_Id :=
           Namespace_Identifier (Property_Name_Identifier);
         Prop_Name_Id : constant Node_Id :=
           Identifier (Property_Name_Identifier);
      begin
         if Prop_Set_Id /= No_Node then
            Get_Name_String (Name (Prop_Set_Id));
            Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
            Add_Str_To_Name_Buffer (Get_Name_String (Name (Prop_Name_Id)));
            Set_Name (Full_Name_Identifier, Name_Find);

            Get_Name_String
              (Ocarina.ME_AADL.AADL_Tree.Nodes.Display_Name (Prop_Set_Id));
            Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
            Add_Str_To_Name_Buffer
              (Get_Name_String (Display_Name (Prop_Name_Id)));
            Set_Display_Name (Full_Name_Identifier, Name_Find);
         else
            Get_Name_String (Name (Prop_Name_Id));
            Set_Name (Full_Name_Identifier, Name_Find);

            Get_Name_String (Display_Name (Prop_Name_Id));
            Set_Display_Name (Full_Name_Identifier, Name_Find);
         end if;

         Property :=
           Add_New_Property_Association
             (Loc =>
                Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Full_Name_Identifier),
              Name           => Full_Name_Identifier,
              Container      => Container,
              Property_Name  => Property_Name_Identifier,
              Is_Additive    => Is_Additive,
              Is_Constant    => Is_Constant,
              Is_Access      => Is_Access,
              Property_Value => Prop_Value,
              In_Binding     => In_Binding,
              Applies_To     => Applies,
              In_Modes       => In_Modes);
      end;

      pragma Assert (Property /= No_Node);
      --  Container may be No_Node if it is to be created later. Then
      --  the parser will parse again the property associations,
      --  giving the container. So, we just check the syntax this
      --  time.

      return Property;
   end P_Property_Association;

   -----------------------------
   -- P_Property_Associations --
   -----------------------------

   --     ( [ { { Property_Association }+ } ] )
   --  or (   { { Property_Association }+ }   )

   function P_Property_Associations
     (Container     : Node_Id;
      Optional      : Boolean;
      Property_Type : Property_Association_Type;
      Code          : Parsing_Code) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      pragma Assert (Container /= No_Node);

      Property        : Node_Id;
      Loc             : Location;
      Success         : Boolean := True;
      Number_Of_Items : Integer := 0;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Curly_Bracket then
         loop
            Save_Lexer (Loc);
            Property :=
              P_Property_Association
                (Container     => Container,
                 Property_Type => Property_Type);

            if No (Property) then
               if Token_Location = Loc then
                  --  Error when parsing the first token of
                  --  property_association, display error message

                  DPE (Code, (T_Identifier, T_Right_Curly_Bracket));
               end if;

               --  If the error is caused by a missing semi-colon and
               --  the property association we deal with is the last
               --  one, we must skip tokens until a '}', otherwise,
               --  we skip token until a ';'.

               if Token /= T_Right_Curly_Bracket then
                  Skip_Tokens (T_Semicolon);
               end if;

               Success := False;
            else
               Number_Of_Items := Number_Of_Items + 1;
            end if;

            Save_Lexer (Loc);
            Scan_Token;

            exit when Token = T_Right_Curly_Bracket or else not Success;
            Restore_Lexer (Loc);
         end loop;

         Success := Number_Of_Items /= 0 and then Success;
      else
         if not Optional then
            --  Property_Associations must be defined
            DPE (Code, T_Left_Curly_Bracket);
            Skip_Tokens (T_Semicolon);
            Success := False;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      return Success;
   end P_Property_Associations;

   ---------------------------------------
   -- P_Property_Definition_Declaration --
   ---------------------------------------

   --  AADL_V1
   --  property_name_declaration ::=
   --     defining_property_name_identifier : [ access ] [ inherit ]
   --        ( single_valued_property | multi_valued_property )
   --     applies to ( ( property_owner_category
   --                       { , property_owner_category }* | all ) ) ;

   --  AADL_V2
   --  property_definition_declaration ::=
   --     defining_property_name_identifier : [ inherit ]
   --        ( single_valued_property | multi_valued_property )
   --     applies to ( ( property_owner { , property_owner }* | all ) );

   function P_Property_Definition_Declaration
     (Identifier   : Node_Id;
      Property_Set : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Properties.Values;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.Builder.AADL.Properties;

      Is_Access                 : Boolean;
      Is_All                    : Boolean;
      Is_Inherit                : Boolean;
      Is_A_List                 : Boolean;
      Property                  : Node_Id;
      Single_Default_Value      : Node_Id;
      Property_Definition_Value : Node_Id;
      Multiple_Default_Value    : List_Id;
      Owner_Categories          : List_Id;
      Loc                       : Location;
      Multiplicity              : Int := 0;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Access then
         case AADL_Version is
            when AADL_V1 =>
               Is_Access := True;
            when AADL_V2 =>
               DPE
                 (PC_Property_Definition_Declaration,
                  EMC_Not_Allowed_In_AADL_V2);
         end case;
      else
         Is_Access := False;
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Inherit then
         Is_Inherit := True;
      else
         Is_Inherit := False;
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_List then
         Is_A_List    := True;
         Multiplicity := 1;

         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Of then
            Scan_Token;
            if Token /= T_List then
               Restore_Lexer (Loc);
            else
               Multiplicity := 2; -- XXX shall we iterate? BNF says yes
            end if;
         else
            Restore_Lexer (Loc);
         end if;

         Property_Definition_Value := P_Multi_Valued_Property;
         Single_Default_Value      := No_Node;

         if Property_Definition_Value /= No_Node then
            Multiple_Default_Value :=
              Property_Expressions (Property_Definition_Value);
         else
            Multiple_Default_Value := No_List;
         end if;

      else
         Restore_Lexer (Loc);
         Is_A_List                 := False;
         Property_Definition_Value := P_Single_Valued_Property;
         Multiple_Default_Value    := No_List;

         if Property_Definition_Value /= No_Node then
            Single_Default_Value :=
              Property_Expression (Property_Definition_Value);
         else
            Single_Default_Value := No_Node;
         end if;
      end if;

      if Property_Definition_Value = No_Node then
         --  error when parsing Single_Valued_Property, quit
         --  Note that a Multi_Valued_Property can be empty
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;

      if Token /= T_Applies then
         DPE (PC_Property_Definition_Declaration, T_Applies);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;

      if Token /= T_To then
         DPE (PC_Property_Definition_Declaration, T_To);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;

      if Token /= T_Left_Parenthesis then
         DPE (PC_Property_Definition_Declaration, T_Left_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_All then
         Is_All           := True;
         Owner_Categories := No_List;

         Scan_Token;

         if Token /= T_Right_Parenthesis then
            DPE (PC_Property_Definition_Declaration, T_Right_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Is_All := False;
         Restore_Lexer (Loc);
         Owner_Categories :=
           P_Items_List
             (P_Property_Owner_Or_Category'Access,
              No_Node,
              T_Comma,
              T_Right_Parenthesis,
              PC_Property_Definition_Declaration);
         if No (Owner_Categories) then
            --  error when parsing property_owner_category list, quit
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (PC_Property_Definition_Declaration, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Property :=
        Add_New_Property_Definition_Declaration
          (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Property_Set           => Property_Set,
           Name                   => Identifier,
           Is_Access              => Is_Access,
           Is_Inherit             => Is_Inherit,
           Single_Default_Value   => Single_Default_Value,
           Multiple_Default_Value => Multiple_Default_Value,
           Property_Name_Type     =>
             Property_Type_Designator (Property_Definition_Value),
           Property_Type_Is_A_List => Is_A_List,
           Applies_To_All          => Is_All,
           Applies_To              => Owner_Categories);
      Set_Multiplicity (Property_Name_Type (Property), Multiplicity);
      return Property;
   end P_Property_Definition_Declaration;

   -------------------------------
   -- P_Property_Owner_Category --
   -------------------------------

   --  property_owner_category ::=
   --       component_category [ classifier_reference ]
   --     | mode | port group | flow
   --     | [ event ] [ data ] port
   --     | server subprogram
   --     | [ connection_type ] connections

   --  connection_type ::= port group | [ event ] [ data ] port | access

   function P_Property_Owner_Category return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Entities;
      use Ocarina.Builder.AADL.Properties;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use FE_AADL.Parser.Identifiers;
      use FE_AADL.Parser.Components;

      Owner_Category : Node_Id;
      Category       : Named_Element;
      Comp_Cat       : Component_Category := CC_Unknown;
      Classifier_Ref : Node_Id            := No_Node;
      Loc            : Location;

   begin
      Scan_Token;
      Owner_Category := New_Node (K_Named_Element, Token_Location);
      case Token is
         when T_Mode =>
            Category := PO_Mode;

         when T_Port =>
            Save_Lexer (Loc);
            Scan_Token;
            case Token is
               when T_Group =>
                  Save_Lexer (Loc);
                  Scan_Token;
                  if Token = T_Connections then
                     Category := PO_Port_Group_Connections;
                  else
                     Restore_Lexer (Loc);
                     Category := PO_Port_Group;
                  end if;

               when T_Connections =>
                  Category := PO_Port_Connections;

               when others =>
                  Restore_Lexer (Loc);
                  Category := PO_Port;
            end case;

         when T_Flow =>
            Category := PO_Flow;

         when T_Event =>
            Scan_Token;
            case Token is
               when T_Port =>
                  Save_Lexer (Loc);
                  Scan_Token;
                  if Token = T_Connections then
                     Category := PO_Event_Port_Connections;
                  else
                     Restore_Lexer (Loc);
                     Category := PO_Event_Port;
                  end if;

               when T_Data =>
                  Scan_Token;
                  if Token /= T_Port then
                     DPE (PC_Property_Owner_Category, T_Port);
                     return No_Node;
                  end if;

                  Save_Lexer (Loc);
                  Scan_Token;
                  if Token = T_Connections then
                     Category := PO_Event_Data_Port_Connections;
                  else
                     Restore_Lexer (Loc);
                     Category := PO_Event_Data_Port;
                  end if;

               when others =>
                  DPE (PC_Property_Owner_Category, (T_Port, T_Data));
                  return No_Node;
            end case;

         when T_Data =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Port then
               Save_Lexer (Loc);
               Scan_Token;
               if Token = T_Connections then
                  Category := PO_Data_Port_Connections;
               else
                  Restore_Lexer (Loc);
                  Category := PO_Data_Port;
               end if;
            else
               Restore_Lexer (Loc);
               Category := PO_Component_Category;
               Comp_Cat := CC_Data;
            end if;

         when T_Server =>
            Scan_Token;
            if Token /= T_Subprogram then
               DPE (PC_Property_Owner_Category, T_Subprogram);
               return No_Node;
            end if;
            Category := PO_Server_Subprogram;

         when T_Parameter =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Connections then
               Category := PO_Parameter_Connections;
            else
               Restore_Lexer (Loc);
               Category := PO_Parameter;
            end if;

         when T_Access =>
            Scan_Token;
            if Token /= T_Connections then
               DPE (PC_Property_Owner_Category, T_Connections);
               return No_Node;
            end if;
            Category := PO_Access_Connection;

         when T_Connections =>
            Category := PO_Connections;

         when T_Subprogram |
           T_Thread        |
           T_Process       |
           T_Memory        |
           T_Processor     |
           T_Bus           |
           T_Device        |
           T_Virtual       |
           T_System        =>
            Category := PO_Component_Category;
            Comp_Cat := P_Component_Category;

         when others =>
            DPE (PC_Property_Owner_Category);
            return No_Node;
      end case;

      if Category = PO_Component_Category then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Identifier then
            Restore_Lexer (Loc);
            Classifier_Ref := P_Entity_Reference (PC_Property_Owner_Category);

            if No (Classifier_Ref) then
               --  error when parsing Classifier_Reference, quit
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      Set_Category (Owner_Category, Named_Element'Pos (Category));
      Set_Component_Cat (Owner_Category, Component_Category'Pos (Comp_Cat));
      Set_Classifier_Ref (Owner_Category, Classifier_Ref);

      return Owner_Category;
   end P_Property_Owner_Category;

   ----------------------------------
   -- P_Property_Owner_Or_Category --
   ----------------------------------

   --  AADL_V1
   --  property_owner_category ::=
   --       component_category [ classifier_reference ]
   --     | mode | port group | flow
   --     | [ event ] [ data ] port
   --     | server subprogram
   --     | [ connection_type ] connections

   --  connection_type ::= port group | [ event ] [ data ] port | access

   --  AADL_V2
   --  property_owner ::= named_element_qualified_meta_model_identifier

   function P_Property_Owner_Or_Category
     (Container : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use FE_AADL.Parser.Properties.Values;

      pragma Unreferenced (Container);

      Owner_Category : Node_Id;

   begin

      case AADL_Version is
         when AADL_V1 =>
            Owner_Category := P_Property_Owner_Category;

         when AADL_V2 =>
            Owner_Category := P_Named_Element;
      end case;

      if Owner_Category = No_Node then
         DPE (PC_Property_Owner_Or_Category);
      end if;

      return Owner_Category;
   end P_Property_Owner_Or_Category;

   --------------------
   -- P_Property_Set --
   --------------------

   --  AADL_V1
   --  property set defining_property_set_identifier is
   --     { property_type_declaration |
   --       property_name_declaration |
   --       property_constant }+
   --  end defining_property_set_identifier ;

   --  AADL_V2
   --  property_set ::=
   --  property set defining_property_set_identifier is
   --    { with ( property_set_identifier
   --                {, property_set_identifier }* ; }*
   --    { property_type_declaration       |
   --      property_definition_declaration |
   --      property_constant }+
   --  end defining_property_set_identifier ;

   function P_Property_Set
     (AADL_Spec : Node_Id;
      Start_Loc : Location) return Node_Id
   is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Namespaces;
      use Ocarina.FE_AADL.Parser.Properties.Values;
      use Ocarina.Builder.AADL.Properties;

      Previous     : Node_Id;
      Property_Set : Node_Id;
      Identifier   : Node_Id;
      Decl_Ident   : Node_Id;  --  identifier of current declaration
      Current_Decl : Node_Id;
      Imports_List : List_Id := No_List;
      Import_Node  : Node_Id := No_Node;
      Loc          : Location;

   begin
      Scan_Token;

      if Token /= T_Identifier then
         DPE (PC_Property_Set, T_Identifier);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Identifier := Make_Current_Identifier (No_Node);

      Scan_Token;

      if Token /= T_Is then
         DPE (PC_Property_Set, T_Is);
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Property_Set := Add_New_Property_Set (Start_Loc, Identifier, AADL_Spec);

      if Property_Sets.Is_Standard (Identifier) then

         --  For a standard property set, check whether it has already
         --  been declared in which case there is an identifier
         --  attached to the property set name.

         Previous := Property_Set_Entity (Identifier);
         if No (Previous) then
            Set_Property_Set_Entity (Identifier);

         --  If it is already defined, check whether it is predefined
         --  in which we are allowed to remove the predefined
         --  declaration to parse the user declaration.

         elsif not Is_User_Defined (Previous) then
            Remove_Node_From_List
              (Corresponding_Entity (Previous),
               Declarations (AADL_Spec));
            Set_Property_Set_Entity (Identifier);
            Set_As_User_Defined (Identifier);
         end if;

      else
         Set_As_User_Defined (Identifier);
      end if;

      if Property_Set = No_Node then
         Skip_Tokens ((T_End, T_Semicolon));
         return No_Node;
      end if;

      Scan_Token;

      if Token = T_With then
         case AADL_Version is
            when AADL_V2 =>
               Save_Lexer (Loc);
               Imports_List := New_List (K_List_Id, Loc);

               while Token = T_With loop
                  Save_Lexer (Loc);

                  Import_Node := P_Import_Declaration (Property_Set, Loc);
                  if No (Import_Node) then
                     DPE (PC_Property_Set);
                     Skip_Tokens (T_Semicolon);
                     return No_Node;
                  else
                     Append_Node_To_List (Import_Node, Imports_List);
                  end if;

                  Scan_Token;
               end loop;

               if Is_Empty (Imports_List) then
                  DPE (PC_Property_Set, EMC_List_Is_Empty);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            when others =>
               DPE (PC_Property_Set, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;
      end if;

      loop
         exit when Token = T_End;

         if Token /= T_Identifier then
            DPE (PC_Property_Declaration, T_Identifier);
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

         Decl_Ident := Make_Current_Identifier (No_Node);
         Scan_Token;

         if Token /= T_Colon then
            DPE (PC_Property_Declaration, T_Colon);
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

         Save_Lexer (Loc);
         Scan_Token;

         case Token is
            when T_Type =>
               Current_Decl :=
                 P_Property_Type_Declaration
                   (Identifier   => Decl_Ident,
                    Property_Set => Property_Set);

            when T_Constant =>
               Current_Decl :=
                 P_Property_Constant
                   (Identifier   => Decl_Ident,
                    Property_Set => Property_Set);

            when others =>
               Restore_Lexer (Loc);
               Current_Decl :=
                 P_Property_Definition_Declaration
                   (Identifier   => Decl_Ident,
                    Property_Set => Property_Set);
         end case;

         if No (Current_Decl) then
            --  Error when parsing property declaration, quit
            Skip_Tokens ((T_End, T_Semicolon));
            return No_Node;
         end if;

         Scan_Token;
         exit when Token = T_End;
      end loop;

      if not P_Expected_Identifier (Identifier) then
         --  Error when parsing defining_identifier, quit
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (PC_Property_Set, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Imports_List /= No_List then
         Set_Imports_List (Property_Set, Imports_List);
      end if;

      return Property_Set;
   end P_Property_Set;

end Ocarina.FE_AADL.Parser.Properties;
