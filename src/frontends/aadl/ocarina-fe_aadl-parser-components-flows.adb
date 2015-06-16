------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.FE_AADL.PARSER.COMPONENTS.FLOWS                  --
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

with Locations;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.FE_AADL.Lexer;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Components.Modes;
with Ocarina.FE_AADL.Parser.Properties;
with Ocarina.Builder.AADL.Components.Flows;

package body Ocarina.FE_AADL.Parser.Components.Flows is

   use Lexer;
   use Ocarina.ME_AADL.Tokens;
   use Locations;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Parser.Properties;
   use Parser.Identifiers;
   use Parser.Components.Modes;
   use Ocarina.Builder.AADL.Components.Flows;

   function P_Flow_Implementation
     (Identifier    : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean;
      Is_End_To_End : Boolean;
      Category      : Flow_Category) return Node_Id;
   --  Parse Flow_Implementation, Flow_Implementation_Refinement
   --  End_To_End_Flow_Spec and End_To_End_Flow_Refinement

   function P_Flow_Spec
     (Identifier    : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean;
      Category      : Flow_Category) return Node_Id;
   --  This function is called in P_Flow_Spec (Boolean) to make this
   --  function body shorter and easier to read

   ---------------------------
   -- P_Flow_Implementation --
   ---------------------------

   --  flow_source_implementation ::= flow_identifier : flow source
   --     { subcomponent_flow_identifier -> connection_identifier -> }*
   --     flow_feature_identifier

   --  flow_sink_implementation ::= flow_identifier : flow sink
   --     flow_feature_identifier
   --     { -> connection_identifier -> subcomponent_flow_identifier }*

   --  flow_path_implementation ::= flow_identifier : flow path
   --     source_flow_feature_identifier
   --     [ { -> connection_identifier -> subcomponent_flow_identifier }+
   --     -> connection_identifier ]
   --     -> sink_flow_feature_identifier

   --  end_to_end_flow_spec ::= flow_identifier : end to end flow
   --     start_subcomponent_flow_identifier
   --        { -> connection_identifier
   --          -> flow_path_subcomponent_flow_identifier }*
   --     -> connection_identifier -> end_subcomponent_flow_identifier
   --     [ { ( property_association }+ } ]
   --     [ in_modes_and_transitions ] ;

   --  flow_source_implementation_refinement ::=
   --     flow_identifier : refined to flow source
   --     ( { { property_association }+ } [ in_modes_and_transitions ]
   --       | in_modes_and_transitions ) ;

   --  flow_sink_implementation_refinement ::=
   --     flow_identifier : refined to flow sink
   --     ( { { property_association }+ } [ in_modes_and_transitions ]
   --       | in_modes_and_transitions ) ;

   --  flow_path_implementation_refinement ::=
   --     flow_identifier : refined to flow path
   --     ( { { property_association }+ } [ in_modes_and_transitions ]
   --       | in_modes_and_transitions ) ;

   --  end_to_end_flow_refinement ::=
   --     defining_identifier : refined to end to end flow
   --     ( { { property_association }+ } [ in_modes_and_transitions ]
   --       | in_modes_and_transitions ) ;

   --  subcomponent_flow_identifier ::=
   --     subcomponent_identifier . flow_spec_identifier

   function P_Flow_Implementation
     (Identifier    : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean;
      Is_End_To_End : Boolean;
      Category      : Flow_Category) return Node_Id
   is
      Flow_Impl   : Node_Id;
      Connections : List_Id;
      Modes_Trans : Node_Id := No_Node;  --  modes and transitions
      Code        : Parsing_Code;
      Loc         : Location;
      OK          : Boolean;
      Item        : Node_Id;
      Source_Flow : Node_Id := No_Node;
      Sink_Flow   : Node_Id := No_Node;
   begin
      if Is_End_To_End then
         if Is_Refinement then
            Code := PC_End_To_End_Flow_Refinement;
         else
            Code := PC_End_To_End_Flow_Spec;
         end if;
      else
         case Category is
            when FC_Source =>
               if Is_Refinement then
                  Code := PC_Flow_Source_Implementation_Refinement;
               else
                  Code := PC_Flow_Source_Implementation;
               end if;
            when FC_Sink =>
               if Is_Refinement then
                  Code := PC_Flow_Sink_Implementation_Refinement;
               else
                  Code := PC_Flow_Sink_Implementation;
               end if;
            when FC_Path =>
               if Is_Refinement then
                  Code := PC_Flow_Path_Implementation_Refinement;
               else
                  Code := PC_Flow_Path_Implementation;
               end if;
         end case;
      end if;

      if not Is_Refinement then
         --  Parse connections

         Connections := New_List (K_List_Id, Token_Location);

         loop
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Semicolon or else Token = T_Left_Curly_Bracket then
               Restore_Lexer (Loc);
               exit;
            end if;

            Restore_Lexer (Loc);
            Item := P_Entity_Reference (Code);

            if Present (Item) then
               Append_Node_To_List (Item, Connections);
            else
               --  Error when parsing item, ignores tokens ---> Delimiter; quit
               Skip_Tokens (T_Semicolon, False);
               Connections := No_List;
            end if;

            Save_Lexer (Loc);
            Scan_Token; --  consume "->"

            if Token = T_Semicolon
              or else Token = T_Left_Curly_Bracket
              or else Token = T_In
            then
               Restore_Lexer (Loc);
               exit;
            end if;

            if Token /= T_Direct_Connection then
               DPE (Code, (T_Semicolon, T_Left_Curly_Bracket, T_In));
               Skip_Tokens (T_Semicolon, False);
               Connections := No_List;
               exit;
            end if;
         end loop;

         if No (Connections) or else Is_Empty (Connections) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         else
            --  According to the AADL grammar some of the elements in
            --  the X -> Y -> Z ... chain are different from the rest
            --  of the elements. We relocate them to special fields ti
            --  make the analysis simpler. We also perform some
            --  implicit syntactic rules on the number of the
            --  remaining elements in the list (odd or even) depending
            --  on the flow nature.

            if Is_End_To_End then
               --  The list must contain at least two elements

               if Length (Connections) < 3 then
                  DPE (Code, EMC_At_Least_Three_Elements_Expected);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
               --  The first entity reference is the start identifier

               Source_Flow := First_Node (Connections);
               Remove_Node_From_List (Source_Flow, Connections);

               --  The last entity reference is the end identifier

               Sink_Flow := Last_Node (Connections);
               Remove_Node_From_List (Sink_Flow, Connections);

               --  According to the grammar the remaining number
               --  of element in the connection list must be
               --  odd.

               if Length (Connections) mod 2 = 0 then
                  --  We display en error message expecting an
                  --  odd number because we already removed two.

                  DPE (Code, EMC_Odd_Number_Of_Element_Expected);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
            else
               case Category is
                  when FC_Source =>
                     --  The last entity reference is the ultimate
                     --  source.

                     Source_Flow := Last_Node (Connections);
                     Remove_Node_From_List (Source_Flow, Connections);

                     --  According to the grammar the remaining number
                     --  of element in the connection list must be
                     --  even.

                     if Length (Connections) mod 2 /= 0 then
                        --  We display en error message expecting an
                        --  odd number because we already removed one.

                        DPE (Code, EMC_Odd_Number_Of_Element_Expected);
                        Skip_Tokens (T_Semicolon);
                        return No_Node;
                     end if;

                  when FC_Sink =>
                     --  The first entity reference is the sink
                     --  entrypoint.

                     Sink_Flow := First_Node (Connections);
                     Remove_Node_From_List (Sink_Flow, Connections);

                     --  According to the grammar the remaining number
                     --  of element in the connection list must be
                     --  even.

                     if Length (Connections) mod 2 /= 0 then
                        --  We display en error message expecting an
                        --  odd number because we already removed one.

                        DPE (Code, EMC_Odd_Number_Of_Element_Expected);
                        Skip_Tokens (T_Semicolon);
                        return No_Node;
                     end if;

                  when FC_Path =>
                     --  The list must contain at least two elements

                     if Length (Connections) < 2 then
                        DPE (Code, EMC_At_Least_Tow_Elements_Expected);
                        Skip_Tokens (T_Semicolon);
                        return No_Node;
                     end if;

                     --  The first entity reference is the path source

                     Source_Flow := First_Node (Connections);
                     Remove_Node_From_List (Source_Flow, Connections);

                     --  The last entity reference is the path sink

                     Sink_Flow := Last_Node (Connections);
                     Remove_Node_From_List (Sink_Flow, Connections);

                     --  According to the grammar the remaining number
                     --  of element in the connection list must be
                     --  zero or odd.

                     if Length (Connections) /= 0
                       and then Length (Connections) mod 2 = 0
                     then
                        --  We display en error message expecting an
                        --  odd number because we already removed two.

                        DPE (Code, EMC_Odd_Number_Of_Element_Expected);
                        Skip_Tokens (T_Semicolon);
                        return No_Node;
                     end if;
               end case;
            end if;

            if not Is_Empty (Connections) then
               Set_Loc
                 (Node_Id (Connections),
                  Ocarina.ME_AADL.AADL_Tree.Nodes.Loc
                    (First_Node (Connections)));
            end if;
         end if;
      end if;

      if Is_End_To_End then
         Flow_Impl :=
           Add_New_End_To_End_Flow_Spec
             (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
              Container     => Container,
              Name          => Identifier,
              Is_Refinement => Is_Refinement,
              In_Modes      => Modes_Trans,
              Source_Flow   => Source_Flow,
              Sink_Flow     => Sink_Flow);
      else
         Flow_Impl :=
           Add_New_Flow_Implementation
             (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
              Container     => Container,
              Name          => Identifier,
              Category      => Category,
              Is_Refinement => Is_Refinement,
              In_Modes      => Modes_Trans,
              Source_Flow   => Source_Flow,
              Sink_Flow     => Sink_Flow);
      end if;

      OK := P_Property_Associations (Flow_Impl, True, PAT_Simple, Code);

      if not OK then
         --  Property_Associations are not defined,
         --  In_Modes_And_Transitions must exist if Is_Refinement =
         --  TRUE

         if Is_Refinement and then Token /= T_In then
            DPE (Code, (T_In, T_Left_Curly_Bracket));
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_In then
         Modes_Trans := P_In_Modes (Code => PC_In_Modes_And_Transitions);

         if No (Modes_Trans) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Present (Flow_Impl) and then not Is_Refinement then
         Set_Connections (Flow_Impl, Connections);
      end if;

      Set_In_Modes (Flow_Impl, Modes_Trans);

      return Flow_Impl;
   end P_Flow_Implementation;

   ---------------------------------------------------
   -- P_Flow_Implementation_Or_End_To_End_Flow_Spec --
   ---------------------------------------------------

   --  flow_implementation ::=
   --     (   flow_source_implementation
   --       | flow_sink_implementation
   --       | flow_path_implementation )
   --     [ { { property_association }+ } ]
   --     [ in_modes_and_transitions ] ;

   --  flow_implementation_refinement ::=
   --       flow_source_implementation_refinement
   --     | flow_sink_implementation_refinement
   --     | flow_path_implementation_refinement

   function P_Flow_Implementation_Or_End_To_End_Flow_Spec
     (Container : Node_Id;
      Refinable : Boolean) return Node_Id
   is
      Loc           : Location;
      Identifier    : Node_Id;
      Is_Refinement : Boolean      := False;
      Is_End_To_End : Boolean;
      Category      : Flow_Category;
      Code          : Parsing_Code := PC_Flow_Implementation;
      OK            : Boolean;
   begin

      case AADL_Version is
         when AADL_V1 =>
            P_Identifier_Refined_To
              (Refinable_To_RT (Refinable),
               False,
               PC_Flow_Implementation,
               PC_Flow_Implementation_Refinement,
               T_Semicolon,
               Identifier,
               Is_Refinement,
               OK);

            if not OK then
               return No_Node;
            end if;

         when AADL_V2 =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token /= T_Identifier then
               return No_Node;
            else
               Identifier := Make_Current_Identifier (No_Node);
            end if;

            Scan_Token;
            if Token /= T_Colon then
               DPE (PC_Flow_Implementation, T_Colon);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
      end case;

      Scan_Token;

      case Token is
         when T_Flow =>
            --  Flow_Implementation or Flow_Implementation_Refinement

            Is_End_To_End := False;

            if Is_Refinement then
               Code := PC_Flow_Implementation_Refinement;
            else
               Code := PC_Flow_Implementation;
            end if;

            Scan_Token;

            case Token is
               when T_Source =>
                  Category := FC_Source;
               when T_Sink =>
                  Category := FC_Sink;
               when T_Path =>
                  Category := FC_Path;

               when others =>
                  DPE (Code, (T_Source, T_Sink, T_Path));
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
            end case;

         when T_End =>
            --  End_To_End_Flow_Spec or End_To_End_Flow_Refinement

            Is_End_To_End := True;

            if Is_Refinement then
               Code := PC_End_To_End_Flow_Refinement;
            else
               Code := PC_End_To_End_Flow_Spec;
            end if;

            Scan_Token;      --  consume 'to'

            if Token /= T_To then
               DPE (Code, T_To);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Scan_Token;      --  consume 'end'

            if Token /= T_End then
               DPE (Code, T_End);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Scan_Token;      --  consume 'flow'

            if Token /= T_Flow then
               DPE (Code, T_Flow);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when others =>
            DPE (Code, (T_Flow, T_End));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      return P_Flow_Implementation
          (Identifier    => Identifier,
           Container     => Container,
           Is_Refinement => Is_Refinement,
           Is_End_To_End => Is_End_To_End,
           Category      => Category);
   end P_Flow_Implementation_Or_End_To_End_Flow_Spec;

   -----------------
   -- P_Flow_Spec --
   -----------------

   --  AADL_V1
   --  flow_spec ::= flow_source_spec | flow_sink_spec | flow_path_spec

   --  flow_spec_refinement ::=   flow_source_spec_refinement
   --                           | flow_sink_spec_refinement
   --                           | flow_path_spec_refinement

   --  flow_source_spec ::=
   --     defining_identifier : flow source flow_feature_identifier
   --        [ { { property_association }+ } ] ;

   --  flow_sink_spec ::=
   --     defining_identifier : flow sink flow_feature_identifier
   --        [ { { property_association }+ } ] ;

   --  flow_path_spec ::=
   --     defining_identifier : flow path
   --        source_flow_feature_identifier -> sink_flow_feature_identifier
   --        [ { { property_association }+ } ] ;

   --  flow_source_spec_refinement ::=
   --     defining_identifier : refined to flow source
   --        { { property_association }+ } ;

   --  flow_sink_spec_refinement ::=
   --     defining_identifier : refined to flow sink
   --        { { property_association }+ } ;

   --  flow_path_spec_refinement ::=
   --     defining_identifier : refined to flow path
   --        { { property_association }+ } ;

   --  AADL_V2
   --  flow_spec ::= flow_source_spec | flow_sink_spec | flow_path_spec

   --  flow_spec_refinement ::=   flow_source_spec_refinement
   --                           | flow_sink_spec_refinement
   --                           | flow_path_spec_refinement

   --  flow_source_spec ::=
   --     defining_flow_identifier : flow source out_flow_feature_identifier
   --        [ { { property_association }+ } ] [ in_modes ] ;

   --  flow_sink_spec ::=
   --      defining_flow_identifier : flow sink in_flow_feature_identifier
   --         [ { { property_association }+ } ] [ in_modes ] ;

   --  flow_path_spec ::=
   --     defining_flow_identifier : flow path
   --        in_flow_feature_identifier -> out_flow_feature_identifier
   --        [ { { property_association }+ } ] [ in_modes ] ;

   --  flow_source_spec_refinement ::=
   --     defining_flow_identifier : refined to flow source
   --        { { property_association }+ } [ in_modes ] ;

   --  flow_sink_spec_refinement ::=
   --     defining_flow_identifier : refined to flow sink
   --        { { property_association }+ } [ in_modes ] ;

   --  flow_path_spec_refinement ::=
   --     defining_flow_identifier : refined to flow path
   --        { { property_association }+ } [ in_modes ] ;

   function P_Flow_Spec
     (Container : Node_Id;
      Refinable : Boolean) return Node_Id
   is
      Identifier    : Node_Id;
      Is_Refinement : Boolean;
      OK            : Boolean;
      Category      : Flow_Category;
      Code          : Parsing_Code;
   begin
      P_Identifier_Refined_To
        (Refinable_To_RT (Refinable),
         False,
         PC_Flow_Spec,
         PC_Flow_Spec_Refinement,
         T_Semicolon,
         Identifier,
         Is_Refinement,
         OK);

      if not OK then
         return No_Node;
      end if;

      if Is_Refinement then
         Code := PC_Flow_Spec_Refinement;
      else
         Code := PC_Flow_Spec;
      end if;

      Scan_Token;

      if Token = T_Flow then
         Scan_Token;

         case Token is
            when T_Source =>
               Category := FC_Source;
            when T_Sink =>
               Category := FC_Sink;
            when T_Path =>
               Category := FC_Path;

            when others =>
               DPE (Code, (T_Source, T_Sink, T_Path));
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;
      else
         DPE (Code, T_Flow);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      return P_Flow_Spec
          (Identifier    => Identifier,
           Container     => Container,
           Is_Refinement => Is_Refinement,
           Category      => Category);
   end P_Flow_Spec;

   -----------------
   -- P_Flow_Spec --
   -----------------

   --  AADL_V1
   --  flow_feature_identifier ::=   port_identifier
   --                              | parameter_identifier
   --                              | port_group_identifier
   --                              | port_group_identifier . port_identifier

   --  AADL_V2
   --  flow_feature_identifier ::=
   --     feature_identifier
   --   | feature_group_identifier
   --   | feature_group_identifier . feature_identifier
   --   | feature_group_identifier . feature_group_identifie

   function P_Flow_Spec
     (Identifier    : Node_Id;
      Container     : Node_Id;
      Is_Refinement : Boolean;
      Category      : Flow_Category) return Node_Id
   is
      Flow_Spec   : Node_Id;
      In_Modes    : Node_Id;
      Source_Flow : Node_Id := No_Node;
      Sink_Flow   : Node_Id := No_Node;
      Code        : Parsing_Code;
      OK          : Boolean;
      Loc         : Location;

      --  Sub-function determining parsing code to display error messages

      function Flow_Parsing_Code return Parsing_Code;
      pragma Inline (Flow_Parsing_Code);

      -----------------------
      -- Flow_Parsing_Code --
      -----------------------

      function Flow_Parsing_Code return Parsing_Code is
      begin
         if Is_Refinement then
            case Category is
               when FC_Source =>
                  return PC_Flow_Source_Spec_Refinement;
               when FC_Sink =>
                  return PC_Flow_Sink_Spec_Refinement;
               when FC_Path =>
                  return PC_Flow_Path_Spec_Refinement;
            end case;
         else
            case Category is
               when FC_Source =>
                  return PC_Flow_Source_Spec;
               when FC_Sink =>
                  return PC_Flow_Sink_Spec;
               when FC_Path =>
                  return PC_Flow_Path_Spec;
            end case;
         end if;
      end Flow_Parsing_Code;

   begin
      Code := Flow_Parsing_Code;

      if not Is_Refinement then
         --  Parse flow_feature_identifier(s)
         case Category is
            when FC_Source =>
               Source_Flow := P_Entity_Reference (Code);

               if No (Source_Flow) then
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            when FC_Sink =>
               Sink_Flow := P_Entity_Reference (Code);

               if No (Sink_Flow) then
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            when FC_Path =>
               Source_Flow := P_Entity_Reference (Code);

               if No (Source_Flow) then
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Scan_Token;  --  Consume "->"

               if Token /= T_Direct_Connection then
                  DPE (Code, T_Direct_Connection);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Sink_Flow := P_Entity_Reference (Code);

               if No (Sink_Flow) then
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
         end case;
      end if;

      Flow_Spec :=
        Add_New_Flow_Spec
          (Loc           => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Comp_Type     => Container,
           Name          => Identifier,
           Category      => Category,
           Is_Refinement => Is_Refinement,
           Sink_Flow     => Sink_Flow,
           Source_Flow   => Source_Flow);

      OK :=
        P_Property_Associations
          (Flow_Spec,
           not Is_Refinement,
           PAT_Simple,
           Code);

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_In then
         In_Modes := P_In_Modes (PC_In_Modes);

         if No (In_Modes) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Set_In_Modes (Flow_Spec, In_Modes);
      else
         In_Modes := No_Node;
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if not OK then
         return No_Node;
      end if;

      return Flow_Spec;
   end P_Flow_Spec;

end Ocarina.FE_AADL.Parser.Components.Flows;
