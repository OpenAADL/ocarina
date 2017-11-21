------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           OCARINA.FE_AADL.PARSER.COMPONENTS.SUBPROGRAM_CALLS             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.FE_AADL.Lexer;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Components.Modes;
with Ocarina.FE_AADL.Parser.Properties;

with Ocarina.Builder.AADL.Components.Subprogram_Calls;

package body Ocarina.FE_AADL.Parser.Components.Subprogram_Calls is

   function P_Subprogram_Call (Container : Types.Node_Id) return Node_Id;
   function P_Called_Subprogram (Code : Parsing_Code) return Node_Id;

   -----------------------
   -- P_Subprogram_Call --
   -----------------------

   --  AADL_V1
   --  subprogram_call ::=
   --     defining_call_identifier : subprogram called_subprogram
   --     [ { { subcomponent_call_property_association }+ } ] ;

   --  called_subprogram ::=
   --     subprogram_classifier_reference
   --     | data_unique_type_reference . data_subprogram_identifier

   --  AADL_V2
   --  subprogram_call ::=
   --     defining_call_identifier : subprogram called_subprogram
   --     [ { { subcomponent_call_property_association }+ } ] ;

   function P_Subprogram_Call (Container : Types.Node_Id) return Node_Id is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Parser.Properties;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Subprogram_Calls;

      pragma Assert
        (Container /= No_Node
         and then Kind (Container) = K_Subprogram_Call_Sequence);

      Subprog_Call : Node_Id;
      Identifier   : Node_Id;
      Call_Ref     : Node_Id;
      OK           : Boolean;
      Loc          : Location;

   begin
      Scan_Token;
      if Token /= T_Identifier then
         DPE (PC_Subprogram_Call, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         Save_Lexer (Loc);
      end if;

      Identifier := Make_Current_Identifier (No_Node);

      Scan_Token;     --  parse ':'
      if Token /= T_Colon then
         DPE (PC_Subprogram_Call, T_Colon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;     --  parse 'subprogram'
      if Token /= T_Subprogram then
         DPE (PC_Subprogram_Call, T_Subprogram);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Call_Ref := P_Called_Subprogram (PC_Subprogram_Call);

      if No (Call_Ref) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Subprog_Call :=
        Add_New_Subprogram_Call
          (Loc           => Loc,
           Name          => Identifier,
           Call_Sequence => Container);

      OK :=
        P_Property_Associations
          (Subprog_Call,
           True,
           PAT_Simple,
           PC_Subprogram_Call);
      if not OK then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (PC_Subprogram_Call, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Entity_Ref (Subprog_Call, Call_Ref);

      return Subprog_Call;
   end P_Subprogram_Call;

   --------------------------------
   -- P_Subprogram_Call_Sequence --
   --------------------------------

   --  AADL_V1
   --  subprogram_call_sequence ::=
   --     [ defining_call_sequence_identifier : ]
   --     { { subprogram_call }+ } [ in_modes ] ;

   --  AADL_V2
   --  subprogram_call_sequence ::=
   --     defining_call_sequence_identifier :
   --     { { subprogram_call }+ }
   --     [ { { call_sequence_property_association }+ } ] [ in_modes ] ;

   function P_Subprogram_Call_Sequence
     (Container : Types.Node_Id) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Parser.Properties;
      use Parser.Components.Modes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Subprogram_Calls;

      Call_Sequence    : Node_Id;
      Identifier       : Node_Id := No_Node;
      Subprogram_Calls : List_Id;
      In_Modes         : Node_Id;
      Loc              : Location;
      Start_Loc        : Location;
      OK               : Boolean;
   begin
      Save_Lexer (Loc);
      Start_Loc := Loc;
      Scan_Token;

      case AADL_Version is
         when AADL_V1 =>
            if Token /= T_Identifier
              and then Token /= T_Left_Curly_Bracket
            then
               --  return No_Node, try to parse other element,
               --  so no error message
               Restore_Lexer (Loc);
               return No_Node;
            end if;

         when AADL_V2 =>
            if Token /= T_Identifier then
               Restore_Lexer (Loc);
               return No_Node;
            end if;
      end case;

      if Token = T_Identifier then
         Identifier := Make_Current_Identifier (No_Node);

         Scan_Token;    --  parse next token, ':' is expected
         if Token /= T_Colon then
            DPE (PC_Subprogram_Call_Sequence, T_Colon);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;    --  parse next token, '{' is expected
      end if;

      if Token /= T_Left_Curly_Bracket then
         DPE (PC_Subprogram_Call_Sequence, T_Left_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Call_Sequence :=
        Add_New_Subprogram_Call_Sequence
          (Loc       => Start_Loc,
           Comp_Impl => Container,
           Name      => Identifier,
           In_Modes  => No_Node);

      Subprogram_Calls :=
        P_Elements_List
          (P_Subprogram_Call'Access,
           Call_Sequence,
           T_Right_Curly_Bracket);

      --  XXX The container should not be 'no_node'. Yet, at this
      --  point, the sequence has not been created yet

      if No (Subprogram_Calls) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      if AADL_Version = AADL_V2 then
         OK :=
           P_Property_Associations
             (Call_Sequence,
              True,
              PAT_Simple,
              PC_Subprogram_Call_Sequence);
         if not OK then
            return No_Node;
         end if;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_In then
         In_Modes := P_In_Modes (PC_In_Modes);

         if No (In_Modes) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Set_In_Modes (Call_Sequence, In_Modes);
      else
         In_Modes := No_Node;
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_Semicolon then
         DPE (PC_Subprogram_Call_Sequence, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      return Call_Sequence;
   end P_Subprogram_Call_Sequence;

   -------------------------
   -- P_Called_Subprogram --
   -------------------------

   --  AADL_V2
   --  called_subprogram ::=
   --     subprogram_unique_component_classifier_reference
   --   | ( data_unique_component_type_reference
   --                  . data_provides_subprogram_access_identifier )
   --   | ( subprogram_group_unique_component_type_reference
   --                  . provides_subprogram_access_identifier )
   --   | prototype_identifier
   --   | processor . provides_subprogram_access_identifier
   --   | subprogram_subcomponent_identifier
   --   | subprogram_group_subcomponent_identifier
   --                  . provides_subprogram_access_identifier
   --   | requires_subprogram_access_identifier
   --   | requires_subprogram_group_access_reference
   --                  . provides_subprogram_access_identifier

   function P_Called_Subprogram (Code : Parsing_Code) return Node_Id is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Loc  : Location;
      Node : Node_Id;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Processor =>
            if AADL_Version = AADL_V2 then
               Scan_Token;
               if Token /= T_Dot then
                  DPE (PC_Called_Subprogram, T_Dot);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Node := P_Entity_Reference (Code);
            else
               DPE (PC_Called_Subprogram, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Identifier =>
            Restore_Lexer (Loc);
            Node := P_Entity_Reference (Code);

         when others =>
            DPE (PC_Called_Subprogram, (T_Processor, T_Identifier));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      if No (Node) then
         DPE (PC_Called_Subprogram);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      return Node;

   end P_Called_Subprogram;

end Ocarina.FE_AADL.Parser.Components.Subprogram_Calls;
