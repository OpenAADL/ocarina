------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.FE_AADL.PARSER.COMPONENTS.MODES                  --
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

with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Lexer;
with Ocarina.FE_AADL.Parser.Properties;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.Builder.AADL.Components.Modes;

package body Ocarina.FE_AADL.Parser.Components.Modes is

   function P_Mode
     (Mode          : Node_Id;
      Is_Refinement : Boolean;
      Is_Initial    : Boolean;
      Refinable     : Boolean) return Node_Id;
   --  Parse Mode and Mode_Refinement
   --  NOTE: The parameter Refinable is only useful for determining output
   --        error message (for list of expected tokens)

   function P_Mode_Transition
     (Mode_Transition : Types.Node_Id;
      Source_Modes    : List_Id) return Node_Id;
   --  Current token is '-[' and will be ignored in this function

   function P_Unique_Port_Or_Transition_Trigger
     (Container : Types.Node_Id) return Node_Id;
   --  Parse Unique_Port_Identifier when AADL_V1
   --  Parse Transition_Trigger when AADL_V2

   function P_Mode_Transition_Trigger
     (Container : Types.Node_Id) return Node_Id;

   ----------------
   -- P_In_Modes --
   ----------------

   --  AADL_V1
   --  ( in modes ( ( Item { , Item }* | none ) ) )

   --  AADL_V2
   --  in_modes ::=
   --  in modes ( ( mode_identifier { , mode_identifier }* | none ) )

   function P_In_Modes (Code : Parsing_Code) return Node_Id is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.FE_AADL.Parser.Identifiers;

      pragma Assert
        (Code = PC_In_Modes or else Code = PC_In_Modes_And_Transitions);

      In_Modes  : Node_Id;
      Mode_List : List_Id;
      Start_Loc : Location;
      Loc       : Location;
      Item      : Node_Id;

   --  NOTE: no tokens skipping is necessary because when an error
   --  occurs this function return No_Node and calling function
   --  will skip tokens until ';' (in general) is reached
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;   --  Consume 'modes' ('in' is already consumed)

      if Token /= T_Modes then
         DPE (Code, T_Modes);
         return No_Node;
      end if;

      Scan_Token;      --  Consume '('

      if Token /= T_Left_Parenthesis then
         DPE (Code, T_Left_Parenthesis);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_None then
         Mode_List := New_List (K_List_Id, Start_Loc);
      else
         Restore_Lexer (Loc);

         Mode_List := New_List (K_List_Id, Start_Loc);

         loop
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Right_Parenthesis then
               Restore_Lexer (Loc);
               exit;
            end if;

            Restore_Lexer (Loc);

            if Code = PC_In_Modes then
               Item := P_Entity_Reference (Code);
            else
               Item := P_Mode_Or_Transition (No_Node);
            end if;

            if Present (Item) then
               Append_Node_To_List (Item, Mode_List);
            else
               Skip_Tokens (T_Right_Parenthesis, False);
               Mode_List := No_List;
               exit;
            end if;

            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Right_Parenthesis then
               Restore_Lexer (Loc);
               exit;
            end if;

            if Token /= T_Comma then
               Skip_Tokens (T_Right_Parenthesis, False);
               Mode_List := No_List;
            end if;
         end loop;

         if Is_Empty (Mode_List) then
            --  An error message must be displayed because no error
            --  message is displayed in P_Identifier
            DPE (Code, T_Identifier);
            Skip_Tokens (T_Right_Parenthesis);
            return No_Node;
         end if;
      end if;

      Scan_Token;   --  Consume ')'

      if Token /= T_Right_Parenthesis then
         DPE (Code, T_Right_Parenthesis);
         return No_Node;
      end if;

      --  Create the In_Mode node

      In_Modes := New_Node (K_In_Modes, Start_Loc);
      Set_Modes (In_Modes, Mode_List);

      return In_Modes;
   end P_In_Modes;

   ------------
   -- P_Mode --
   ------------

   --  AADL_V1 AND AADL_V2
   --  mode ::= defining_mode_identifier : [ initial ] mode
   --              [ { { mode_property_assocation }+ } ] ;

   --  AADL_V1
   --  mode_refinement ::= defining_mode_identifier : refined to mode
   --                         { { mode_property_assocation }+ } ;

   function P_Mode
     (Mode          : Node_Id;
      Is_Refinement : Boolean;
      Is_Initial    : Boolean;
      Refinable     : Boolean) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Parser.Properties;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;

      Code : Parsing_Code;
      OK   : Boolean;
      Loc  : Location;
   begin
      if Is_Refinement then
         Code := PC_Mode_Refinement;
      else
         Code := PC_Mode;
      end if;

      Scan_Token;
      if Token /= T_Mode then
         if Is_Initial or else Is_Refinement then
            DPE (Code, T_Mode);
         else
            if Refinable then
               DPE (PC_Mode, (T_Mode, T_Initial, T_Refined));
            else
               DPE (PC_Mode, (T_Mode, T_Initial));
            end if;
         end if;

         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      OK :=
        P_Property_Associations (Mode, not Is_Refinement, PAT_Simple, Code);

      if not OK then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Is_Refinement (Mode, Is_Refinement);
      Set_Is_Initial (Mode, Is_Initial);
      return Mode;
   end P_Mode;

   -------------------------------
   -- P_Mode_Or_Mode_Transition --
   -------------------------------

   --  AADL_V1
   --  mode ::= defining_mode_identifier : [ initial ] mode
   --              [ { { mode_property_assocation }+ } ] ;

   --  mode_transition ::=
   --     source_mode_identifier { , source_mode_identifier }*
   --        -[ unique_port_identifier { , unique_port_identifier }* ]->
   --        destination_mode_identifier ;

   --  mode_refinement ::= defining_mode_identifier : refined to mode
   --                         { { mode_property_assocation }+ } ;

   --  AADL_V2

   --  modes_subclause ::=
   --     modes ( { mode | mode_transition }+ | none_statement )

   --  requires_modes_subclause ::=
   --     requires modes ( { mode }+ | none_statement )

   --  mode ::= defining_mode_identifier : [ initial ] mode
   --              [ { { mode_property_assocation }+ } ];

   --  mode_transition ::=
   --     [ defining__mode_transition_identifier : ]
   --     source_mode_identifier
   --        -[ mode_transition_trigger { , mode_transition_trigger }* ]->
   --        destination_mode_identifier
   --        { { mode_transition_property_assocation }+ };

   function P_Mode_Or_Mode_Transition
     (Container : Node_Id;
      Refinable : Boolean) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Modes;

      Identifier       : Node_Id;
      Identifiers_List : List_Id;
      Source_Modes     : List_Id;
      Is_Refinement    : Boolean := False;
      Is_Initial       : Boolean := False;
      Is_Requires      : Boolean := False;
      Loc              : Location;
      Node             : Node_Id;
   begin

      --  requires modes, only in AADLv2
      case AADL_Version is
         when AADL_V1 =>
            if Token = T_Requires then
               DPE (PC_Mode, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens ((T_End, T_Semicolon));
               return No_Node;
            end if;

         when AADL_V2 =>
            if Token = T_Requires then
               Scan_Token;

               if Token = T_Modes then
                  Is_Requires := True;
               else
                  DPE (PC_Requires_Modes_Subclause, T_Modes);
                  Skip_Tokens ((T_End, T_Semicolon));
                  return No_Node;
               end if;
            end if;
      end case;

      Identifier := P_Identifier (No_Node);
      if No (Identifier) then
         --  Error when parsing identifier, quit
         return No_Node;
      end if;

      Scan_Token;
      case Token is
         when T_Colon =>       --  parsing Mode or Mode_Refinement
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Refined and then AADL_Version = AADL_V1 then
               if not Refinable then
                  DPE (PC_Mode, EMC_Refinement_Is_Not_Allowed);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Scan_Token;     --  parsing 'to'
               if Token /= T_To then
                  DPE (PC_Mode_Refinement, T_To);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Is_Refinement := True;

            elsif Token = T_Refined and then AADL_Version = AADL_V2 then
               DPE (PC_Mode, EMC_Refinement_Is_Not_Allowed);
               Skip_Tokens (T_Semicolon);
               return No_Node;

            elsif Token = T_Initial then
               Is_Initial := True;

            else
               Restore_Lexer (Loc);
            end if;

            Node :=
              Add_New_Mode
                (Loc => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
                 Identifier => Identifier,
                 Component  => Container);

            return P_Mode (Node, Is_Refinement, Is_Initial, Refinable);

         when T_Left_Step_Bracket | T_Comma =>   --  parse Mode_Transition

            if Is_Requires then
               DPE
                 (PC_Requires_Modes_Subclause,
                  EMC_Mode_Transition_Not_Allowed_In_Requires_Modes);
               return No_Node;
            end if;

            Source_Modes :=
              New_List
                (K_Identifiers_List,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier));
            Append_Node_To_List (Identifier, Source_Modes);

            if Token = T_Comma then
               --  parse next source modes
               Identifiers_List :=
                 P_Items_List
                   (P_Identifier'Access,
                    No_Node,
                    T_Comma,
                    T_Left_Step_Bracket,
                    PC_Mode_Or_Mode_Transition,
                    False);
               if No (Identifiers_List) then
                  DPE (PC_Mode_Transition, T_Identifier);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Append_Node_To_List
                 (First_Node (Identifiers_List),
                  Source_Modes);

               if Token /= T_Left_Step_Bracket then
                  DPE (PC_Mode_Transition, T_Left_Step_Bracket);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
            end if;

            Node :=
              Add_New_Mode_Transition
                (Loc       => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
                 Component => Container);
            return P_Mode_Transition (Node, Source_Modes);

         when others =>
            DPE (PC_Mode_Or_Mode_Transition, (T_Colon, T_Left_Step_Bracket));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
   end P_Mode_Or_Mode_Transition;

   --------------------------
   -- P_Mode_Or_Transition --
   --------------------------

   --  AADL_V1
   --  mode_or_transition ::=
   --     mode_identifier | ( old_mode_identifier -> new_mode_identifier )

   --  AADL_V2
   --  mode_or_transition ::=
   --     mode_identifier | mode_transition_identifier

   function P_Mode_Or_Transition (Container : Types.Node_Id) return Node_Id is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      pragma Unreferenced (Container);

      Mode_Tran : Node_Id;
      Loc       : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Parenthesis and then AADL_Version = AADL_V1 then
         Mode_Tran := New_Node (K_Pair_Of_Entity_References, Token_Location);
         Set_First_Reference
           (Mode_Tran,
            P_Entity_Reference (PC_Mode_Or_Mode_Transition));
         Scan_Token;

         if Token /= T_Direct_Connection then
            DPE (PC_Mode_Or_Transition, (T_Right_Parenthesis));
            return No_Node;
         end if;

         Set_Second_Reference
           (Mode_Tran,
            P_Entity_Reference (PC_Mode_Or_Mode_Transition));
         Scan_Token;

         if Token /= T_Right_Parenthesis then
            Mode_Tran := No_Node;
            DPE (PC_Mode_Or_Transition, (T_Right_Parenthesis));
         end if;

      else
         Restore_Lexer (Loc);
         Mode_Tran := P_Entity_Reference (PC_Mode_Or_Transition);
      end if;

      return Mode_Tran;
   end P_Mode_Or_Transition;

   -----------------------
   -- P_Mode_Transition --
   -----------------------

   --  AADL_V1
   --  mode_transition ::=
   --     source_mode_identifier { , source_mode_identifier }*
   --        -[ unique_port_identifier { , unique_port_identifier }* ]->
   --        destination_mode_identifier ;

   --  AADL_V2
   --  mode_transition ::=
   --     [ defining__mode_transition_identifier : ]
   --     source_mode_identifier
   --        -[ mode_transition_trigger { , mode_transition_trigger }* ]->
   --        destination_mode_identifier
   --        { { mode_transition_property_assocation }+ };

   function P_Mode_Transition
     (Mode_Transition : Node_Id;
      Source_Modes    : List_Id) return Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Parser.Properties;
      use Ocarina.FE_AADL.Parser.Identifiers;

      pragma Assert (Mode_Transition /= No_Node);

      List_Ident  : List_Id;
      Destination : Node_Id;
      Loc         : Location;
      OK          : Boolean;
   begin
      List_Ident :=
        P_Items_List
          (P_Unique_Port_Or_Transition_Trigger'Access,
           No_Node,
           T_Comma,
           T_Right_Step_Bracket,
           PC_Mode_Or_Mode_Transition,
           False);
      if No (List_Ident) then
         DPE (PC_Mode_Transition, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      if Token /= T_Right_Step_Bracket then
         DPE (PC_Mode_Transition, T_Right_Step_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Identifier then
         DPE (PC_Mode_Transition, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Destination := Make_Current_Identifier (No_Node);

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Left_Curly_Bracket then
         case AADL_Version is
            when AADL_V1 =>
               DPE (PC_Property_Association, EMC_Not_Allowed_In_AADL_V2);
               Restore_Lexer (Loc);
               return No_Node;

            when AADL_V2 =>
               Restore_Lexer (Loc);
               if AADL_Version = AADL_V2 then
                  OK :=
                    P_Property_Associations
                      (Mode_Transition,
                       False,
                       PAT_Simple,
                       PC_Mode_Transition);

                  if not OK then
                     return No_Node;
                  end if;
               end if;
         end case;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_Mode_Transition, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Source_Modes (Mode_Transition, Source_Modes);
      Set_Triggers (Mode_Transition, List_Ident);
      Set_Destination_Mode (Mode_Transition, Destination);

      return Mode_Transition;
   end P_Mode_Transition;

   -----------------------------------------
   -- P_Unique_Port_Or_Transition_Trigger --
   -----------------------------------------

   --  AADL_V1 AND AADL_V2
   --  unique_port_identifier ::=
   --     [ subcomponent_identifier . ] port_identifier

   --  AADL_V2
   --  mode_transition_trigger ::=
   --     unique_port_identifier
   --   | self . event_source_identifier
   --   | processor . event_source_identifier

   function P_Unique_Port_Or_Transition_Trigger
     (Container : Types.Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

   begin
      case AADL_Version is
         when AADL_V1 =>
            return P_Entity_Reference (PC_Unique_Port_Identifier);

         when AADL_V2 =>
            return P_Mode_Transition_Trigger (Container);
      end case;
   end P_Unique_Port_Or_Transition_Trigger;

   --------------------------------
   -- P_Mode_Transition_Trigger --
   -------------------------------

   --  AADL_V2
   --  mode_transition_trigger ::=
   --     unique_port_identifier
   --   | self . event_source_identifier
   --   | processor . event_source_identifier

   function P_Mode_Transition_Trigger
     (Container : Types.Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Locations;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.Builder.AADL.Components.Modes;

      pragma Unreferenced (Container);

      Loc          : Location;
      Node         : Node_Id;
      Identifier   : Node_Id;
      Is_Self      : Boolean := False;
      Is_Processor : Boolean := False;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Self then
         Is_Self := True;
      elsif Token = T_Processor then
         Is_Processor := True;
      else
         Restore_Lexer (Loc);
      end if;

      if Is_Self or else Is_Processor then
         Scan_Token;
         if Token /= T_Dot then
            DPE (PC_Mode_Transition_Trigger, T_Dot);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      Identifier := P_Entity_Reference (PC_Mode_Transition_Trigger);
      if No (Identifier) then
         DPE (PC_Mode_Transition_Trigger, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Node :=
        Add_New_Mode_Transition_Trigger
          (Loc,
           Identifier,
           Is_Self,
           Is_Processor);
      if No (Node) then
         DPE (PC_Mode_Transition_Trigger);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      return Node;

   end P_Mode_Transition_Trigger;

end Ocarina.FE_AADL.Parser.Components.Modes;
