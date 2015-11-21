------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.FE_AADL.PARSER.COMPONENTS.CONNECTIONS               --
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
with Ocarina.FE_AADL.Lexer;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Components.Modes;
with Ocarina.FE_AADL.Parser.Properties;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.Tokens;

with Ocarina.Builder.AADL.Components.Connections;

package body Ocarina.FE_AADL.Parser.Components.Connections is

   function P_Connection_Reference (Code : Parsing_Code) return Types.Node_Id;

   ------------------
   -- P_Connection --
   ------------------

   --  AADL_V1
   --  connection ::=
   --     port_connection | parameter_connection | access_connection

   --  connection_refinement ::=   port_connection_refinement
   --                            | parameter_connection_refinement
   --                            | access_connection_refinement

   --  port_connection ::=   data_connection | event_connection
   --                      | event_data_connection | port_group_connection

   --  data_connection ::= [ defining_data_connection_identifier :]
   --     data port source_unique_port_identifier
   --               ( immediate_connection_symbol | delayed_connection_symbol )
   --               destination_unique_port_identifier
   --        [ { { property_association }+ } ] [ in_modes_and_transitions ] ;

   --  immediate_connection_symbol ::= ->
   --  delayed_connection_symbol   ::= ->>

   --  event_connection ::= [ defining_event_connection_identifier :]
   --     event port source_unique_port_identifier
   --             -> destination_unique_port_identifier
   --        [ { { property_association }+ } ] [ in_modes_and_transitions ] ;

   --  event_data_connection ::= [ defining_event_data_connection_identifier :]
   --     event data port source_unique_port_identifier
   --                  -> destination_unique_port_identifier
   --        [ { { property_association }+ } ] [ in_modes_and_transitions ] ;

   --  port_group_connection ::= [ defining_port_group_connection_identifier :]
   --     port group source_unique_port_group_identifier
   --             -> destination_unique_port_group_identifier
   --        [ { { property_association }+ } ] [ in_modes_and_transitions ] ;

   --  port_connection_refinement ::=
   --     connection_identifier : refined to
   --     ( data port | event port | event data port | port group )
   --        ( ( { { property_association }+ } [ in_modes_and_transitions ] )
   --          | in_modes_and_transitions ) ;

   --  parameter_connection ::= [ defining_parameter_connection_identifier :]
   --     parameter source_unique_parameter_identifier
   --            -> destination_unique_parameter_identifier
   --        [ { { property_association }+ } ] [ in_modes ] ;

   --  parameter_connection_refinement ::=
   --     connection_identifier : refined to parameter
   --       { { property_association }+ } [ in_modes ] ;

   --  access_connection ::= [ access_connection_identifier :]
   --     ( bus | data ) access unique_access provider_identifier
   --                        -> unique_access_requirer_identifier
   --       [ { { property_association }+ } ] [ in_modes ] ;

   --  access_connection_refinement ::=
   --     connection_identifier : refined to ( bus | data ) access
   --       { { property_association }+ } [ in_modes ] ;

   --  AADL_V2
   --  connection ::=
   --     [ defining_connection_identifier : ]
   --         ( feature_connection | port_connection | parameter_connection
   --           | access_connection | feature_group_connection )
   --          [ { { property_association }+ } ]
   --          [ in_modes_and_transitions ] ;

   --  connection_refinement ::=
   --     defining_connection_identifier : refined to
   --       [ feature_connection_refinement | port_connection_refinement
   --         | parameter_connection_refinement | access_connection_refinement
   --         | feature_group_connection_refinement ]
   --       [ { { property_association }+ } ]
   --       [ in_modes_and_transitions ] ;

   --  feature_connection ::=
   --     source_feature_reference connection_symbol
   --     destination_feature_reference

   --  connection_symbol ::=
   --     directional_connection_symbol | bidirectional_connection_symbol

   --  directional_connection_symbol ::= ->
   --  bidirectional_connection_symbol ::= <->

   --  feature_connection_refinement ::=
   --     source_feature_reference connection_symbol
   --     destination_feature_reference

   --  port_connection ::=
   --     port source_port_connection_reference connection_symbol
   --     destination_port_connection_reference

   --  port_connection_refinement ::=
   --     port [ source_port_connection_reference connection_symbol
   --     destination_port_connection_reference ]

   --  Note: data port, event data port, and event port connections
   --  are replaced by port connections in AADL V2

   --  parameter_connection ::=
   --     parameter source_parameter_reference directional_connection_symbol
   --     destination_parameter_reference

   --  parameter_connection_refinement ::=
   --     parameter

   --  access_connection ::=
   --     [ bus | subprogram | subprogram group | data ] access
   --     access_provider_reference connection_symbol access_requirer_reference

   --  access_connection_refinement ::=
   --     [ bus | subprogram | subprogram group | data ] access

   --  feature_group_connection ::=
   --     feature group source_feature_group_reference
   --       bidirectional_connection_symbol destination_feature_group_reference

   --  feature_group_connection_refinement ::=
   --     feature group

   function P_Connection
     (Container : Types.Node_Id;
      Refinable : Boolean) return Types.Node_Id
   is
      use Locations;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Components.Modes;
      use Ocarina.FE_AADL.Parser.Properties;
      use Ocarina.Builder.AADL.Components.Connections;

      Connection     : Node_Id;
      Identifier     : Node_Id;
      Is_Refinement  : Boolean;
      Is_Bidirect    : Boolean  := False;
      Category       : Connection_Type;
      Source         : Node_Id  := No_Node;
      Destination    : Node_Id  := No_Node;
      In_Modes       : Node_Id;
      OK             : Boolean;
      Code           : Parsing_Code;
      Loc            : Location;
      Start_Loc      : Location := No_Location;
      Properties_Loc : Location;

   begin
      P_Identifier_Refined_To
        (Refinable_To_RT (Refinable),
         True,
         PC_Connection,
         PC_Connection_Refinement,
         T_Semicolon,
         Identifier,
         Is_Refinement,
         OK);
      if not OK then
         return No_Node;
      end if;

      if Present (Identifier) then
         --  update Start_Loc
         Start_Loc := Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier);
      end if;

      if Is_Refinement then
         Code := PC_Connection_Refinement;
      else
         Code := PC_Connection;
      end if;

      --  parsing connection category
      Save_Lexer (Loc);
      Scan_Token;

      if Start_Loc = No_Location then
         Start_Loc := Token_Location;
      end if;

      case Token is
         when T_Data =>        --  data_connection or access_connection
            Scan_Token;

            case Token is
               when T_Port =>      --  data_connection
                  if AADL_Version = AADL_V1 then
                     Category := CT_Data;

                     if Is_Refinement then
                        Code := PC_Data_Connection_Refinement;
                     else
                        Code := PC_Data_Connection;
                     end if;
                  else
                     DPE (PC_Data_Connection, EMC_Not_Allowed_In_AADL_V2);
                     Skip_Tokens (T_Semicolon);
                     return No_Node;
                  end if;

               when T_Access =>    --  access_connection
                  Category := CT_Access_Data;
                  if Is_Refinement then
                     Code := PC_Access_Connection_Refinement;
                  else
                     Code := PC_Access_Connection;
                  end if;

               when others =>
                  DPE (Code, (T_Port, T_Access));
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
            end case;

         when T_Event =>       --  event_connection or event_data_connection
            if AADL_Version = AADL_V1 then
               Scan_Token;
               case Token is
                  when T_Data =>      --  event_data_connection
                     if Is_Refinement then
                        Code := PC_Event_Data_Connection_Refinement;
                     else
                        Code := PC_Event_Data_Connection;
                     end if;

                     Scan_Token;
                     if Token /= T_Port then
                        DPE (Code, T_Port);
                        Skip_Tokens (T_Semicolon);
                        return No_Node;
                     end if;

                     Category := CT_Event_Data;

                  when T_Port =>      --  event_connection

                     Category := CT_Event;
                     if Is_Refinement then
                        Code := PC_Event_Connection_Refinement;
                     else
                        Code := PC_Event_Connection;
                     end if;

                  when others =>
                     DPE (PC_Event_Data_Connection, ((T_Data, T_Port)));
                     Skip_Tokens (T_Semicolon);
                     return No_Node;
               end case;
            else
               DPE (PC_Event_Data_Connection, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Port =>  --  port_group_connection
            if AADL_Version = AADL_V1 then
               if Is_Refinement then
                  Code := PC_Feature_Group_Connection_Refinement;
               else
                  Code := PC_Feature_Group_Connection;
               end if;

               Scan_Token;
               if Token /= T_Group then
                  DPE (Code, T_Group);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Category := CT_Feature_Group;
            else
               if Is_Refinement then
                  Code := PC_Port_Connection_Refinement;
               else
                  Code := PC_Port_Connection;
               end if;
               Category := CT_Port_Connection;
               --  represent event, event data and data connection in AADL_V2

            end if;

         when T_Feature =>  --  feature_group_connection
            if AADL_Version = AADL_V2 then
               if Is_Refinement then
                  Code := PC_Feature_Group_Connection_Refinement;
               else
                  Code := PC_Feature_Group_Connection;
               end if;

               Save_Lexer (Loc);
               Scan_Token;
               if Token /= T_Group then
                  Category := CT_Feature;
                  Restore_Lexer (Loc);
               else
                  Category := CT_Feature_Group;
               end if;

            else
               DPE (PC_Feature_Group_Connection, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Parameter =>   --  parameter_connection
            Category := CT_Parameter;
            if Is_Refinement then
               Code := PC_Parameter_Connection_Refinement;
            else
               Code := PC_Parameter_Connection;
            end if;

         when T_Bus =>         --  access_connection
            if Is_Refinement then
               Code := PC_Access_Connection_Refinement;
            else
               Code := PC_Access_Connection;
            end if;

            Scan_Token;
            if Token /= T_Access then
               DPE (Code, T_Access);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Category := CT_Access_Bus;

         when T_Subprogram =>
            if AADL_Version = AADL_V2 then

               if Is_Refinement then
                  Code := PC_Access_Connection_Refinement;
               else
                  Code := PC_Access_Connection;
               end if;

               Scan_Token;
               if Token = T_Group then
                  Category := CT_Access_Subprogram_Group;
                  Scan_Token;
               else
                  Category := CT_Access_Subprogram;
               end if;

               if Token /= T_Access then
                  DPE (Code, T_Access);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            else
               DPE (PC_Connection, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Virtual =>
            Scan_Token;
            if Token /= T_Bus then
               DPE (PC_Connection, T_Bus);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Scan_Token;
            if Token /= T_Access then
               DPE (Code, T_Access);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Category := CT_Access_Virtual_Bus;

         when T_Access =>
            --  In AADLv2, we can have just
            if AADL_Version = AADL_V2 then
               Category := CT_Access;

            else
               raise Program_Error;
            end if;

         when others =>
            if Present (Identifier) then
               case AADL_Version is
                  when AADL_V1 =>
                     DPE (Code, (T_Data, T_Event, T_Bus, T_Port, T_Parameter));
                  when AADL_V2 =>
                     DPE
                       (Code,
                        (T_Data,
                         T_Bus,
                         T_Port,
                         T_Parameter,
                         T_Subprogram,
                         T_Virtual));
               end case;
               Skip_Tokens (T_Semicolon);
               return No_Node;
            else
               --  Nothing was parsed, try to parse other stuff

               --  XXX WRONG WRONG WRONG, should be either none, or
               --  raise an error

               Restore_Lexer (Loc);
               return No_Node;
            end if;
      end case;

      --  parse connection source et destination

      if not Is_Refinement then
         Source := P_Connection_Reference (Code);

         if No (Source) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Direct_Connection then
            case Token is
               when T_Delayed_Connection =>
                  Category := CT_Data_Delayed;

               when T_Bidirect_Connection =>
                  Is_Bidirect := True;

               when others =>
                  if AADL_Version = AADL_V1 then
                     DPE (Code, (T_Direct_Connection, T_Delayed_Connection));
                  else
                     DPE (Code, (T_Direct_Connection, T_Bidirect_Connection));
                  end if;
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
            end case;
         end if;

         Destination := P_Connection_Reference (Code);

         if No (Destination) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      --  parse properties and in_modes

      Save_Lexer (Properties_Loc);
      --  We save the location of the properties, as we have to parse
      --  it later (after the connection node has been created)

      Connection :=
        Add_New_Connection
          (Loc           => Start_Loc,
           Name          => Identifier,
           Comp_Impl     => Container,
           Category      => Category,
           Destination   => Destination,
           Source        => Source,
           Is_Refinement => Is_Refinement,
           Is_Bidirect   => Is_Bidirect,
           In_Modes      => No_Node);

      OK := P_Property_Associations (Connection, True, PAT_Simple, Code);

      if not OK then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if No (Ocarina.ME_AADL.AADL_Tree.Nodes.Properties (Connection)) then
         --  Property_Associations are not defined,
         --  In_Modes_And_Transitions must exist if Is_Refinement = TRUE

         if Aadl_Version = AADL_V1
           and then Is_Refinement and then Token /= T_In
         then
            DPE (Code, (T_In, T_Left_Curly_Bracket));
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      if Token = T_In then
         case Category is
            when CT_Data         |
              CT_Data_Delayed    |
              CT_Port_Connection |
              CT_Event_Data      |
              CT_Event           |
              CT_Feature         |
              CT_Feature_Group   =>
               In_Modes := P_In_Modes (PC_In_Modes_And_Transitions);

            when CT_Parameter            |
              CT_Access_Bus              |
              CT_Access_Data             |
              CT_Access_Subprogram       |
              CT_Access_Subprogram_Group |
              CT_Access_Virtual_Bus      |
              CT_Access                  =>
               In_Modes := P_In_Modes (PC_In_Modes);

            when CT_Error =>
               raise Program_Error;
         end case;

         if No (In_Modes) then
            --  error when parsing In_Modes, quit

            Skip_Tokens (T_Semicolon);
            return No_Node;
         else
            Set_In_Modes (Connection, In_Modes);
         end if;
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

      Save_Lexer (Loc);
      Restore_Lexer (Properties_Loc);
      Restore_Lexer (Loc);
      return Connection;
   end P_Connection;

   ----------------------------
   -- P_Connection_Reference --
   ----------------------------

   --  feature_reference ::=
   --     component_type_feature_identifier |
   --     component_type_feature_group_identifier . feature_identifier |
   --     subcomponent_identifier . feature_identifier

   --  port_connection_reference ::=
   --     component_type_port_identifier |
   --     subcomponent_identifier . port_identifier |
   --     component_type_feature_group_identifier . element_port_identifier |
   --     component_type_port_identifier . data_element_identifier |
   --     component_type_requires_data_access_identifier |
   --     data_subcomponent_identifier |
   --     subcomponent_identifier . provides_data_access_identifier |
   --     component_type_feat._group_identifier.elt_data_access_identifier |
   --     data_subcomponent_identifier .data_subcomponent_identifier |
   --     processor . processor_port_identifier |
   --     self . event_or_event_data_source_identifier

   --  parameter_reference ::=
   --     component_type_parameter_identifier [ . parameter_identifier ] |
   --     subprogram_call_identifier . parameter_identifier |
   --     component_type_port_identifier [ . data_subcomponent_identifier ] |
   --     data_subcomponent_identifier |
   --     requires_data_access_identifier |
   --     component_type_feat._group_identifier.elt_data_access_identifier |
   --     component_type_feat._group_identifier [ . element_port_identifier ]

   --  access_reference ::=
   --     requires_access_identifier | provides_access_identifier |
   --     feature_group_identifier [ . requires_access_identifier ] |
   --     feature_group_identifier [ . provides_access_identifier ] |
   --     subcomponent_identifier . provides_access_identifier |
   --     subcomponent_identifier . requires_access_identifier |
   --     data_subprogram_subprogam_group_or_bus_subcomponent_identifier |
   --     processor . provides_subprogram_access_identifier

   --  feature_group_reference ::=
   --     component_type_feature_group_identifier |
   --     subcomponent_identifier . feature_group_identifier |
   --     component_type_feature_group_identifier .
   --     element_feature_group_identifier

   function P_Connection_Reference
     (Code : Parsing_Code) return Types.Node_Id
   is
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
         when T_Processor | T_Self =>
            if AADL_Version = AADL_V2 then
               Scan_Token;
               if Token /= T_Dot then
                  DPE (PC_Connection_Reference, T_Dot);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
            else
               DPE (PC_Connection_Reference, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Identifier =>
            Restore_Lexer (Loc);

         when others =>
            DPE (PC_Connection_Reference, (T_Processor, T_Identifier));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Node := P_Entity_Reference (Code);
      if No (Node) then
         DPE (PC_Connection_Reference);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      return Node;

   end P_Connection_Reference;

end Ocarina.FE_AADL.Parser.Components.Connections;
