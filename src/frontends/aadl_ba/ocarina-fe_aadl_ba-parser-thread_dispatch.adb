------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.FE_AADL_BA.PARSER.THREAD_DISPATCH                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

with Ocarina.FE_AADL_BA.Lexer;
with Ocarina.FE_AADL_BA.Parser.Identifiers;
with Ocarina.FE_AADL_BA.Parser.Expressions;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.Builder.Aadl_Ba.Thread_Dispatch;

package body Ocarina.FE_AADL_BA.Parser.Thread_Dispatch is

   use Locations;
   use Ocarina.FE_AADL_BA.Lexer;
   use Ocarina.FE_AADL_BA.Parser.Identifiers;
   use Ocarina.FE_AADL_BA.Parser.Expressions;

   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.Tokens;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   use Ocarina.Builder.Aadl_Ba.Thread_Dispatch;

   function P_Dispatch_Trigger_Condition
     (Container : Types.Node_Id)
    return Node_Id;

   function P_Dispatch_Conjunction
     (Container : Types.Node_Id)
    return Node_Id;

   --------------------------
   -- P_Dispatch_Condition --
   --------------------------

   --  dispatch_condition ::=
   --    on dispatch [ dispatch_trigger_condition ] [ frozen frozen_ports ]

   --  frozen_ports ::=
   --    in_port_name { , in_port_name }*

   function P_Dispatch_Condition (Container : Types.Node_Id)
     return Node_Id
   is
      Start_Loc                  : Location;
      Loc                        : Location;

      Frozen_Port_List           : List_Id   := No_List;
      Dispatch_Trigger_Condition : Node_Id;
      Dispatch_Condition         : Node_Id;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;  -- consume T_On

      Dispatch_Condition := Add_New_Dispatch_Condition_Thread (Start_Loc,
                                                               Container,
                                                               No_Node,
                                                               No_List);

      if No (Dispatch_Condition) then
         DPE (PC_Dispatch_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Dispatch then
         Restore_Lexer (Start_Loc);
         DPE (PC_Dispatch_Condition,
              Expected_Token => T_Dispatch);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Dispatch_Trigger_Condition := P_Dispatch_Trigger_Condition
                                       (Dispatch_Condition);

      if No (Dispatch_Trigger_Condition) then
         DPE (PC_Dispatch_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Frozen then
         Frozen_Port_List := P_Items_List (P_Identifier'Access,
                                           Dispatch_Condition,
                                           T_Comma);

         if Is_Empty (Frozen_Port_List) then
            Scan_Token;
            DPE (PC_Dispatch_Condition, Expected_Token => T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Add_New_Dispatch_Condition_Thread (Dispatch_Condition,
                                         Container,
                                         Dispatch_Trigger_Condition,
                                         Frozen_Port_List);

      return Dispatch_Condition;
   end P_Dispatch_Condition;

   ----------------------------------
   -- P_Dispatch_Trigger_Condition --
   ----------------------------------

   --  dispatch_trigger_condition ::=
   --    dispatch_trigger_logical_expression
   --  | provides_subprogram_access_name
   --  | stop
   --  | completion_relative_timeout_condition_and_catch
   --  | dispatch_relative_timeout_catch

   --  dispatch_trigger_logical_expression ::=
   --    dispatch_conjunction { or dispatch_conjunction }*

   --  completion_relative_timeout_condition_and_catch ::=
   --    timeout [ (event_port_name*) ] behavior_time

   --  dispatch_relative_timeout_catch ::=
   --    timeout

   function P_Dispatch_Trigger_Condition (Container : Types.Node_Id)
    return Node_Id
   is
      Start_Loc                  : Location;
      Loc                        : Location;
      Dispatch_Trigger_Condition : Node_Id;
      Behavior_Time              : Node_Id                := No_Node;
      Dispatch_Conjunction_List   : List_Id                := No_List;
      Trig_Kind                  : Dispatch_Trigger_Kind;
   begin
      Save_Lexer (Start_Loc);

      Scan_Token;
      case Token is
         when T_Timeout =>
            Trig_Kind := TRI_Timeout;

            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Real_Literal
              or else Token = T_Integer_Literal
            then
               Restore_Lexer (Loc);
               Behavior_Time := P_Behavior_Time (No_Node);

               if No (Behavior_Time) then
                  DPE (PC_Dispatch_Trigger_Condition, EMC_Failed);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
            else
               Restore_Lexer (Loc);
            end if;

         when T_Stop =>
            Trig_Kind := TRI_Stop;

         when T_Identifier =>
            Trig_Kind := TRI_No_Kind;
            Restore_Lexer (Start_Loc);
            Dispatch_Conjunction_List := P_Items_List
                                        (P_Dispatch_Conjunction'Access,
                                         No_Node,
                                         T_Or);
            if Is_Empty (Dispatch_Conjunction_List) then
               DPE (PC_Dispatch_Trigger_Condition,
                    EMC_List_Is_Empty);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when others =>
            Trig_Kind := TRI_No_Kind;
            Restore_Lexer (Start_Loc);
      end case;

      Dispatch_Trigger_Condition := Add_New_Dispatch_Trigger_Condition
                                    (Start_Loc,
                                     Container,
                                     Trig_Kind,
                                     Dispatch_Conjunction_List,
                                     Behavior_Time);

      if No (Dispatch_Trigger_Condition) then
         DPE (PC_Dispatch_Trigger_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Dispatch_Trigger_Condition;
      end if;

   end P_Dispatch_Trigger_Condition;

   ------------------------------------
   -- P_Dispatch_Conjunction --
   ------------------------------------

   --  dispatch_conjunction ::=
   --    dispatch_trigger { and dispatch_trigger }*

   --  dispatch_trigger ::=
   --    in_event_port_identifier
   --  | in_event_data_port_identifier

   function P_Dispatch_Conjunction
     (Container : Types.Node_Id)
     return Node_Id
   is
      Loc                         : Location;
      Dispatch_Conjunction_Node   : Node_Id := No_Node;
      Dispatch_Triggers           : List_Id  := No_List;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Identifier then
         Restore_Lexer (Loc);
         Dispatch_Triggers := P_Items_List (P_Identifier'Access,
                                           No_Node,
                                           T_And);

         if Is_Empty (Dispatch_Triggers) then
            Scan_Token;
            DPE (PC_Dispatch_Trigger, Expected_Token => T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         DPE (PC_Dispatch_Conjunction,
              Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      if not Is_Empty (Dispatch_Triggers) then
         Dispatch_Conjunction_Node := Add_New_Dispatch_Conjunction (Loc,
                                                           Container,
                                                           Dispatch_Triggers);
      end if;
      if No (Dispatch_Conjunction_Node) then
         DPE (PC_Dispatch_Conjunction, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Dispatch_Conjunction_Node;
      end if;

   end P_Dispatch_Conjunction;

end Ocarina.FE_AADL_BA.Parser.Thread_Dispatch;
