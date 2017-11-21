------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . F E _ A O 4 A A D L . P A R S E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

with Ocarina.Parser;
with Ocarina.FE_AO4AADL.Lexer;
with Ocarina.FE_AO4AADL.Parser_Errors;
with Ocarina.ME_AO4AADL.Tokens;
with Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes;
with Ocarina.ME_AO4AADL.AO4AADL_Tree.Nutils;
with Ocarina.AO4AADL_Values;

package body Ocarina.FE_AO4AADL.Parser is
   use Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes;
   use Ocarina.ME_AO4AADL.AO4AADL_Tree.Nutils;
   use Ocarina.ME_AO4AADL.Tokens;
   use Ocarina.FE_AO4AADL.Lexer;
   use Ocarina.FE_AO4AADL.Parser_Errors;
   use Ocarina.AO4AADL_Values;

   Language     : constant String := "ao4aadl";
   Aspect_Annex : Node_Id;

   procedure P_Specification;
   function P_Aspect_Precedence return Node_Id;
   function P_Aspect_Expression return Node_Id;
   function P_Components_Applied_Tos return Node_Id;
   function P_Component return Node_Id;
   function P_Component_Category return Ocarina.ME_AO4AADL.Component_Category;
   function P_Identifier return Node_Id;
   function P_Pointcut_Specification return Node_Id;
   function P_Advice_Specification return Node_Id;
   function P_Parameter_Specification return Node_Id;
   function P_Pointcut_Expression return Node_Id;
   function P_Primitive_Pointcut return Node_Id;
   function P_Caller return Node_Id;
   function P_Callee return Node_Id;
   function P_Subprogram return Node_Id;
   function P_Port return Node_Id;
   function P_Args return Node_Id;
   function P_Advice_Declaration return Node_Id;
   function P_Pointcut_Reference return Node_Id;
   function P_Advice_Category return Ocarina.ME_AO4AADL.Advice_Category;
   function P_Advice_Action return Node_Id;
   function P_Variables_Declaration return Node_Id;
   function P_Variable return Node_Id;
   function P_Initialisation return Node_Id;
   function P_Assignment return Node_Id;
   function P_Action return Node_Id;
   function P_If_Statement return Node_Id;
   function P_For_Statement return Node_Id;
   function P_While_Statement return Node_Id;
   function P_Basic_Action return Node_Id;
   function P_Conditional_Statement return Node_Id;
   function P_Behavior_Expression return Node_Id;
   function P_Relation return Node_Id;
   function P_Simple_Expressions return List_Id;
   function P_Simple_Expression return Node_Id;
   function P_Operator return Node_Id;
   function P_Term return Node_Id;
   function P_Factor return Node_Id;
   function P_Primary return Node_Id;
   function P_Integer_Range return Node_Id;
   function P_Integer_Value return Node_Id;
   function P_Property_Constant return Node_Id;
   function P_Assignment_Or_Communication_Action return Node_Id;
   function P_Timed_Action return Node_Id;
   function P_Proceed_Action return Node_Id;

   -------------
   -- Process --
   -------------

   function Process (AADL_Root : Node_Id;
                     From      : Location;
                     To        : Location := No_Location;
                     Container : Node_Id  := No_Node)
                    return Node_Id
   is
      pragma Unreferenced (AADL_Root, From, To, Container);
   begin
      P_Specification;
      --  FIXME: We must parse the annex and link it to the component

      return Aspect_Annex;
   end Process;

   ---------------------
   -- P_Specification --
   ---------------------

   --  Aspect_Annex ::= { Aspect_Expression }

   procedure P_Specification
   is
      Aspect_Expressions : List_Id;
      Aspect_Expression  : Node_Id;
      Next        : Token_Type;
      Loc : Location;
   begin
      Aspect_Annex := New_Node (K_Aspect_Annex, Token_Location);
      Aspect_Expressions   := New_List (K_List_Id, Token_Location);
      Set_Aspect_Expressions (Aspect_Annex,  Aspect_Expressions);

      --  Scanning aspect expressions
      Save_Lexer (Loc);
      Next := Next_Token;
      while Next = T_Aspect loop
         Restore_Lexer (Loc);
         Aspect_Expression := P_Aspect_Expression;
         Append_Node_To_List (Aspect_Expression, Aspect_Expressions);
         Save_Lexer (Loc);
         Next := Next_Token;
      end loop;

   end P_Specification;

   ---------------------
   -- P_Aspect_Expression --
   ---------------------

   --  Aspect_Expression ::= aspect Aspect_Identifier {
   --                       [ Components_applied_to; ]
   --                       { Pointcut_Specification; }+
   --                       { Advice_Specification }+
   --                       [ Aspect_Precedence ]
   --                     }

   function P_Aspect_Expression return Node_Id is
      Identifier : Node_Id;
      Components_Applied_Tos : Node_Id;
      Pointcuts_Specification : List_Id;
      Pointcut_Specification : Node_Id;
      Advices_Specification : List_Id;
      Advice_Specification : Node_Id;
      Precedence : Node_Id;
      Aspect_Expr : Node_Id;
      Next : Token_Type;
      Loc : Location;
   begin
      Scan_Token; --  past "aspect"
      Aspect_Expr := New_Node (K_Aspect_Expression, Token_Location);

      Identifier := P_Identifier;
      if No (Identifier) then
         DPE (PC_Aspect_Declaration, Expected_Token => T_Identifier);
         return No_Node;
      end if;
      Set_Identifier (Aspect_Expr, Identifier);

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         DPE (PC_Aspect_Declaration, Expected_Token => T_Left_Brace);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      if Next_Token = T_Applied then
         Scan_Token; --  past "applied"
         if Next_Token = T_To then
            Restore_Lexer (Loc);
            Components_Applied_Tos := P_Components_Applied_Tos;
            Set_Components_Applied_Tos (Aspect_Expr, Components_Applied_Tos);
         end if;
      end if;

      --  Scanning Precedence
      if Next_Token = T_Precedence then
         Scan_Token; --  past "precedence"
         Restore_Lexer (Loc);
         Precedence := P_Aspect_Precedence;
         Set_Aspect_Precedence (Aspect_Expr, Precedence);
      end if;

      Pointcuts_Specification := New_List (K_List_Id, Token_Location);
      Set_Pointcut_Specification (Aspect_Expr,  Pointcuts_Specification);

      --  Scanning pointcuts

      Save_Lexer (Loc);
      Next := Next_Token;
      while Next = T_Pointcut loop
         Restore_Lexer (Loc);
         Pointcut_Specification := P_Pointcut_Specification;
         Append_Node_To_List (Pointcut_Specification, Pointcuts_Specification);
         Save_Lexer (Loc);
         Next := Next_Token;
      end loop;

      Advices_Specification := New_List (K_List_Id, Token_Location);
      Set_Advice_Specification (Aspect_Expr,  Advices_Specification);

      --  Scanning advices
      while Next = T_Advice loop
         Restore_Lexer (Loc);
         Advice_Specification := P_Advice_Specification;
         Append_Node_To_List (Advice_Specification, Advices_Specification);
         Save_Lexer (Loc);
         Next := Next_Token;
      end loop;

      Scan_Token (T_Right_Brace);
      if Token = T_Error then
         DPE (PC_Aspect_Declaration, Expected_Token => T_Right_Brace);
         return No_Node;
      end if;

      return Aspect_Expr;
   end P_Aspect_Expression;

   ------------------------
   -- P_Aspect_Precedence --
   -------------------------

   --  Aspect_Precedence ::= precedence Aspect_Identifier
   --                                  { , Aspect_Identifier }*

   function P_Aspect_Precedence return Node_Id is
      Aspect_Identifiers : List_Id;
      Aspect_Identifier  : Node_Id;
      Node_Prec          : Node_Id;

   begin
      Scan_Token; -- past "precedence"
      Node_Prec := New_Node (K_Aspect_Precedence, Token_Location);
      Aspect_Identifiers := New_List (K_List_Id, Token_Location);
      Set_Identifiers (Node_Prec, Aspect_Identifiers);
      loop
         Aspect_Identifier := P_Identifier;
         if No (Aspect_Identifier) then
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;

         Append_Node_To_List (Aspect_Identifier, Aspect_Identifiers);

         exit when Next_Token /= T_Comma;
         Scan_Token; -- past ','
      end loop;

      return Node_Prec;
   end P_Aspect_Precedence;

   ------------------------------
   -- P_Components_Applied_Tos --
   ------------------------------

   --  Components_applied_to ::= applied to ListComponents
   --  ListComponents ::= Component { ,Component }*

   function P_Components_Applied_Tos return Node_Id is
      Comp_App_To : Node_Id;
      Components : List_Id;
      Component : Node_Id;
   begin
      Comp_App_To := New_Node (K_Components_Applied_To, Token_Location);
      Components := New_List (K_List_Id, Token_Location);
      Set_Components (Comp_App_To, Components);
      loop
         Component := P_Component;
         if No (Component) then
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;

         Append_Node_To_List (Component, Components);

         exit when Next_Token /= T_Comma;
         Scan_Token; --  past ','
      end loop;
      Scan_Token (T_Semi_Colon);
      return Comp_App_To;
   end P_Components_Applied_Tos;

   -----------------
   -- P_Component --
   -----------------

   --  Component ::= Component_Category Component_Identifier

   function P_Component return Node_Id is
      use Ocarina.ME_AO4AADL;
      Component : Node_Id;
      Category : Ocarina.ME_AO4AADL.Component_Category;
      Component_Identifier : Node_Id;
   begin
      Component := New_Node (K_Component, Token_Location);
      Scan_Token;
      Category := P_Component_Category;
      if Category = CC_Unknown then
         null;
      else
         Set_Component_Category
           (Component,
            Ocarina.ME_AO4AADL.Component_Category'Pos (Category));
      end if;

      Component_Identifier := P_Identifier;
      Set_Identifier (Component_Identifier, Component);

      return Component;
   end P_Component;

   --------------------------
   -- P_Component_Category --
   --------------------------

   --  Component_Category ::= thread | process | subprogram | system

   function P_Component_Category
     return Ocarina.ME_AO4AADL.Component_Category
   is
      use Ocarina.ME_AO4AADL;
   begin
      case Token is
         when T_Thread =>
            return CC_Thread;

         when T_Process =>
            return CC_Process;

         when T_Subprogram =>
            return CC_Subprogram;

         when T_System =>
            return CC_System;

         when others =>
            return CC_Unknown;
      end case;
   end P_Component_Category;

   ------------------
   -- P_Identifier --
   ------------------

   function P_Identifier return Node_Id is
      Identifier : Node_Id;
   begin
      Scan_Token;

      if Token /= T_Identifier then
         return No_Node;
      end if;

      Identifier := New_Node (K_Identifier, Token_Location);
      Set_Name (Identifier, Token_Name);
      return Identifier;
   end P_Identifier;

   ------------------------------
   -- P_Pointcut_Specification --
   ------------------------------

   --  Pointcut_Specification ::= pointcut Pointcut_Identifier
   --                        ( [ ParamList ] ):
   --                        Pointcut_Expression

   --  ParamList ::= Parameter_Specification { ,Parameter_Specification }*

   function P_Pointcut_Specification return Node_Id is
      Identifier : Node_Id;
      Pointcut : Node_Id;
      Parameters : List_Id;
      Parameter : Node_Id;
      Pointcut_Expression : Node_Id;
      State : Location;
   begin
      Scan_Token; -- past "pointcut"
      Pointcut := New_Node (K_Pointcut_Specification, Token_Location);
      Identifier := P_Identifier;
      Set_Identifier (Pointcut, Identifier);
      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         DPE (PC_Pointcut_Specification, Expected_Token => T_Left_Paren);
         return No_Node;
      end if;

      Parameters := New_List (K_List_Id, Token_Location);
      Set_Parameters (Pointcut, Parameters);

      loop
         Save_Lexer (State);
         Parameter := P_Parameter_Specification;
         if No (Parameter) then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            exit;
         end if;

         Append_Node_To_List (Parameter, Parameters);
         Save_Lexer (State);
         Scan_Token (T_Comma);
         if Token /= T_Comma then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            if Token /= T_Right_Paren then
               Token := T_Error;
               DPE (PC_Pointcut_Specification,
                    Expected_Token => T_Right_Paren);
            end if;
            exit;
         end if;
      end loop;

      if Next_Token = T_Colon then
         Scan_Token; --  past ':'
         Pointcut_Expression := P_Pointcut_Expression;
         Set_Pointcut_Expression (Pointcut, Pointcut_Expression);
         Save_Lexer (State);
      else
         DPE (PC_Pointcut_Specification, Expected_Token => T_Colon);
      end if;

      return Pointcut;
   end P_Pointcut_Specification;

   ------------------------------
   -- P_Parameter_Specification --
   ------------------------------

   --  Parameter_Specification ::= Parameter_Identifier : Type_Identifier

   function P_Parameter_Specification return Node_Id is
      Param_Specification : Node_Id;
      Param_Identifier  : Node_Id;
      Type_Identifier   : Node_Id;
      Param_Location    : Location;

   begin
      Param_Location := Token_Location;
      Param_Specification :=
        New_Node (K_Parameter_Specification, Param_Location);

      Param_Identifier := P_Identifier;
      if No (Param_Identifier) then
         return No_Node;
      end if;
      Set_Identifier (Param_Specification, Param_Identifier);

      Scan_Token (T_Colon);

      Type_Identifier := P_Identifier;

      if No (Type_Identifier) then
         return No_Node;
      end if;
      Set_Type_Identifier (Param_Specification, Type_Identifier);

      return Param_Specification;

   end P_Parameter_Specification;

   ------------------------------
   -- P_Pointcut_Expression --
   ------------------------------

   --  Pointcut_Expression ::= Pointcut_Primitive | ( Pointcut_Expression )
   --                   | Pointcut_Expression && Pointcut_Expression
   --                   | Pointcut_Expression || Pointcut_Expression

   function P_Pointcut_Expression return Node_Id is
      use Ocarina.ME_AO4AADL;
      Pointcut_Expr : Node_Id;
      Caller  : Node_Id;
      P_Expr   : Node_Id := No_Node;
      Loc    : Location;
      Escape        : Boolean       := False;
      Operator_Node : Node_Id := No_Node;
      Operat_Kind   : Ocarina.ME_AO4AADL.Operator_Kind;

   begin

      Pointcut_Expr := New_Node (K_Pointcut_Expression, Token_Location);

      Caller := P_Primitive_Pointcut;
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Concat =>
            Operat_Kind := OK_Concat;
         when T_Or_Logic =>
            Operat_Kind := OK_Or_Logic;
         when others =>
            Escape := True;
      end case;

      if Escape then

         Restore_Lexer (Loc);
         Scan_Token (T_Semi_Colon);
      else

         Operator_Node :=  New_Node (K_Operator, Token_Location);
         Set_Operator_Kind (Operator_Node,
                            Ocarina.ME_AO4AADL.Operator_Kind'Pos
                              (Operat_Kind));
         P_Expr := P_Pointcut_Expression;

      end if;
      Set_Caller (Pointcut_Expr, Caller);
      Set_Operator (Pointcut_Expr, Operator_Node);
      Set_Pointcut_Expression (Pointcut_Expr, P_Expr);

      if No (Pointcut_Expr) then
         DPE (PC_Value_Expression, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Pointcut_Expr;
      end if;
   end P_Pointcut_Expression;

   ------------------------------
   -- P_Primitive_Pointcut --
   ------------------------------

   function P_Primitive_Pointcut return Node_Id is
      Caller : Node_Id;
      Loc_Caller : Location;
   begin
      Save_Lexer (Loc_Caller);
      Scan_Token;
      if Token = T_Execution or else
        Token = T_Call
      then
         Restore_Lexer (Loc_Caller);
         Caller := P_Caller;
      elsif Token = T_Args then
         Caller := P_Args;
      end if;
      return Caller;
   end P_Primitive_Pointcut;

   ------------------------------
   -- P_Caller --
   ------------------------------

   function P_Caller return Node_Id is
      use Ocarina.ME_AO4AADL;
      Caller : Node_Id;
      Loc : Location;
      Caller_K : Ocarina.ME_AO4AADL.Caller_Kind;
      Callee : Node_Id;

   begin
      Caller := New_Node (K_Caller, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;
      case Token is
         when T_Call =>
            Caller_K := CK_Call;
            Set_Caller_Kind
              (Caller, Ocarina.ME_AO4AADL.Caller_Kind'Pos (Caller_K));
            Save_Lexer (Loc);
            Callee := P_Callee;
            Set_Callee (Caller, Callee);

         when T_Execution =>
            Caller_K := CK_Execution;
            Set_Caller_Kind
              (Caller, Ocarina.ME_AO4AADL.Caller_Kind'Pos (Caller_K));
            Save_Lexer (Loc);
            Callee := P_Callee;
            Set_Callee (Caller, Callee);

         when others =>
            Restore_Lexer (Loc);
            Caller := No_Node;
      end case;

      return Caller;
   end P_Caller;

   ------------------------------
   -- P_Callee --
   ------------------------------

   --  Callee ::= subprogram ( Subprogram_Identifier
   --                          ( [ Subprogram_Parameter_Types ] ) )
   --      | inport ( Input_Port_Identifier ( [ Data_Type ] ) )
   --      | outport ( Ouput_Port_Identifier ( [ Data_Type ] ) )
   --      | inoutport ( InOuput_Port_Identifier ( [ Data_Type ] ) )

   function P_Callee return Node_Id is
      Callee : Node_Id;
      Loc : Location;

   begin
      Callee := New_Node (K_Callee, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;
      case Token is
         when T_Subprogram =>
            Restore_Lexer (Loc);
            Callee := P_Subprogram;

         when T_Inport | T_Outport | T_InOutport =>
            Restore_Lexer (Loc);
            Callee := P_Port;

         when others =>
            Restore_Lexer (Loc);
            Callee := No_Node;
      end case;

      return Callee;
   end P_Callee;

   ------------------------------
   -- P_Subprogram --
   ------------------------------

   --  Callee ::= subprogram
   --  ( Subprogram_Identifier ( [ Subprogram_Parameter_Types ] ) )

   function P_Subprogram return Node_Id is
      Subprogram_Call : Node_Id;
      Subprogram_Identifier : Node_Id;
      Parameters : List_Id;
      Parameter : Node_Id;
      State : Location;
      Loc : Location;

   begin
      Scan_Token; --  past "subprogram"
      Subprogram_Call := New_Node (K_Subprogram_Call, Token_Location);
      Scan_Token (T_Left_Paren);
      Save_Lexer (Loc);
      Scan_Token;
      Restore_Lexer (Loc);

      case Token is
         when T_Identifier => Subprogram_Identifier := P_Identifier;
         when T_Star =>  Subprogram_Identifier :=
            New_Node (K_Star, Token_Location);
            Scan_Token (T_Star);
         when others =>
            DPE (PC_Pointcut_Specification,
                 Expected_Tokens => (T_Identifier,
                                     T_Star));
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;

      Set_Identifier (Subprogram_Call, Subprogram_Identifier);
      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         return No_Node;
      end if;

      Parameters := New_List (K_List_Id, Token_Location);
      Set_Parameters (Subprogram_Call, Parameters);

      loop
         Save_Lexer (State);
         Scan_Token;
         Restore_Lexer (State);
         case Token is
            when T_Identifier => Parameter := P_Identifier;
            when T_Interval =>  Parameter :=
               New_Node (K_Interval, Token_Location);
               Scan_Token (T_Interval);
            when others =>
               DPE (PC_Pointcut_Specification,
                    Expected_Tokens => (T_Identifier,
                                        T_Interval));
               Skip_Declaration (T_Semi_Colon);
               return No_Node;
         end case;

         Append_Node_To_List (Parameter, Parameters);

         Save_Lexer (State);
         Scan_Token ((T_Comma, T_Right_Paren));
         if Token /= T_Comma then
            if Token = T_Error then
               Restore_Lexer (State);
               Scan_Token (T_Right_Paren);
            end if;
            exit;
         end if;
      end loop;
      Scan_Token (T_Right_Paren);

      return Subprogram_Call;

   end P_Subprogram;

   ------------------------------
   -- P_Port --
   ------------------------------

   --  Callee ::= inport ( Input_Port_Identifier ( [ Data_Type ] ) )
   --      | outport ( Ouput_Port_Identifier ( [ Data_Type ] ) )
   --      | inoutport ( InOuput_Port_Identifier ( [ Data_Type ] ) )

   function P_Port return Node_Id is
      Port_Call : Node_Id;
      Port_Identifier : Node_Id;
      Mode : Mode_Id;
      Parameters : List_Id;
      Parameter : Node_Id;
      State : Location;
      Loc : Location;

   begin
      Scan_Token;
      case Token is
         when T_Inport =>
            Mode := Mode_In;

         when T_Outport =>
            Mode := Mode_Out;

         when T_InOutport =>
            Mode := Mode_InOut;

         when others =>
            null;
      end case;
      Port_Call := New_Node (K_Port_Call, Token_Location);
      Set_Mode (Port_Call, Mode);
      Scan_Token (T_Left_Paren);
      Save_Lexer (Loc);
      Scan_Token;
      Restore_Lexer (Loc);
      case Token is
         when T_Identifier => Port_Identifier := P_Identifier;
         when T_Star =>  Port_Identifier :=
            New_Node (K_Star, Token_Location);
            Scan_Token (T_Star);
         when others =>
            DPE (PC_Pointcut_Specification,
                 Expected_Tokens => (T_Identifier,
                                     T_Star));
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;

      Set_Identifier (Port_Call, Port_Identifier);

      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         return No_Node;
      end if;

      Parameters := New_List (K_List_Id, Token_Location);
      Set_Parameters (Port_Call, Parameters);

      loop
         Save_Lexer (State);
         Scan_Token;
         Restore_Lexer (State);
         case Token is
            when T_Identifier => Parameter := P_Identifier;
            when T_Interval =>  Parameter :=
               New_Node (K_Interval, Token_Location);
               Scan_Token (T_Interval);
            when others =>
               DPE (PC_Pointcut_Specification,
                    Expected_Tokens => (T_Identifier,
                                        T_Interval));
               Skip_Declaration (T_Semi_Colon);
               return No_Node;
         end case;

         Append_Node_To_List (Parameter, Parameters);

         Save_Lexer (State);
         Scan_Token ((T_Comma, T_Right_Paren));
         if Token /= T_Comma then
            if Token = T_Error then
               Restore_Lexer (State);
               Scan_Token (T_Right_Paren);
            end if;
            exit;
         end if;
      end loop;
      Scan_Token (T_Right_Paren);

      return Port_Call;
   end P_Port;

   ------------------------------
   -- P_Args --
   ------------------------------

   function P_Args return Node_Id is
      Args : Node_Id;
      Arguments : List_Id;
      Argument : Node_Id;
      State : Location;

   begin
      Args := New_Node (K_Args, Token_Location);
      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         return No_Node;
      end if;

      Arguments := New_List (K_List_Id, Token_Location);
      Set_Arguments (Args, Arguments);

      loop
         Save_Lexer (State);
         Scan_Token;
         Restore_Lexer (State);
         case Token is
            when T_Identifier => Argument := P_Identifier;
            when T_Interval =>  Argument :=
               New_Node (K_Interval, Token_Location);
               Scan_Token (T_Interval);
            when others =>
               DPE (PC_Pointcut_Specification,
                    Expected_Tokens => (T_Identifier,
                                        T_Interval));
               Skip_Declaration (T_Semi_Colon);
               return No_Node;
         end case;

         Append_Node_To_List (Argument, Arguments);

         Save_Lexer (State);
         Scan_Token ((T_Comma, T_Right_Paren));
         if Token /= T_Comma then
            if Token = T_Error then
               Restore_Lexer (State);
               Scan_Token (T_Right_Paren);
            end if;
            exit;
         end if;
      end loop;

      return Args;
   end P_Args;

   ------------------------------
   -- P_Advice_Specification --
   ------------------------------

   --  Advice_Specification ::= advice Advice_Declaration : Pointcut_Reference
   --                      Advice_Action

   function P_Advice_Specification return Node_Id is

      Advice_Specification : Node_Id;
      Advice_Declaration  : Node_Id;
      Pointcut_Reference : Node_Id;
      Advice_Action   : Node_Id;
      Loc    : Location;

   begin
      Advice_Specification := New_Node
        (K_Advice_Specification, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;

      while Token /= T_Colon loop
         Scan_Token;
      end loop;

      Restore_Lexer (Loc);
      Advice_Declaration := P_Advice_Declaration;
      Set_Advice_Declaration (Advice_Specification,  Advice_Declaration);
      Scan_Token (T_Colon);
      Save_Lexer (Loc);

      while Token /= T_Left_Brace  loop
         Scan_Token;
      end loop;

      Restore_Lexer (Loc);
      Pointcut_Reference := P_Pointcut_Reference;
      Set_Pointcut_Reference (Advice_Specification,  Pointcut_Reference);
      Scan_Token (T_Left_Brace);
      Save_Lexer (Loc);

      while Token /= T_Right_Brace loop
         Scan_Token;
      end loop;

      Restore_Lexer (Loc);
      Advice_Action := P_Advice_Action;
      Set_Advice_Action (Advice_Specification, Advice_Action);
      Scan_Token (T_Right_Brace);
      return Advice_Specification;
   end P_Advice_Specification;

   ------------------------------
   --  P_Advice_Declaration --
   ------------------------------

   --  Advice_Declaration ::= Before_Advice | After_Advice | Around_Advice

   --  Before_Advice::= before ([Paramlist])

   --  After_Advice::= after ([Paramlist])

   --  Around_Advice::= around ([Paramlist])

   function P_Advice_Declaration return Node_Id is
      use Ocarina.ME_AO4AADL;
      Advice_Declaration : Node_Id;
      Category : Ocarina.ME_AO4AADL.Advice_Category;
      Parameter : Node_Id;
      Parameters : List_Id;
      State : Location;
   begin
      Advice_Declaration := New_Node (K_Advice_Declaration,
                                      Token_Location);
      Scan_Token;
      Category := P_Advice_Category;
      if Category = AC_Unknown then
         null;
      else
         Set_Advice_Category
           (Advice_Declaration,
            Ocarina.ME_AO4AADL.Advice_Category'Pos (Category));
      end if;
      Scan_Token (T_Left_Paren);

      Parameters := New_List (K_List_Id, Token_Location);
      Set_Parameters (Advice_Declaration, Parameters);

      loop
         Save_Lexer (State);
         Parameter := P_Parameter_Specification;
         if No (Parameter) then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            exit;
         end if;

         Append_Node_To_List (Parameter, Parameters);
         Save_Lexer (State);
         Scan_Token (T_Comma);
         if Token /= T_Comma then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            if Token /= T_Right_Paren then
               Token := T_Error;
            end if;
            exit;
         end if;
      end loop;

      return Advice_Declaration;
   end P_Advice_Declaration;

   -------------------------
   -- P_Advice_Category--
   -------------------------

   --  Advice_Category ::= before | after | around

   function P_Advice_Category return Ocarina.ME_AO4AADL.Advice_Category is
      use Ocarina.ME_AO4AADL;
   begin
      Scan_Token;
      case Token is
         when T_Before =>
            return AC_Before;

         when T_After =>
            return AC_After;

         when T_Around =>
            return AC_Around;

         when others =>
            return AC_Unknown;
      end case;
   end P_Advice_Category;

   --------------------------
   -- P_Pointcut_Reference --
   --------------------------

   function P_Pointcut_Reference return Node_Id is
      Pointcut_Identifier : Node_Id;
      Pointcut_Reference : Node_Id;
      Parameters : List_Id;
      Parameter : Node_Id;
      State : Location;
   begin
      Pointcut_Reference := New_Node (K_Pointcut_Reference, Token_Location);
      Pointcut_Identifier := P_Identifier;
      Set_Identifier (Pointcut_Reference, Pointcut_Identifier);
      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         return No_Node;
      end if;

      Parameters := New_List (K_List_Id, Token_Location);
      Set_Parameter_Profile (Pointcut_Reference, Parameters);

      loop
         Save_Lexer (State);
         Parameter := P_Identifier;
         if No (Parameter) then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            exit;
         end if;

         Append_Node_To_List (Parameter, Parameters);
         Save_Lexer (State);
         Scan_Token (T_Comma);
         if Token /= T_Comma then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            if Token /= T_Right_Paren then
               Token := T_Error;
            end if;
            exit;
         end if;
      end loop;
      return Pointcut_Reference;
   end P_Pointcut_Reference;

   ------------------------------
   --  P_Advice_Action--
   ------------------------------

   function P_Advice_Action return Node_Id is
      Advice_Action : Node_Id;
      Variables_Declaration : Node_Id;
      Initialisation : Node_Id;
      Actions : List_Id;
      Action : Node_Id;
      Loc : Location;
   begin
      Advice_Action := New_Node (K_Advice_Action, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Right_Brace then
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      if Token = T_Variables then
         Variables_Declaration := P_Variables_Declaration;
         Set_Variables_Declaration (Advice_Action, Variables_Declaration);
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Initially then
         Initialisation := P_Initialisation;
         Set_Initialisation (Advice_Action, Initialisation);
      else
         Restore_Lexer (Loc);
      end if;

      Actions := New_List (K_List_Id, Token_Location);

      loop
         Save_Lexer (Loc);
         Action := P_Action;
         if No (Action) then
            Restore_Lexer (Loc);
            exit;
         end if;
         Append_Node_To_List (Action, Actions);
      end loop;
      Set_Actions (Advice_Action, Actions);

      return Advice_Action;
   end P_Advice_Action;

   ------------------------------
   -- P_Variables_Declaration --
   ------------------------------

   function P_Variables_Declaration return Node_Id is
      Variables_Declaration : Node_Id;
      List_Variables : List_Id;
      Variable : Node_Id;
      Loc : Location;
   begin
      Variables_Declaration :=
        New_Node (K_Variables_declaration, Token_Location);
      List_Variables := New_List (K_List_Id, Token_Location);
      Set_Variables (Variables_Declaration, List_Variables);

      Scan_Token (T_Left_Brace);
      Save_Lexer (Loc);

      loop
         Save_Lexer (Loc);
         Variable := P_Variable;
         if No (Variable) then
            Restore_Lexer (Loc);
            Scan_Token (T_Right_Brace);
            exit;
         end if;

         Append_Node_To_List (Variable, List_Variables);
         Save_Lexer (Loc);
         Scan_Token (T_Semi_Colon);
         if Token /= T_Semi_Colon then
            Restore_Lexer (Loc);
            Token := T_Error;
            exit;
         end if;
         if Next_Token = T_Right_Brace then
            exit;
         end if;
      end loop;

      Scan_Token (T_Right_Brace);

      return Variables_Declaration;

   end P_Variables_Declaration;

   -------------------------
   -- P_Variable --
   -------------------------

   --  variable ::=
   --    local_variable_identifier { , local_variable_identifier }*
   --      : Type_Identifier;

   function P_Variable return Node_Id
   is
      Loc               : Location;
      Identifiers       : List_Id;
      Identifier : Node_Id;
      Type_Identifier : Node_Id;
      Variable : Node_Id;
   begin
      Variable := New_Node (K_Variable, Token_Location);

      Identifiers := New_List (K_List_Id, Token_Location);

      loop
         Save_Lexer (Loc);
         Identifier := P_Identifier;
         if No (Identifier) then
            Restore_Lexer (Loc);
            exit;
         end if;

         Append_Node_To_List (Identifier, Identifiers);
         Save_Lexer (Loc);
         Scan_Token (T_Comma);
         if Token /= T_Comma then
            Restore_Lexer (Loc);
            exit;
         end if;
      end loop;
      Set_Identifiers (Variable, Identifiers);

      Save_Lexer (Loc);
      Scan_Token (T_Colon);

      if Token = T_Colon then
         Type_Identifier := P_Identifier;
         if No (Type_Identifier) then
            DPE (PC_Behavior_Variable, EMC_Unique_Classifier_Ref);
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;

         Set_Type_Identifier (Variable, Type_Identifier);
      else
         Restore_Lexer (Loc);
      end if;

      return Variable;

   end P_Variable;

   ------------------------------
   -- P_Initialisation --
   ------------------------------

   function P_Initialisation return Node_Id is
      Initialisation : Node_Id;
      List_Assignments : List_Id;
      Assignment : Node_Id;
      Loc : Location;
   begin
      Initialisation :=
        New_Node (K_Initialisation, Token_Location);
      List_Assignments := New_List (K_List_Id, Token_Location);
      Set_Assignments (Initialisation, List_Assignments);

      Scan_Token (T_Left_Brace);
      Save_Lexer (Loc);

      loop
         Save_Lexer (Loc);
         Assignment := P_Assignment;
         if No (Assignment) then
            Restore_Lexer (Loc);
            Scan_Token (T_Right_Brace);
            exit;
         end if;

         Append_Node_To_List (Assignment, List_Assignments);
         Save_Lexer (Loc);
         Scan_Token (T_Semi_Colon);
         if Token /= T_Semi_Colon then
            Restore_Lexer (Loc);
            Token := T_Error;
            exit;
         end if;
         if Next_Token = T_Right_Brace then
            exit;
         end if;
      end loop;

      Scan_Token (T_Right_Brace);

      return Initialisation;
   end P_Initialisation;

   ------------------------------
   -- P_Assignment --
   ------------------------------

   function P_Assignment return Node_Id is
      Assignment : Node_Id;
      Reference_Expression : Node_Id;
      Behavior_Expression : Node_Id;
   begin
      Assignment :=
        New_Node (K_Assignment_Action, Token_Location);
      Reference_Expression := P_Identifier;
      Set_Reference_Expression (Assignment, Reference_Expression);
      Scan_Token (T_Assignment);

      Scan_Token;
      case Token is
         when T_False =>
            Behavior_Expression :=
              New_Node (K_Boolean_Literal, Token_Location);
            Set_Is_True (Behavior_Expression, False);

         when T_True =>
            Behavior_Expression :=
              New_Node (K_Boolean_Literal, Token_Location);
            Set_Is_True (Behavior_Expression, True);

         when T_Real_Literal =>
            Behavior_Expression := New_Node (K_Literal, Token_Location);
            Set_Value (Behavior_Expression,
                       New_Real_Value (Real_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));

         when T_Integer_Literal =>
            Behavior_Expression := New_Node (K_Literal, Token_Location);
            Set_Value (Behavior_Expression,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));

         when T_String_Literal =>
            Behavior_Expression := New_Node (K_Literal, Token_Location);
            Set_Value (Behavior_Expression,
                       New_String_Value (String_Literal_Value));
         when others =>
            DPE (PC_Assignment,
                 Expected_Tokens => (T_False,
                                     T_True,
                                     T_Real_Literal,
                                     T_Integer_Literal,
                                     T_String_Literal));
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;

      Set_Behavior_Expression (Assignment, Behavior_Expression);

      return Assignment;
   end P_Assignment;

   ------------------------------
   -- P_Action --
   ------------------------------
   --  Action ::=
   --    Basic_Action ;
   --  | if ( logical_expression ) { behavior_action }+
   --    { elsif ( logical_expression ) { behavior_action }+ }*
   --    [ else { behavior_action }+ ]
   --    end if ;
   --  | for ( loop_variable_identifier in range )
   --           { { behavior_action }+ };
   --  | while ( logical_expression ) { { behavior_action }+ };

   function P_Action return Node_Id is
      Loc            : Location;

      Action          : Node_Id;
      Action_Node : Node_Id;
   begin
      Action :=  New_Node (K_Action, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;
      Restore_Lexer (Loc);

      case Token is
         when T_If =>
            Action_Node := P_If_Statement;

         when T_For =>
            Action_Node := P_For_Statement;

         when T_While =>
            Action_Node := P_While_Statement;

         when T_Identifier | T_Computation | T_Delay | T_Proceed =>
            Action_Node := P_Basic_Action;

         when others =>
            if Token /= T_Right_Brace then
               DPE (PC_Behavior_Action,
                    Expected_Tokens => (T_If, T_For, T_While,
                                        T_Identifier, T_Computation,
                                        T_Delay, T_Proceed));
               Skip_Declaration (T_Semi_Colon);
            end if;
            return No_Node;
      end case;

      Set_Action_Node (Action, Action_Node);

      if No (Action) then
         DPE (PC_Behavior_Action, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Action;
      end if;
   end P_Action;

   ----------------------
   -- P_If_Statement --
   ----------------------

   function P_If_Statement return Node_Id is
      Loc            : Location;
      If_Statement : Node_Id;
      If_Node        : Node_Id;
      Elsif_Node     : Node_Id := No_Node;
      Else_Node      : Node_Id := No_Node;
   begin
      If_Statement :=  New_Node (K_If_Statement, Token_Location);
      Scan_Token;

      if Token /= T_If then
         DPE (PC_If_Cond_Struct,
              Expected_Token => T_If);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      If_Node := P_Conditional_Statement;
      if No (If_Node) then
         DPE (PC_If_Cond_Struct, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Elsif then
         Elsif_Node := P_Conditional_Statement;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Else then
         Else_Node := P_Conditional_Statement;
      else
         Restore_Lexer (Loc);
         Scan_Token;
      end if;

      Set_If_Cond (If_Statement, If_Node);
      Set_Elsif_Cond (If_Statement, Elsif_Node);
      Set_Else_Cond (If_Statement, Else_Node);

      return If_Statement;
   end P_If_Statement;

   -----------------------------
   -- P_Conditional_Statement --
   -----------------------------

   function P_Conditional_Statement return Node_Id
   is
      Loc            : Location;
      Cond_Stat_Node : Node_Id;
      Behavior_Expression     : Node_Id := No_Node;
      Action : Node_Id;
      Actions        : List_Id := No_List;
   begin
      Cond_Stat_Node :=  New_Node (K_Conditional_Statement, Token_Location);
      Actions := New_List (K_List_Id, Token_Location);
      if Token /= T_Else then
         Scan_Token (T_Left_Paren);

         if Token /= T_Left_Paren then
            DPE (PC_Conditional_Statement,
                 Expected_Token => T_Left_Paren);
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;

         Behavior_Expression := P_Behavior_Expression;
         if No (Behavior_Expression) then
            DPE (PC_Conditional_Statement, EMC_Failed);
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;

         Scan_Token (T_Right_Paren);
         if Token /= T_Right_Paren then
            DPE (PC_Conditional_Statement,
                 Expected_Token => T_Right_Paren);
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;
      end if;

      Scan_Token (T_Left_Brace);

      loop
         Save_Lexer (Loc);
         Action := P_Action;
         if No (Action) then
            Restore_Lexer (Loc);
            exit;
         end if;
         Append_Node_To_List (Action, Actions);
      end loop;
      Set_Behavior_Expression (Cond_Stat_Node, Behavior_Expression);
      Set_Actions (Cond_Stat_Node, Actions);
      Scan_Token (T_Right_Brace);

      if No (Cond_Stat_Node) then
         DPE (PC_Conditional_Statement, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Cond_Stat_Node;
      end if;
   end P_Conditional_Statement;

   -----------------------
   -- P_For_Statement --
   -----------------------

   function P_For_Statement return Node_Id is
      Loc : Location;
      For_Statement : Node_Id;
      Var_Identifier  : Node_Id;
      Range_Node      : Node_Id;
      Actions         : List_Id;
      Action : Node_Id;
   begin
      For_Statement := New_Node (K_For_Statement, Token_Location);
      Actions := New_List (K_List_Id, Token_Location);
      Set_Actions (For_Statement, Actions);
      if No (For_Statement) then
         DPE (PC_For_Cond_Struct, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;
      Scan_Token (T_For);
      Scan_Token;
      if Token /= T_Left_Paren then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Left_Paren);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Var_Identifier := P_Identifier;
      if No (Var_Identifier) then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Identifier);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Set_Loop_Variable_Identifier (For_Statement, Var_Identifier);

      Scan_Token;
      if Token /= T_In then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_In);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Range_Node := P_Integer_Range;
      if No (Range_Node) then
         DPE (PC_For_Cond_Struct, EMC_Invalid_Range);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Set_Integer_Range (For_Statement, Range_Node);

      Scan_Token;
      if Token /= T_Right_Paren then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Right_Paren);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Brace then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Left_Brace);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      loop
         Restore_Lexer (Loc);
         Action := P_Action;
         if No (Action) then
            exit;
         end if;

         Append_Node_To_List (Action, Actions);
         Save_Lexer (Loc);
      end loop;

      if Is_Empty (Actions) then
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Brace then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Right_Brace);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      return For_Statement;

   end P_For_Statement;

   -------------------------
   -- P_While_Statement --
   -------------------------

   function P_While_Statement return Node_Id is
      While_Statement : Node_Id;
      While_Node        : Node_Id;
   begin
      While_Statement :=  New_Node (K_While_Statement, Token_Location);
      Scan_Token;

      if Token /= T_While then
         DPE (PC_While_Cond_Struct,
              Expected_Token => T_If);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      While_Node := P_Conditional_Statement;
      if No (While_Node) then
         DPE (PC_While_Cond_Struct, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Set_Conditional_Statement (While_Statement, While_Node);

      return While_Statement;

   end P_While_Statement;

   ----------------------
   -- Behavior_Expression --
   ----------------------

   --  Behavior_Expression ::= relation { logical_operator relation}*

   function P_Behavior_Expression return Node_Id is
      use Ocarina.ME_AO4AADL;
      Loc           : Location;
      Value_Expr    : Node_Id;
      Item          : Node_Id;
      Relation_List : List_Id;
      Escape        : Boolean       := False;
      Operator_Node : Node_Id;
      Operat_Kind   : Ocarina.ME_AO4AADL.Operator_Kind;
      First_Parsing   : Boolean   := True;
   begin
      Save_Lexer (Loc);
      Value_Expr :=  New_Node (K_Behavior_Expression, Token_Location);
      Relation_List := New_List (K_List_Id, Token_Location);
      Set_Relations (Value_Expr, Relation_List);

      loop
         if not First_Parsing then
            Save_Lexer (Loc);
            Scan_Token;
            if Token in Operator_Type then
               Item := P_Operator;
               if Present (Item) then
                  Append_Node_To_List (Item, Relation_List);
               else
                  exit;
               end if;
            else
               Restore_Lexer (Loc);
               exit;
            end if;
         else
            First_Parsing := False;
         end if;

         Item := P_Relation;
         if Present (Item) then
            Append_Node_To_List (Item, Relation_List);
         else
            Relation_List := No_List;
            exit;
         end if;

         Save_Lexer (Loc);
         Scan_Token;

         case Token is
            when T_And =>
               Operat_Kind := OK_And;
            when T_Or =>
               Operat_Kind := OK_Or;
            when others =>
               Escape := True;
         end case;

         if Escape then
            Restore_Lexer (Loc);
            exit;
         else

            Operator_Node :=  New_Node (K_Operator, Token_Location);
            Set_Operator_Kind (Operator_Node,
                               Ocarina.ME_AO4AADL.Operator_Kind'Pos
                                 (Operat_Kind));

            if No (Operator_Node) then
               DPE (PC_Value_Expression, EMC_Failed);
               return No_Node;
            end if;

            Append_Node_To_List (Operator_Node, Relation_List);
         end if;
      end loop;

      if Is_Empty (Relation_List) then
         DPE (PC_Value_Expression, EMC_List_Is_Empty);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      if No (Value_Expr) then
         DPE (PC_Value_Expression, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Value_Expr;
      end if;

   end P_Behavior_Expression;

   ----------------
   -- P_Relation --
   ----------------

   --  relation ::=
   --    boolean_value
   --  | simple_expression [relational_operator simple_expression]

   function P_Relation return Node_Id is
      Loc     : Location;
      Bool_Val      : Boolean  := False;
      Spl_Expr_List : List_Id  := No_List;
      Node          : Node_Id;
   begin
      Node :=  New_Node (K_Relation, Token_Location);
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_True =>
            Bool_Val := True;
            Set_Boolean_Value (Node, Bool_Val);

         when T_False =>
            Bool_Val := False;
            Set_Boolean_Value (Node, Bool_Val);

         when others =>
            Restore_Lexer (Loc);
            Spl_Expr_List := P_Simple_Expressions;
            Set_Simple_Exprs (Node, Spl_Expr_List);
      end case;

      if Is_Empty (Spl_Expr_List) then
         DPE (PC_Relation, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Node;
      end if;

   end P_Relation;

   --------------------------
   -- P_Simple_Expressions --
   --------------------------

   --  simple_expression [relational_operator simple_expression]

   function P_Simple_Expressions return List_Id is
      Loc             : Location;
      Item            : Node_Id;
      Expression_List : List_Id;
      First_Parsing   : Boolean   := True;
   begin
      Expression_List := New_List (K_List_Id, Token_Location);

      loop
         if not First_Parsing then
            Save_Lexer (Loc);
            Scan_Token;
            if Token in Operator_Type then
               Item := P_Operator;
               if Present (Item) then
                  Append_Node_To_List (Item, Expression_List);
               else
                  DPE (PC_Simple_Expressions, EMC_Failed);
                  Expression_List := No_List;
                  exit;
               end if;
            else
               Restore_Lexer (Loc);
               exit;
            end if;
         else
            First_Parsing := False;
         end if;

         Item := P_Simple_Expression;
         if Present (Item) then
            Append_Node_To_List (Item, Expression_List);
         else
            DPE (PC_Simple_Expressions, EMC_Failed);
            Expression_List := No_List;
            exit;
         end if;

      end loop;

      if Is_Empty (Expression_List) then
         DPE (PC_Simple_Expressions, EMC_List_Is_Empty);
         Skip_Declaration (T_Semi_Colon);
         return No_List;
      end if;

      return Expression_List;

   end P_Simple_Expressions;

   -------------------------
   -- P_Simple_Expression --
   -------------------------

   --  simple_expression ::=
   --    [unary_adding_operator] term {binary_adding_operator term}*

   function P_Simple_Expression return Node_Id is
      Start_Loc        : Location;
      Simple_Expr_Node : Node_Id;
      Operator_Node    : Node_Id;
      Term_Node        : Node_Id;
      Terms_And_Operators_List : List_Id;
      First_Parsing : Boolean := True;
   begin
      Simple_Expr_Node := New_Node (K_Simple_Expression, Token_Location);
      Terms_And_Operators_List := New_List (K_List_Id, Token_Location);

      loop
         if not First_Parsing then
            Save_Lexer (Start_Loc);
            Scan_Token;

            if Token in Unary_Adding_Operator then
               Operator_Node := P_Operator;
               if Present (Operator_Node) then
                  Append_Node_To_List
                    (Operator_Node, Terms_And_Operators_List);
               else
                  DPE (PC_Simple_Expression, EMC_Failed);
                  Skip_Declaration (T_Semi_Colon);
                  exit;
               end if;
            else
               Restore_Lexer (Start_Loc);
               exit;
            end if;
         else
            First_Parsing := False;
         end if;

         Term_Node := P_Term;
         if Present (Term_Node) then
            Append_Node_To_List (Term_Node, Terms_And_Operators_List);
         else
            DPE (PC_Simple_Expression, EMC_Failed);
            Skip_Declaration (T_Semi_Colon);
            exit;
         end if;
      end loop;

      if Is_Empty (Terms_And_Operators_List) then
         DPE (PC_Simple_Expressions, EMC_List_Is_Empty);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Set_Term_And_Operator (Simple_Expr_Node, Terms_And_Operators_List);

      if No (Simple_Expr_Node) then
         DPE (PC_Simple_Expression, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Simple_Expr_Node;
      end if;

   end P_Simple_Expression;

   ------------
   -- P_Term --
   ------------

   --  term ::= factor {multiplying_operator factor}*

   function P_Term return Node_Id is
      Start_Loc     : Location;
      Loc           : Location;
      Term_Node     : Node_Id;
      Factor_Node   : Node_Id;
      Operator_Node : Node_Id;
      Factor_List   : List_Id;
      First_Parsing : Boolean := True;
   begin
      Term_Node := New_Node (K_Term, Token_Location);
      Save_Lexer (Start_Loc);
      Factor_List := New_List (K_List_Id, Token_Location);
      Set_Factors (Term_Node, Factor_List);

      loop
         if not First_Parsing then
            Save_Lexer (Loc);
            Scan_Token;
            if Token in Multiplying_Operator then
               Operator_Node := P_Operator;
               if Present (Operator_Node) then
                  Append_Node_To_List (Operator_Node, Factor_List);
               else
                  DPE (PC_Term, EMC_Failed);
                  Factor_List := No_List;
                  exit;
               end if;
            else
               Restore_Lexer (Loc);
               exit;
            end if;
         else
            First_Parsing := False;
         end if;

         Factor_Node := P_Factor;
         if Present (Factor_Node) then
            Append_Node_To_List (Factor_Node, Factor_List);
         else
            DPE (PC_Term, EMC_Failed);
            Factor_List := No_List;
            exit;
         end if;

      end loop;

      if Is_Empty (Factor_List) then
         DPE (PC_Term, EMC_List_Is_Empty);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      if No (Term_Node) then
         DPE (PC_Term, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Term_Node;
      end if;

   end P_Term;

   --------------
   -- P_Factor --
   --------------

   --  factor ::= primary [** primary] | abs primary | not primary

   function P_Factor return Node_Id is
      Start_Loc   : Location;
      Factor_Node : Node_Id;
      Low_Primary : Node_Id;
      Is_Not_Bool : Boolean  := False;
   begin
      Factor_Node := New_Node (K_Factor, Token_Location);
      Save_Lexer (Start_Loc);
      Scan_Token;

      if Token = T_Not then
         Is_Not_Bool := True;
      else
         Restore_Lexer (Start_Loc);
      end if;
      Set_Is_Not (Factor_Node, Is_Not_Bool);

      Low_Primary := P_Primary;
      if No (Low_Primary) then
         DPE (PC_Factor, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;
      Set_Lower_Primary (Factor_Node, Low_Primary);

      if No (Factor_Node) then
         DPE (PC_Factor, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Factor_Node;
      end if;

   end P_Factor;

   ---------------
   -- P_Primary --
   ---------------

   --  primary ::= value_holder | value_constant | ( value_expression )

   function P_Primary return Node_Id is
      Loc  : Location;
      Node : Node_Id;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_False =>
            Node := New_Node (K_Boolean_Literal, Token_Location);
            Set_Is_True (Node, False);

         when T_True =>
            Node := New_Node (K_Boolean_Literal, Token_Location);
            Set_Is_True (Node, True);

         when T_Real_Literal =>
            Node := New_Node (K_Literal, Token_Location);
            Set_Value (Node,
                       New_Real_Value (Real_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));

         when T_Integer_Literal =>
            Node := New_Node (K_Literal, Token_Location);
            Set_Value (Node,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));

         when T_String_Literal =>
            Node := New_Node (K_Literal, Token_Location);
            Set_Value (Node,
                       New_String_Value (String_Literal_Value));

         when T_Identifier =>
            Restore_Lexer (Loc);
            Node := P_Identifier;

         when T_Left_Paren =>
            Node := P_Behavior_Expression;
            Scan_Token;
            if Token /= T_Right_Paren then
               DPE (PC_Primary, Expected_Token => T_Right_Paren);
               Skip_Declaration (T_Semi_Colon);
               return No_Node;
            end if;

         when others =>
            DPE (PC_Primary,
                 Expected_Tokens => (T_False, T_True, T_Identifier,
                                     T_Real_Literal, T_Integer_Literal));
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;

      if No (Node) then
         DPE (PC_Primary, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Node;
      end if;

   end P_Primary;

   ----------------
   -- P_Operator --
   ----------------

   function P_Operator return Node_Id is
      use Ocarina.ME_AO4AADL;
      Operator_Cat  : Ocarina.ME_AO4AADL.Operator_Kind;
      Operator_Node : Node_Id;
      Loc           : Location;

      --  fixme : todo add generic procedure for test operator kind
   begin
      Operator_Node :=  New_Node (K_Operator, Token_Location);
      Save_Lexer (Loc);

      case Token is
         --  relational operator
         when T_Equal =>
            Operator_Cat := OK_Equal;

         when T_Non_Equal =>
            Operator_Cat := OK_Non_Equal;

         when T_Less_Than =>
            Operator_Cat := OK_Less_Than;

         when T_Greater_Than =>
            Operator_Cat := OK_Greater_Than;

         when T_Greater_Or_Equal =>
            Operator_Cat := OK_Less_Or_Equal;

         when T_Less_Or_Equal =>
            Operator_Cat := OK_Greater_Or_Equal;

            --  unary and Binary adding operator
         when T_Plus =>
            Operator_Cat := OK_Plus;

         when T_Minus =>
            Operator_Cat := OK_Minus;

         when T_Concat =>
            Operator_Cat := OK_Concat;

            --  multiplying operator
         when T_Star =>
            Operator_Cat := OK_Multiply;

         when T_Divide =>
            Operator_Cat := OK_Divide;

         when T_Not =>
            Operator_Cat := Ok_Not;

         when T_Or_Logic =>
            Operator_Cat := Ok_Or_Logic;

         when others =>
            Restore_Lexer (Loc);
            DPE (PC_Operator, EMC_Operator_Unknown);
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;

      Set_Operator_Kind (Operator_Node,
                         Ocarina.ME_AO4AADL.Operator_Kind'Pos (Operator_Cat));

      if No (Operator_Node) then
         DPE (PC_Operator, EMC_Failed);
         return No_Node;
      end if;

      return Operator_Node;

   end P_Operator;

   -------------------------
   -- P_Property_Constant --
   -------------------------

   --  property_constant ::=
   --    [ property_set_identifier :: ] property_constant_identifier

   function P_Property_Constant return Node_Id is
      Start_Loc       : Location;
      Loc             : Location;

      Property_Cst_Node : Node_Id;
      Property_Cst_Id   : Node_Id;
      Property_Set_Id   : Node_Id := No_Node;
   begin
      Property_Cst_Node := New_Node (K_Property_Constant, Token_Location);
      Save_Lexer (Start_Loc);

      Property_Cst_Id := P_Identifier;
      if No (Property_Cst_Id) then
         DPE (PC_Property_Constant, Expected_Token => T_Identifier);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Colon_Colon then
         Property_Set_Id := Property_Cst_Id;
         Property_Cst_Id := P_Identifier;
      else
         Restore_Lexer (Loc);
      end if;

      Set_Property_Set (Property_Cst_Node, Property_Set_Id);
      Set_Identifier (Property_Cst_Node, Property_Cst_Id);

      if No (Property_Cst_Node) then
         DPE (PC_Property_Constant, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Property_Cst_Node;
      end if;
   end P_Property_Constant;

   ---------------------
   -- P_Integer_Range --
   ---------------------

   --  integer_range ::= integer_value .. integer_value

   function P_Integer_Range return Node_Id is
      Start_Loc     : Location;
      Integer_Range : Node_Id;
      Lower_Bound   : Node_Id;
      Upper_Bound   : Node_Id;
   begin
      Integer_Range := New_Node (K_Integer_Range, Token_Location);
      Save_Lexer (Start_Loc);

      Lower_Bound := P_Integer_Value;
      if No (Lower_Bound) then
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Interval then
         DPE (PC_Integer_Range, Expected_Token => T_Interval);
         return No_Node;
      end if;

      Upper_Bound := P_Integer_Value;

      if No (Upper_Bound) then
         return No_Node;
      end if;

      Set_Upper_Int_Val (Integer_Range, Upper_Bound);
      Set_Lower_Int_Val (Integer_Range, Lower_Bound);

      if No (Integer_Range) then
         DPE (PC_Integer_Range, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Integer_Range;
      end if;

   end P_Integer_Range;

   ---------------------
   -- P_Integer_Value --
   ---------------------

   --  integer_value ::=
   --    integer_value_holder
   --  | integer_numerical_literal
   --  | integer_property_constant

   function P_Integer_Value return Node_Id is
      Start_Loc   : Location;

      Integer_Cst  : Node_Id;
      Integer_Val  : Node_Id;

      --  fixme : todo parse integer_value_holder
   begin
      Integer_Val := New_Node (K_Integer_Value, Token_Location);
      Save_Lexer (Start_Loc);
      Scan_Token;

      case Token is
         when T_Identifier =>
            Integer_Cst := P_Property_Constant;

         when T_Real_Literal =>
            Integer_Cst := New_Node (K_Literal, Token_Location);
            Set_Value (Integer_Cst,
                       New_Real_Value (Real_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));

         when T_Integer_Literal =>
            Integer_Cst := New_Node (K_Literal, Token_Location);
            Set_Value (Integer_Cst,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));

         when others =>
            DPE (PC_Integer_Value,
                 Expected_Tokens => (T_Identifier,
                                     T_Real_Literal,
                                     T_Integer_Literal));
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;

      Set_Entity (Integer_Val, Integer_Cst);

      if No (Integer_Val) then
         DPE (PC_Integer_Value, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Integer_Val;
      end if;

   end P_Integer_Value;

   --------------------
   -- P_Basic_Action --
   --------------------

   --  basic_action ::=
   --    assignment_action
   --  | communication_action
   --  | timed_action
   --  | proceed

   function P_Basic_Action return Node_Id is
   begin
      case Token is
         when T_Identifier =>
            return P_Assignment_Or_Communication_Action;

         when T_Delay | T_Computation =>
            return P_Timed_Action;

         when T_Proceed =>
            return P_Proceed_Action;

         when others =>
            DPE (PC_Basic_Action,
                 Expected_Tokens => (T_Identifier,
                                     T_Delay, T_Computation, T_Proceed));
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;
   end P_Basic_Action;

   ------------------------------------------
   -- P_Assignment_Or_Communication_Action --
   ------------------------------------------

   function P_Assignment_Or_Communication_Action return Node_Id
   is
      use Ocarina.ME_AO4AADL;
      Start_Loc : Location;
      State : Location;
      Loc               : Location;
      --  Loc2              : Location;
      Node              : Node_Id;
      Ident             : Node_Id;
      Value_Expr        : Node_Id            := No_Node;
      --  Target            : Node_Id            := No_Node;
      Sub_Parameters    : List_Id            := No_List;
      Comm_Kind          : Ocarina.ME_AO4AADL.Com_Kind := CK_Unknown;
      Parameter : Node_Id;
      Parameter_Prof : Node_Id;
   begin
      Save_Lexer (Start_Loc);
      Ident := P_Identifier;

      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Exclamation =>
            Comm_Kind := CK_Exclamation;

         when T_Interrogative =>
            Comm_Kind := CK_Interrogation;

         when T_Assignment =>
            Save_Lexer (Loc);
            Scan_Token;
            Restore_Lexer (Loc);
            Value_Expr := P_Behavior_Expression;
            if No (Value_Expr) then
               DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
               Skip_Declaration (T_Semi_Colon);
               return No_Node;
            end if;

         when others =>
            DPE (PC_Assignment_Or_Communication_Action,
                 Expected_Tokens => (T_Exclamation,
                                     T_Interrogative,
                                     T_Assignment));
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
      end case;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Paren then
         if Comm_Kind = CK_Exclamation
           or else Comm_Kind = CK_Interrogation
         then
            Sub_Parameters := New_List (K_List_Id, Token_Location);

            loop
               Save_Lexer (State);
               Parameter_Prof := New_Node
                 (K_Parameter_Profile, Token_Location);
               Parameter := P_Behavior_Expression;
               Set_Behavior_Expression (Parameter_Prof, Parameter);
               if No (Parameter) then
                  Restore_Lexer (State);
                  Scan_Token (T_Right_Paren);
                  exit;
               end if;

               Append_Node_To_List (Parameter_Prof, Sub_Parameters);
               Save_Lexer (State);
               Scan_Token (T_Comma);
               if Token /= T_Comma then
                  Restore_Lexer (State);
                  exit;
               end if;
            end loop;

            if Is_Empty (Sub_Parameters) then
               DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
               Skip_Declaration (T_Semi_Colon);
               return No_Node;
            end if;
         end if;

         Scan_Token;
         if Token /= T_Right_Paren then
            DPE (PC_Assignment_Or_Communication_Action,
                 Expected_Token => T_Right_Paren);
            Skip_Declaration (T_Semi_Colon);
            return No_Node;
         end if;

      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Assignment_Or_Communication_Action,
              Expected_Token => T_Semi_Colon);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      end if;

      if No (Value_Expr) then
         Node := New_Node (K_Communication_Action, Start_Loc);
         Set_Identifier (Node, Ident);
         Set_Parameter_Profile (Node, Sub_Parameters);
         Set_Com_Kind (Node, Ocarina.ME_AO4AADL.Com_Kind'Pos (Comm_Kind));
      else
         Node := New_Node (K_Assignment_Action, Start_Loc);
         Set_Reference_Expression (Node, Ident);
         Set_Behavior_Expression (Node, Value_Expr);

      end if;

      if No (Node) then
         DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
         Skip_Declaration (T_Semi_Colon);
         return No_Node;
      else
         return Node;
      end if;

   end P_Assignment_Or_Communication_Action;

   --------------------
   -- P_Timed_Action --
   --------------------

   --  timed_action ::=
   --    computation ( behavior_time [ , behavior_time [ , distribution ] ] )
   --  | delay ( behavior_time [ , behavior_time [ , distribution ] ] )

   --  distribution ::= fixed | normal | poisson | random

   function P_Timed_Action return Node_Id is
   begin
      return No_Node;
   end P_Timed_Action;

   --------------------
   -- P_Proceed_Action --
   --------------------

   --  proceed_action ::=
   --    proceed ( [ Parameter_profile ] )

   function P_Proceed_Action return Node_Id is
      Proceed_Node : Node_Id;
      State : Location;
      --  Loc : Location;
      Parameter : Node_Id;
      Parameter_Prof : Node_Id;
      Parameters : List_Id := No_List;
   begin
      Proceed_Node := New_Node (K_Proceed_Action, Token_Location);
      Scan_Token; -- past "proceed"
      Scan_Token (T_Left_Paren);
      Parameters := New_List (K_List_Id, Token_Location);
      loop
         Save_Lexer (State);
         Parameter_Prof := New_Node
           (K_Parameter_Profile, Token_Location);
         Scan_Token;
         case Token is
            when T_Right_Paren => Parameter := No_Node;
               --  Parameter_Prof := No_Node;
            when others =>
               Restore_Lexer (State);
               Parameter := P_Behavior_Expression;
         end case;
         Set_Behavior_Expression (Parameter_Prof, Parameter);

         if No (Parameter) then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            exit;
         end if;

         Append_Node_To_List (Parameter_Prof, Parameters);
         Save_Lexer (State);
         Scan_Token (T_Comma);
         if Token /= T_Comma then
            Restore_Lexer (State);
            Scan_Token (T_Right_Paren);
            exit;
         end if;
      end loop;
      Set_Parameter_Profile (Proceed_Node, Parameters);
      Scan_Token (T_Semi_Colon);
      if No (Proceed_Node) then
         return No_Node;
      else
         return Proceed_Node;
      end if;

   end P_Proceed_Action;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Ocarina.Parser.Register_Parser (Language, Process'Access);
   end Init;
end Ocarina.FE_AO4AADL.Parser;
