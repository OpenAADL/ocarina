------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . B E _ R E A L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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

with Ocarina.Types;
with Ocarina.Output;
with Ocarina.Namet;
with Ocarina.Backends;
with Ocarina.REAL_Values;
with Ocarina.ME_REAL.Tokens;
with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.REAL_Tree.Nutils;
with Ocarina.ME_REAL.REAL_Tree.Utils;

package body Ocarina.BE_REAL is
   use Ocarina.Types;
   use Ocarina.Output;
   use Ocarina.ME_REAL.Tokens;
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Nutils;
   use Ocarina.ME_REAL.REAL_Tree.Utils;

   procedure Generate_REAL_Spec (Node : Node_Id);

   procedure Print (Node : Node_Id);

   procedure Print_Token (Token : Ocarina.ME_REAL.Tokens.Token_Type);

   procedure Print_Identifier (Node : Node_Id);

   procedure Print_Literal (Node : Node_Id);

   procedure Print_Element (Node : Node_Id);

   procedure Print_Theorem (Node : Node_Id);

   procedure Print_Variable (Node : Node_Id);

   procedure Print_Range_Declaration (Node : Node_Id);

   procedure Print_Variable_Decl (Node : Node_Id);

   procedure Print_Variable_Decl_Expression (Node : Node_Id);

   procedure Print_Variable_Decl_Compute (Node : Node_Id);

   procedure Print_Declaration (Node : Node_Id);

   procedure Print_Set_Declaration (Node : Node_Id);

   procedure Print_Requirement (Node : Node_Id);

   procedure Print_Check_Expression (Node : Node_Id);

   procedure Print_Return_Expression (Node : Node_Id);

   procedure Print_Parametrized_Identifier (Node : Node_Id);

   procedure Print_Expression (Node : Node_Id);

   procedure Print_Check_Subprogram_Call (Node : Node_Id);

   ----------
   -- Init --
   ----------

   procedure Init is
      use Ocarina.Backends;
   begin
      Register_Backend
        ("real_specification",
         Generate_REAL_Spec'Access,
         REAL_PP);
   end Init;

   ------------------------
   -- Generate_REAL_Spec --
   ------------------------

   procedure Generate_REAL_Spec (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Root_Node);

      N : Node_Id;
   begin
      if not Is_Empty (Theorems (Node)) then
         N := First_Node (Theorems (Node));
         Write_Eol;
         Write_Eol;
         while Present (N) loop
            Print_Theorem (N);
            Write_Eol;
            Write_Eol;
            N := Next_Node (N);
         end loop;
      end if;
   end Generate_REAL_Spec;

   -----------
   -- Print --
   -----------

   procedure Print (Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Var_Reference | K_Identifier | K_Set_Reference =>
            Print_Identifier (Node);

         when K_Literal =>
            Print_Literal (Node);

         when K_Check_Subprogram_Call =>
            Print_Check_Subprogram_Call (Node);

         when K_Check_Expression | K_Set_Expression | K_Expression =>
            Print_Expression (Node);

         when others =>
            raise Program_Error;
      end case;
   end Print;

   -----------------------
   -- Print_Declaration --
   -----------------------

   procedure Print_Declaration (Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Set_Declaration =>
            Print_Set_Declaration (Node);

         when K_Variable_Decl_Expression =>
            Print_Variable_Decl_Expression (Node);

         when K_Variable_Decl_Compute =>
            Print_Variable_Decl_Compute (Node);

         when others =>
            raise Program_Error;
      end case;
   end Print_Declaration;

   -------------------
   -- Print_Theorem --
   -------------------

   procedure Print_Theorem (Node : Node_Id) is
      D : Node_Id;
   begin

      --  We add a comment for readability

      Write_Str ("--  ");
      Print_Identifier (Identifier (Node));
      Write_Eol;
      Write_Eol;

      Print_Token (T_Theorem);
      Write_Space;
      Print_Identifier (Identifier (Node));
      Write_Eol;

      --  Range definition

      Print_Range_Declaration (Range_Declaration (Node));
      Write_Eol;
      Increment_Indentation;

      --  Declarations for sets and variables

      Increment_Indentation;
      if not Is_Empty (Declarations (Node)) then
         D := First_Node (Declarations (Node));
         while Present (D) loop
            Print_Declaration (D);
            D := Next_Node (D);
            Write_Eol;
         end loop;
         Write_Eol;
      end if;
      Decrement_Indentation;

      --  Requirements

      if not Is_Empty (Required_Theorems (Node)) then
         Print_Token (T_Requires);
         Write_Space;
         Print_Token (T_Left_Paren);
         D := First_Node (Required_Theorems (Node));
         while Present (D) loop
            Print_Requirement (D);
            D := Next_Node (D);
            if Present (D) then
               Print_Token (T_Comma);
               Write_Space;
            end if;
         end loop;
         Print_Token (T_Right_Paren);
         Print_Token (T_Semi_Colon);
         Write_Eol;
         Write_Eol;
      end if;

      --  Check expression

      if Present (Check_Expression (Node)) then
         Print_Check_Expression (Check_Expression (Node));
      elsif Present (Return_Expression (Node)) then
         Print_Return_Expression (Return_Expression (Node));
      else
         raise Program_Error;
      end if;
      Print_Token (T_Semi_Colon);
      Write_Eol;
      Decrement_Indentation;

      Print_Token (T_End);
      Write_Space;
      Print_Identifier (Identifier (Node));
      Print_Token (T_Semi_Colon);
      Write_Eol;
   end Print_Theorem;

   ----------------------------
   -- Print_Check_Expression --
   ----------------------------

   procedure Print_Check_Expression (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Check_Expression);
   begin
      Print_Token (T_Check);
      Write_Space;
      Print_Token (T_Left_Paren);
      Print (Node);
      Print_Token (T_Right_Paren);
   end Print_Check_Expression;

   -----------------------------
   -- Print_Return_Expression --
   -----------------------------

   procedure Print_Return_Expression (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Return_Expression);
      use Ocarina.REAL_Values;

      procedure Print_Range_Function (V : Value_Id);

      --------------------------
      -- Print_Range_Function --
      --------------------------

      procedure Print_Range_Function (V : Value_Id) is
      begin
         if V /= No_Value then
            case V is
               when FC_MMax =>
                  Write_Str ("mmax");
               when FC_MSum =>
                  Write_Str ("msum");
               when others =>
                  raise Program_Error;
            end case;
         end if;
      end Print_Range_Function;
   begin
      Print_Token (T_Return);
      Write_Space;
      Print_Token (T_Left_Paren);
      if Range_Function (Node) /= No_Value then
         Print_Token (T_Left_Paren);
         Print_Range_Function (Range_Function (Node));
         Write_Space;
         Print (Check_Expression (Node));
         Print_Token (T_Right_Paren);
      else
         Print (Check_Expression (Node));
      end if;
      Print_Token (T_Right_Paren);
   end Print_Return_Expression;

   -----------------------
   -- Print_Requirement --
   -----------------------

   procedure Print_Requirement (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Required_Theorem);
      use Ocarina.Namet;
   begin
      Write_Name (Theorem_Name (Node));
   end Print_Requirement;

   ---------------------------
   -- Print_Set_Declaration --
   ---------------------------

   procedure Print_Set_Declaration (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Set_Declaration);
   begin
      Print_Parametrized_Identifier (Parametrized_Expr (Node));
      Write_Space;
      Print_Token (T_Affect);
      Write_Space;
      Print_Token (T_Left_Brace);
      Write_Space;
      Print_Identifier (Local_Variable (Node));
      Write_Space;
      Print_Token (T_In);
      Write_Space;
      Print (Local_Set_Expression (Node));
      Write_Space;
      Print_Token (T_Sothat);
      Write_Eol;
      Increment_Indentation;
      Print (Selection_Expression (Node));
      Print_Token (T_Right_Brace);
      Print_Token (T_Semi_Colon);
      Decrement_Indentation;
   end Print_Set_Declaration;

   -------------------------
   -- Print_Variable_Decl --
   -------------------------

   procedure Print_Variable_Decl (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Variable_Declaration
         or else Kind (Node) = K_Variable_Decl_Compute
         or else Kind (Node) = K_Variable_Decl_Expression);
   begin
      if Int (Is_Global (Node)) = 0 then
         Print_Token (T_Var);
      else
         Print_Token (T_Global);
      end if;
      Write_Space;
      Print_Variable (Var_Ref (Node));
      Write_Space;
      Print_Token (T_Affect);
      Write_Space;
   end Print_Variable_Decl;

   ---------------------------------
   -- Print_Variable_Decl_Compute --
   ---------------------------------

   procedure Print_Variable_Decl_Compute (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Variable_Decl_Compute);
      use Ocarina.Namet;
   begin
      Print_Variable_Decl (Node);
      Print_Token (T_Compute);
      Write_Space;
      Write_Name (Theorem_Name (Node));

      if not Is_Empty (Parameters (Node)) then
         declare
            D : Node_Id := First_Node (Parameters (Node));
         begin
            Print_Token (T_Left_Paren);
            while Present (D) loop
               Print (D);
               D := Next_Node (D);
               if Present (D) then
                  Print_Token (T_Comma);
                  Write_Space;
               end if;
            end loop;
            Print_Token (T_Right_Paren);
         end;
      end if;
      Print_Token (T_Semi_Colon);
   end Print_Variable_Decl_Compute;

   ------------------------------------
   -- Print_Variable_Decl_Expression --
   ------------------------------------

   procedure Print_Variable_Decl_Expression (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Variable_Decl_Expression);
   begin
      Print_Variable_Decl (Node);
      Write_Eol;
      Increment_Indentation;
      Print_Return_Expression (Return_Expr (Node));
      Print_Token (T_Semi_Colon);
      Decrement_Indentation;
   end Print_Variable_Decl_Expression;

   -----------------------------
   -- Print_Range_Declaration --
   -----------------------------

   procedure Print_Range_Declaration (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Range_Declaration);
   begin
      Print_Token (T_Foreach);
      Write_Space;
      Print_Element (Range_Variable (Node));
      Write_Space;
      Print_Token (T_In);
      Write_Space;
      Print (Range_Set (Node));
      Write_Space;
      Print_Token (T_Do);
      Write_Eol;
   end Print_Range_Declaration;

   ----------------------
   -- Print_Expression --
   ----------------------

   procedure Print_Expression (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Expression
         or else Kind (Node) = K_Set_Expression
         or else Kind (Node) = K_Check_Expression);

      procedure Print_Operator (Op : Operator_Id);

      --------------------
      -- Print_Operator --
      --------------------

      procedure Print_Operator (Op : Operator_Id) is
      begin
         case Op is
            when OV_Not =>
               Write_Str ("not");
            when OV_And =>
               Write_Str ("and");
            when OV_Or =>
               Write_Str ("or");
            when OV_Minus =>
               Write_Str ("-");
            when OV_Star =>
               Write_Str ("*");
            when OV_Plus =>
               Write_Str ("+");
            when OV_Equal =>
               Write_Str ("=");
            when OV_Different =>
               Write_Str ("<>");
            when OV_Greater =>
               Write_Str (">");
            when OV_Greater_Equal =>
               Write_Str (">=");
            when OV_Less =>
               Write_Str ("<");
            when OV_Less_Equal =>
               Write_Str ("<=");
            when OV_Slash =>
               Write_Str ("/");
            when OV_Power =>
               Write_Str ("**");
            when others =>
               raise Program_Error;
         end case;
      end Print_Operator;
   begin
      if No (Right_Expr (Node)) then
         Print_Operator (Operator (Node));
         Write_Space;
         Print (Right_Expr (Node));
      else
         if Present (Left_Expr (Node)) then
            Print_Token (T_Left_Paren);
            Print (Left_Expr (Node));
            Write_Space;
         end if;
         Print_Operator (Operator (Node));

         --  In case of boolean binary operator, we break the line

         if Operator (Node) = OV_And or else Operator (Node) = OV_Or then
            Write_Eol;
            Increment_Indentation;
            Print (Right_Expr (Node));
            Decrement_Indentation;
         else
            Write_Space;
            Print (Right_Expr (Node));
         end if;

         if Present (Left_Expr (Node)) then
            Print_Token (T_Right_Paren);
         end if;
      end if;
   end Print_Expression;

   ---------------------------------
   -- Print_Check_Subprogram_Call --
   ---------------------------------

   procedure Print_Check_Subprogram_Call (Node : Node_Id) is
   begin
      Print_Parametrized_Identifier (Node);
   end Print_Check_Subprogram_Call;

   -----------------------------------
   -- Print_Parametrized_Identifier --
   -----------------------------------

   procedure Print_Parametrized_Identifier (Node : Node_Id) is
   begin
      Print_Identifier (Identifier (Node));

      if not (Is_Empty (Parameters (Node))) then
         Print_Token (T_Left_Paren);
         declare
            N : Node_Id := First_Node (Parameters (Node));
         begin
            while Present (N) loop
               Print (N);
               N := Next_Node (N);
               if Present (N) then
                  Print_Token (T_Comma);
                  Write_Space;
               end if;
            end loop;
         end;
         Print_Token (T_Right_Paren);
      end if;
   end Print_Parametrized_Identifier;

   -------------------
   -- Print_Literal --
   -------------------

   procedure Print_Literal (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Literal);
      use Ocarina.REAL_Values;
   begin
      Write_Str (Image (Value (Node)));
   end Print_Literal;

   -------------------
   -- Print_Element --
   -------------------

   procedure Print_Element (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Element);
      use Ocarina.Namet;
   begin
      Write_Name (Name (Identifier (Node)));
   end Print_Element;

   --------------------
   -- Print_Variable --
   --------------------

   procedure Print_Variable (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Var_Reference);
      use Ocarina.Namet;
   begin
      Write_Name (Name (Node));
   end Print_Variable;

   ----------------------
   -- Print_Identifier --
   ----------------------

   procedure Print_Identifier (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Identifier
         or else Kind (Node) = K_Var_Reference
         or else Kind (Node) = K_Set_Reference);
      use Ocarina.Namet;
   begin
      Write_Name (Name (Node));
   end Print_Identifier;

   -----------------
   -- Print_Token --
   -----------------

   procedure Print_Token (Token : Token_Type) is
   begin
      Write_Str (Image (Token));
   end Print_Token;

end Ocarina.BE_REAL;
