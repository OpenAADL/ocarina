------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B E _ A A D L _ B A . E X P R E S S I O N S        --
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

with Ocarina.Output;

with Ocarina.AADL_Values;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.BE_AADL_BA.Identifiers;
with Ocarina.BE_AADL_BA.Actions;

package body Ocarina.BE_AADL_BA.Expressions is

   use Ocarina.Output;

   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.BE_AADL_BA.Identifiers;
   use Ocarina.BE_AADL_BA.Actions;

   package BAN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   procedure Print_Relation          (Node : Node_Id);
   procedure Print_Simple_Expression (Node : Node_Id);
   procedure Print_Term              (Node : Node_Id);
   procedure Print_Factor            (Node : Node_Id);
   procedure Print_Value             (Node : Node_Id);

   --------------------------
   -- Print_Value_Variable --
   --------------------------

   procedure Print_Value_Variable (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Value_Variable);

      Ident : constant Node_Id := BAN.Identifier (Node);
   begin
      case Kind (Ident) is
         when K_Name   => Print_Name (Ident);

         when K_Data_Component_Reference =>
            Print_Data_Component_Reference (Ident);

         when others => Write_Line (Bug_Str);
      end case;

      if Is_Interrogative (Node) then
         Print_Token (T_Interrogative);
      end if;

      if Is_Count (Node) then
         Print_Token (T_Tick);
         Print_Token (T_Count);
      elsif Is_Fresh (Node) then
         Print_Token (T_Tick);
         Print_Token (T_Fresh);
      elsif Is_Updated (Node) then
         Print_Token (T_Tick);
         Print_Token (T_Updated);
      end if;
   end Print_Value_Variable;

   ----------------------------
   -- Print_Value_Expression --
   ----------------------------

   procedure Print_Value_Expression (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Value_Expression);
      pragma Assert (not Is_Empty (Relations (Node)));

      List_Node : Node_Id;
   begin
      List_Node := First_Node (Relations (Node));
      Print_Relation (List_Node);

      List_Node := Next_Node (List_Node);
      while Present (List_Node) loop
         Write_Space;

         case Kind (List_Node) is
            when K_Relation => Print_Relation (List_Node);
            when K_Operator => Print_Operator (List_Node);
            when others     => Write_Line     (Bug_Str);
         end case;

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Value_Expression;

   --------------------
   -- Print_Relation --
   --------------------

   procedure Print_Relation (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Relation);

      List_Node : Node_Id;
   begin
      if not Is_Empty (Simple_Exprs (Node)) then
         --  if Boolean_Value (Node) then
         --   Print_Token (T_True);
         --  else
         --   Print_Token (T_False);
         --  end if;
      --  else
         List_Node := First_Node (Simple_Exprs (Node));
         Print_Simple_Expression (List_Node);

         List_Node := Next_Node (List_Node);
         while Present (List_Node) loop
            Write_Space;

            case Kind (List_Node) is
               when K_Simple_Expression => Print_Simple_Expression (List_Node);
               when K_Operator          => Print_Operator          (List_Node);
               when others              => Write_Line              (Bug_Str);
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;
   end Print_Relation;

   -----------------------------
   -- Print_Simple_Expression --
   -----------------------------

   procedure Print_Simple_Expression (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Simple_Expression);
      pragma Assert (not Is_Empty (Term_And_Operator (Node)));

      No_First  : Boolean  := False;
      List_Node : Node_Id;
   begin
      List_Node := First_Node (Term_And_Operator (Node));

      while Present (List_Node) loop
         if No_First then
            Write_Space;
         end if;
         No_First := True;

         case Kind (List_Node) is
            when K_Operator => Print_Operator (List_Node);
            when K_Term     => Print_Term     (List_Node);
            when others     => Write_Line     (Bug_Str);
         end case;

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Simple_Expression;

   ----------------
   -- Print_Term --
   ----------------

   procedure Print_Term (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Term);
      pragma Assert (not Is_Empty (Factors (Node)));

      No_First  : Boolean := False;
      List_Node : Node_Id;
   begin
      List_Node := First_Node (Factors (Node));

      while Present (List_Node) loop
         if No_First then
            Write_Space;
         end if;
         No_First := True;

         case Kind (List_Node) is
            when K_Operator => Print_Operator (List_Node);
            when K_Factor   => Print_Factor   (List_Node);
            when others     => Write_Line     (Bug_Str);
         end case;

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Term;

   ------------------
   -- Print_Factor --
   ------------------

   procedure Print_Factor (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Factor);

   begin
      if Is_Abs (Node) then
         Print_Token (T_Abs);
         Write_Space;
      elsif Is_Not (Node) then
         Print_Token (T_Not);
         Write_Space;
      end if;

      Print_Value (Lower_Value (Node));

      if Present (Upper_Value (Node)) then
         Write_Space;
         Print_Token (T_Exponent);
         Write_Space;
         Print_Value (Upper_Value (Node));
      end if;
   end Print_Factor;

   -----------------
   -- Print_Value --
   -----------------

   procedure Print_Value (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Value_Variable
                       or else Kind (Node) = K_Value_Expression
                       or else Kind (Node) = K_Literal
                       or else Kind (Node) = K_Boolean_Literal
                       or else Kind (Node) = K_Property_Constant
                       or else Kind (Node) = K_Property_Reference
                       or else Kind (Node) = K_Identifier);
   begin
      case Kind (Node) is
         when K_Value_Variable    => Print_Value_Variable    (Node);
         when K_Literal           => Print_Literal           (Node);
         when K_Boolean_Literal   => Print_Boolean_Literal   (Node);
         when K_Property_Constant => Print_Property_Constant (Node);
         when K_Property_Reference => Print_Property_Reference (Node);
         when K_Value_Expression  =>
            Print_Token (T_Left_Parenthesis);
            Print_Value_Expression (Node);
            Print_Token (T_Right_Parenthesis);
         when K_Identifier        => Print_Identifier        (Node);
         when others              => Write_Line              (Bug_Str);
      end case;
   end Print_Value;

   -----------------------------
   -- Print_Property_Constant --
   -----------------------------

   procedure Print_Property_Constant (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Property_Constant);

   begin
      if Present (Property_Set (Node)) then
         Print_Identifier (Property_Set (Node));
         Print_Token (T_Colon_Colon);
      end if;

      Print_Identifier (BAN.Identifier (Node));
   end Print_Property_Constant;

   -----------------------------
   -- Print_Property_Reference --
   -----------------------------

   procedure Print_Property_Reference (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Property_Reference);

   begin
      if Present (Property_Set_Idt (Node)) then
         Print_Token (T_Number_Sign);
         Print_Identifier (Property_Set_Idt (Node));
         Print_Token (T_Colon_Colon);
      end if;
      if Present (Entity (Node)) then
         Print_Component_Element_Ref (Entity (Node));
         Print_Token (T_Number_Sign);
      end if;
      Print_Property_Name (Property_Name (Node));
   end Print_Property_Reference;

   -------------------------
   -- Print_Property_Name --
   -------------------------

   procedure Print_Property_Name (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Property_Name);

      List_Node : Node_Id;
   begin
      Print_Identifier (Property_Idt (Node));
      if not Is_Empty (Property_Field (Node)) then
         List_Node := First_Node (Property_Field (Node));
         Print_Property_Field (List_Node);

         List_Node := Next_Node (List_Node);
         while Present (List_Node) loop
            Write_Space;
            Print_Property_Field (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;
   end Print_Property_Name;

   --------------------------
   -- Print_Property_Field --
   --------------------------

   procedure Print_Property_Field (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Property_Field);

   begin
      if Is_Upper_Bound (Node) then
         Print_Token (T_Dot);
         Print_Token (T_Upper_Bound);
         Write_Space;
      elsif Is_Lower_Bound (Node) then
         Print_Token (T_Dot);
         Print_Token (T_Lower_Bound);
         Write_Space;
      end if;

      if Present (Entity (Node)) then
         case Kind (Entity (Node)) is
            when K_Identifier =>
               Print_Token (T_Dot);
               Print_Identifier (Entity (Node));
            when K_Integer_Value  =>
               Print_Token (T_Left_Square_Bracket);
               Print_Integer_Value (Entity (Node));
               Print_Token (T_Right_Square_Bracket);
            when others           => Write_Line (Bug_Str);
         end case;
      end if;
   end Print_Property_Field;

   ---------------------------------
   -- Print_Component_Element_Ref --
   ---------------------------------

   procedure Print_Component_Element_Ref (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Component_Element_Reference);

   begin
      if Is_Self (Node) then
         Print_Token (T_Self);
         Write_Space;
      end if;

      if Present (Ident (Node)) then
         Write_Space;
         Print_Identifier (Ident (Node));
      end if;
   end Print_Component_Element_Ref;

   --------------------
   -- Print_Operator --
   --------------------

   procedure Print_Operator (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Operator);

   begin
      case Operator_Kind'Val (Operator_Category (Node)) is

         --  logical operator
         when OK_And              => Print_Token (T_And);
         when OK_Or               => Print_Token (T_Or);
         when OK_Xor              => Print_Token (T_Xor);
         when OK_Or_Else          => Print_Tokens ((T_Or, T_Else));
         when OK_And_Then         => Print_Tokens ((T_And, T_Then));

         --  relational_operator
         when OK_Equal            => Print_Token (T_Equals_Sign);
         when OK_Non_Equal        => Print_Token (T_Non_Equal);
         when OK_Less_Than        => Print_Token (T_Less_Than_Sign);
         when OK_Less_Or_Equal    => Print_Token (T_Less_Or_Equal);
         when OK_Greater_Than     => Print_Token (T_Greater_Than_Sign);
         when OK_Greater_Or_Equal => Print_Token (T_Greater_Or_Equal);

         --  unary_adding_opetor
         --  binary_adding_operator
         when OK_Plus             => Print_Token (T_Plus);
         when OK_Minus            => Print_Token (T_Minus);

         --  multiplying operator
         when OK_Multiply         => Print_Token (T_Multiply);
         when OK_Divide           => Print_Token (T_Divide);
         when OK_Mod              => Print_Token (T_Mod);
         when OK_Rem              => Print_Token (T_Rem);

         --  highest precedence operator
         when OK_Exponent         => Print_Token (T_Exponent);
         when OK_Abs              => Print_Token (T_Abs);
         when OK_Not              => Print_Token (T_Not);

         when others              => Write_Line  (Bug_Str);
      end case;
   end Print_Operator;

   -------------------------
   -- Print_Integer_Range --
   -------------------------

   procedure Print_Integer_Range (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Integer_Range);
      pragma Assert (Kind (Lower_Int_Val (Node)) = K_Integer_Value);
      pragma Assert (Kind (Upper_Int_Val (Node)) = K_Integer_Value);

   begin
      Write_Space;
      Print_Integer_Value (Lower_Int_Val (Node));
      Write_Space;
      Print_Token (T_Interval);
      Write_Space;
      Print_Integer_Value (Upper_Int_Val (Node));
   end Print_Integer_Range;

   -------------------------
   -- Print_Integer_Value --
   -------------------------

   procedure Print_Integer_Value (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Integer_Value);
      pragma Assert (Kind (Entity (Node)) = K_Value_Variable
                       or else Kind (Entity (Node)) = K_Literal
                       or else Kind (Entity (Node)) = K_Property_Constant);

      Entity_Node : constant Node_Id := Entity (Node);
   begin
      case Kind (Entity_Node) is
         when K_Value_Variable    => Print_Value_Variable    (Entity_Node);
         when K_Literal           => Print_Literal           (Entity_Node);
         when K_Property_Constant => Print_Property_Constant (Entity_Node);
         when others              => Write_Line              (Bug_Str);
      end case;
   end Print_Integer_Value;

   -------------------------
   -- Print_Behavior_Time --
   -------------------------

   procedure Print_Behavior_Time (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_Time);

   begin
      Write_Space;
      Print_Integer_Value (Integer_Value (Node));
      Write_Space;
      Print_Identifier (Unit_Identifier (Node));
   end Print_Behavior_Time;

   -------------------
   -- Print_Literal --
   -------------------

   procedure Print_Literal (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Literal);

   begin
      Write_Str (Ocarina.AADL_Values.Image (Value (Node)));
   end Print_Literal;

   ---------------------------
   -- Print_Boolean_Literal --
   ---------------------------

   procedure Print_Boolean_Literal (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Boolean_Literal);

   begin
      if Is_True (Node) then
         Print_Token (T_True);
         Write_Space;
      else
         Print_Token (T_False);
         Write_Space;
      end if;

   end Print_Boolean_Literal;

end Ocarina.BE_AADL_BA.Expressions;
