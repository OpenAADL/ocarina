------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B E _ A A D L _ B A . E X P R E S S I O N S        --
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

with Ocarina.Output;

with Ocarina.AADL_Values;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.BE_AADL_BA.Identifiers;
with Ocarina.BE_AADL_BA.Actions;

package body Ocarina.BE_AADL_BA.Expressions is

   use Ocarina.Output;
   use Ocarina.AADL_Values;
   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.BE_AADL_BA.Identifiers;
   use Ocarina.BE_AADL_BA.Actions;

   package BAN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   procedure Print_Relation (Node : Node_Id);
   procedure Print_Simple_Expression (Node : Node_Id);
   procedure Print_Term (Node : Node_Id);
   procedure Print_Factor (Node : Node_Id);
   procedure Print_Primary (Node : Node_Id);
   procedure Print_Operator (Node : Node_Id);

   ------------------------
   -- Print_Value_Holder --
   ------------------------

   procedure Print_Value_Holder (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Value_Holder);

      Ident : constant Node_Id := BAN.Identifier (Node);
   begin
      case Kind (Ident) is
         when K_Id =>
            Print_Id (Ident);

         when K_Data_Component_Reference =>
            Print_Data_Component_Reference (Ident);

         when others =>
            Write_Line (Bug_Str);
      end case;

      if Is_Interrogative (Node) then
         Write_Space;
         Print_Token (T_Interrogative);
      end if;

      if Is_Count (Node) then
         Print_Token (T_Tick);
         Print_Token (T_Count);
      elsif Is_Fresh (Node) then
         Print_Token (T_Tick);
         Print_Token (T_Fresh);
      end if;
   end Print_Value_Holder;

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
            when K_Relation =>
               Print_Relation (List_Node);
            when K_Operator =>
               Print_Operator (List_Node);
            when others =>
               Write_Line (Bug_Str);
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
      if Is_Empty (Simple_Exprs (Node)) then
         if Boolean_Value (Node) then
            Print_Token (T_True);
         else
            Print_Token (T_False);
         end if;
      else
         List_Node := First_Node (Simple_Exprs (Node));
         Print_Simple_Expression (List_Node);

         List_Node := Next_Node (List_Node);
         while Present (List_Node) loop
            Write_Space;

            case Kind (List_Node) is
               when K_Simple_Expression =>
                  Print_Simple_Expression (List_Node);
               when K_Operator =>
                  Print_Operator (List_Node);
               when others =>
                  Write_Line (Bug_Str);
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

      No_First  : Boolean := False;
      List_Node : Node_Id;
   begin
      List_Node := First_Node (Term_And_Operator (Node));

      while Present (List_Node) loop
         if No_First then
            Write_Space;
         end if;
         No_First := True;

         case Kind (List_Node) is
            when K_Operator =>
               Print_Operator (List_Node);
            when K_Term =>
               Print_Term (List_Node);
            when others =>
               Write_Line (Bug_Str);
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
            when K_Operator =>
               Print_Operator (List_Node);
            when K_Factor =>
               Print_Factor (List_Node);
            when others =>
               Write_Line (Bug_Str);
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

      Print_Primary (Lower_Primary (Node));

      if Present (Upper_Primary (Node)) then
         Write_Space;
         Print_Token (T_Exponent);
         Write_Space;
         Print_Primary (Upper_Primary (Node));
      end if;
   end Print_Factor;

   -------------------
   -- Print_Primary --
   -------------------

   procedure Print_Primary (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Value_Holder
         or else Kind (Node) = K_Value_Expression
         or else Kind (Node) = K_Literal
         or else Kind (Node) = K_Property_Constant
         or else Kind (Node) = K_Identifier);
   begin
      case Kind (Node) is
         when K_Value_Holder =>
            Print_Value_Holder (Node);
         when K_Literal =>
            Print_Literal (Node);
         when K_Property_Constant =>
            Print_Property_Constant (Node);
         when K_Value_Expression =>
            Print_Token (T_Left_Parenthesis);
            Print_Value_Expression (Node);
            Print_Token (T_Right_Parenthesis);
         when K_Identifier =>
            Print_Identifier (Node);
         when others =>
            Write_Line (Bug_Str);
      end case;
   end Print_Primary;

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

   --------------------
   -- Print_Operator --
   --------------------

   procedure Print_Operator (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Operator);

   begin
      case Operator_Kind'Val (Operator_Category (Node)) is

         --  logical operator
         when OK_And =>
            Print_Token (T_And);
         when OK_Or =>
            Print_Token (T_Or);
         when OK_Xor =>
            Print_Token (T_Xor);
         when OK_Cand =>
            Print_Token (T_Cand);
         when OK_Cor =>
            Print_Token (T_Cor);

         --  relational_operator
         when OK_Equal =>
            Print_Token (T_Equals_Sign);
         when OK_Non_Equal =>
            Print_Token (T_Non_Equal);
         when OK_Less_Than =>
            Print_Token (T_Less_Than_Sign);
         when OK_Less_Or_Equal =>
            Print_Token (T_Less_Or_Equal);
         when OK_Greater_Than =>
            Print_Token (T_Greater_Than_Sign);
         when OK_Greater_Or_Equal =>
            Print_Token (T_Greater_Or_Equal);

         --  unary_adding_opetor
         --  binary_adding_operator
         when OK_Plus =>
            Print_Token (T_Plus);
         when OK_Minus =>
            Print_Token (T_Minus);
         when OK_Concat =>
            Print_Token (T_Concat);

         --  multiplying operator
         when OK_Multiply =>
            Print_Token (T_Multiply);
         when OK_Divide =>
            Print_Token (T_Divide);
         when OK_Mod =>
            Print_Token (T_Mod);
         when OK_Rem =>
            Print_Token (T_Rem);

         --  highest precedence operator
         when OK_Exponent =>
            Print_Token (T_Exponent);
         when OK_Abs =>
            Print_Token (T_Abs);
         when OK_Not =>
            Print_Token (T_Not);

         when others =>
            Write_Line (Bug_Str);
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
      pragma Assert
        (Kind (Entity (Node)) = K_Value_Holder
         or else Kind (Entity (Node)) = K_Literal
         or else Kind (Entity (Node)) = K_Property_Constant);

      Entity_Node : constant Node_Id := Entity (Node);
   begin
      case Kind (Entity_Node) is
         when K_Value_Holder =>
            Print_Value_Holder (Entity_Node);
         when K_Literal =>
            Print_Literal (Entity_Node);
         when K_Property_Constant =>
            Print_Property_Constant (Entity_Node);
         when others =>
            Write_Line (Bug_Str);
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

end Ocarina.BE_AADL_BA.Expressions;
