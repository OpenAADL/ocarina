------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B U I L D E R . A A D L _ B A . E X P R E S S I O N S   --
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

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

package body Ocarina.Builder.AADL_BA.Expressions is

   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   ----------------------------
   -- Add_New_Value_Variable --
   ----------------------------

   function Add_New_Value_Variable
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Is_A_Count   : Boolean  := False;
      Is_A_Fresh   : Boolean  := False;
      Is_A_Updated : Boolean  := False;
      Is_A_Interro : Boolean  := False)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Factor
                       or else Kind (Container) = K_Integer_Value);
      pragma Assert (Kind (Ident) = K_Name
                       or else Kind (Ident) = K_Data_Component_Reference);

      Value_Variable : constant Node_Id := New_Node (K_Value_Variable, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Value_Variable, Container);
      end if;

      Set_Identifier (Value_Variable, Ident);
      Set_BE_Container (Ident, Value_Variable);

      Set_Is_Count (Value_Variable, Is_A_Count);
      Set_Is_Fresh (Value_Variable, Is_A_Fresh);
      Set_Is_Updated (Value_Variable, Is_A_Updated);
      Set_Is_Interrogative (Value_Variable, Is_A_Interro);

      return Value_Variable;
   end Add_New_Value_Variable;

   ------------------------------
   -- Add_New_Value_Expression --
   ------------------------------

   function Add_New_Value_Expression
     (Loc           : Location;
      Container     : Node_Id;
      Relation_List : List_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Factor
                       or else Kind (Container) = K_Assignment_Action);
      pragma Assert (not Is_Empty (Relation_List));

      Value_Expr : constant Node_Id := New_Node (K_Value_Expression, Loc);
      List_Node  : Node_Id;
   begin
      if Present (Container) then
         Set_BE_Container (Value_Expr, Container);
      end if;

      Set_Relations (Value_Expr, Relation_List);

      List_Node := First_Node (Relations (Value_Expr));
      while Present (List_Node) loop
         Set_BE_Container (List_Node, Value_Expr);

         List_Node := Next_Node (List_Node);
      end loop;

      return Value_Expr;

   end Add_New_Value_Expression;

   ----------------------
   -- Add_New_Relation --
   ----------------------

   function Add_New_Relation
     (Loc           : Location;
      Container     : Node_Id;
      Spl_Expr_List : List_Id   := No_List)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Value_Expression);

      Relation_Node : constant Node_Id := New_Node (K_Relation, Loc);
      List_Node     : Node_Id;
   begin
      if Present (Container) then
         Set_BE_Container (Relation_Node, Container);
      end if;

      --  Set_Boolean_Value (Relation_Node, Bool_Value);

      Set_Simple_Exprs (Relation_Node, Spl_Expr_List);
      if not Is_Empty (Simple_Exprs (Relation_Node))
        and then Present (Container)
      --  fixme : todo check when container is No_Node
      then
         List_Node := First_Node (Simple_Exprs (Relation_Node));
         while Present (List_Node) loop
            Set_BE_Container (List_Node, Relation_Node);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Relation_Node;

   end Add_New_Relation;

   -------------------------------
   -- Add_New_Simple_Expression --
   -------------------------------

   function Add_New_Simple_Expression
     (Loc              : Location;
      Container        : Node_Id;
      Simple_Expr_List : List_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                     or else Kind (Container) = K_Relation);
      pragma Assert (not Is_Empty (Simple_Expr_List));

      Simple_Expr : constant Node_Id := New_Node (K_Simple_Expression, Loc);
      List_Node   : Node_Id;
   begin
      if Present (Container) then
         Set_BE_Container (Simple_Expr, Container);
      end if;

      Set_Term_And_Operator (Simple_Expr, Simple_Expr_List);
      if not Is_Empty (Term_And_Operator (Simple_Expr)) then
         List_Node := First_Node (Term_And_Operator (Simple_Expr));
         while Present (List_Node) loop
            Set_BE_Container (List_Node, Simple_Expr);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Simple_Expr;

   end Add_New_Simple_Expression;

   ------------------
   -- Add_New_Term --
   ------------------

   function Add_New_Term
     (Loc         : Location;
      Container   : Node_Id;
      Factor_List : List_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                     or else Kind (Container) = K_Simple_Expression);
      pragma Assert (not Is_Empty (Factor_List));

      Term_Node : constant Node_Id := New_Node (K_Term, Loc);
      List_Node   : Node_Id;
   begin
      if Present (Container) then
         Set_BE_Container (Term_Node, Container);
      end if;

      Set_Factors (Term_Node, Factor_List);
      if not Is_Empty (Factors (Term_Node)) then
         List_Node := First_Node (Factors (Term_Node));
         while Present (List_Node) loop
            Set_BE_Container (List_Node, Term_Node);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Term_Node;

   end Add_New_Term;

   --------------------
   -- Add_New_Factor --
   --------------------

   function Add_New_Factor
     (Loc         : Location;
      Container   : Node_Id;
      Is_Abs_Bool : Boolean;
      Is_Not_Bool : Boolean;
      Low_Value   : Node_Id;
      Upp_Value   : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                     or else Kind (Container) = K_Term);

      pragma Assert (Kind (Low_Value) = K_Identifier
                     or else Kind (Low_Value) = K_Value_Variable
                     or else Kind (Low_Value) = K_Literal
                     or else Kind (Low_Value) = K_Boolean_Literal
                     or else Kind (Low_Value) = K_Property_Constant
                     or else Kind (Low_Value) = K_Property_Reference
                     or else Kind (Low_Value) = K_Value_Expression);

      pragma Assert (No (Upp_Value)
                     or else Kind (Upp_Value) = K_Identifier
                     or else Kind (Upp_Value) = K_Literal
                     or else Kind (Upp_Value) = K_Boolean_Literal
                     or else Kind (Upp_Value) = K_Property_Constant
                     or else Kind (Upp_Value) = K_Property_Reference
                     or else Kind (Upp_Value) = K_Value_Expression);

      Factor_Node : constant Node_Id := New_Node (K_Factor, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Factor_Node, Container);
      end if;

      Set_Is_Abs (Factor_Node, Is_Abs_Bool);
      Set_Is_Not (Factor_Node, Is_Not_Bool);

      Set_Lower_Value (Factor_Node, Low_Value);
      Set_BE_Container (Low_Value, Factor_Node);

      if Present (Upp_Value) then
         Set_Upper_Value (Factor_Node, Upp_Value);
         Set_BE_Container (Upp_Value, Factor_Node);
      end if;

      return Factor_Node;

   end Add_New_Factor;

   -------------------------------
   -- Add_New_Property_Constant --
   -------------------------------

   function Add_New_Property_Constant
     (Loc             : Location;
      Container       : Node_Id;
      Property_Set_Id : Node_Id;
      Property_Cst_Id : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Factor
                       or else Kind (Container) = K_Integer_Value);
      pragma Assert (Kind (Property_Cst_Id) = K_Identifier);

      Property_Cst : constant Node_Id := New_Node (K_Property_Constant,
                                                   Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Property_Cst, Container);
      end if;

      Set_Identifier (Property_Cst, Property_Cst_Id);
      Set_BE_Container (Property_Cst_Id, Property_Cst);

      if Present (Property_Set_Id) then
         Set_Property_Set (Property_Cst, Property_Set_Id);
         Set_BE_Container (Property_Set_Id, Property_Cst);
      end if;

      return Property_Cst;

   end Add_New_Property_Constant;

   ---------------------------
   -- Add_New_Integer_Range --
   ---------------------------

   function Add_New_Integer_Range
     (Loc         : Location;
      Container   : Node_Id;
      Lower_Bound : Node_Id;
      Upper_Bound : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Element_Values);
      Integer_Range : constant Node_Id := New_Node (K_Integer_Range, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Integer_Range, Container);
      end if;

      Set_Lower_Int_Val (Integer_Range, Lower_Bound);
      Set_Upper_Int_Val (Integer_Range, Upper_Bound);

      return Integer_Range;

   end Add_New_Integer_Range;

   ---------------------------
   -- Add_New_Integer_Value --
   ---------------------------

   function Add_New_Integer_Value
     (Loc         : Location;
      Container   : Node_Id;
      Entity_Node : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Integer_Range);
      pragma Assert (Kind (Entity_Node) = K_Property_Constant
                       or else Kind (Entity_Node) = K_Literal
                       or else Kind (Entity_Node) = K_Value_Variable);
      --  fixme : todo add other entities

      Integer_Val : constant Node_Id := New_Node (K_Integer_Value, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Integer_Val, Container);
      end if;

      Set_Entity (Integer_Val, Entity_Node);
      Set_BE_Container (Entity_Node, Integer_Val);

      return Integer_Val;

   end Add_New_Integer_Value;

   ---------------------------
   -- Add_New_Behavior_Time --
   ---------------------------

   function Add_New_Behavior_Time
     (Loc           : Location;
      Container     : Node_Id;
      Integer_Val   : Node_Id;
      Unit_Ident    : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Timed_Act);
      pragma Assert (Kind (Integer_Val) = K_Integer_Value);
      pragma Assert (Kind (Unit_Ident) = K_Identifier);

      Behavior_Time : constant Node_Id := New_Node (K_Behavior_Time,
                                                    Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Behavior_Time, Container);
      end if;

      Set_Integer_Value (Behavior_Time, Integer_Val);
      Set_BE_Container (Integer_Val, Behavior_Time);
      Set_Unit_Identifier (Behavior_Time, Unit_Ident);
      Set_BE_Container (Integer_Val, Behavior_Time);

      return Behavior_Time;

   end Add_New_Behavior_Time;

   ----------------------
   -- Add_New_Operator --
   ----------------------

   function Add_New_Operator
     (Loc         : Location;
      Container   : Node_Id;
      Operat_Kind : Operator_Kind)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Term
                       or else Kind (Container) = K_Relation
                       or else Kind (Container) = K_Value_Expression
                       or else Kind (Container) = K_Simple_Expression);

      Operator_Node : constant Node_Id := New_Node (K_Operator, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Operator_Node, Container);
      end if;

      Set_Operator_Category (Operator_Node,
                             Operator_Kind'Pos (Operat_Kind));

      return Operator_Node;

   end Add_New_Operator;

   --------------------------------
   -- Add_New_Property_Reference --
   --------------------------------

   function Add_New_Property_Reference
     (Loc             : Location;
      Container       : Node_Id;
      Property_Set_Id : Node_Id;
      Entity          : Node_Id;
      Property_Name   : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Factor);
      --  pragma Assert (Kind (Entity) = K_Component_Element_Reference
      --                 or else Kind (Entity) = K_Identifier);
      --  pragma Assert (Kind (Property_Name) = K_Property_Name);

      Property_Reference : constant Node_Id := New_Node (K_Property_Reference,
                                                         Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Property_Reference, Container);
      end if;

      if Present (Property_Set_Id) then
         Set_Property_Set_Idt (Property_Reference, Property_Set_Id);
         --  Set_BE_Container (Property_Set_Id, Property_Reference);
      end if;
      if Present (Entity) then
         Set_Entity (Property_Reference, Entity);
      end if;
      --  Set_BE_Container (Entity, Property_Reference);
      Set_Property_Name (Property_Reference, Property_Name);
      --  Set_BE_Container (Property_Name, Property_Reference);

      return Property_Reference;

   end Add_New_Property_Reference;

   ---------------------------
   -- Add_New_Property_Name --
   ---------------------------

   function Add_New_Property_Name
     (Loc            : Location;
      Container      : Node_Id;
      Property_Idt   : Node_Id;
      Property_Fields : List_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Property_Reference);

      Property_Name : constant Node_Id := New_Node (K_Property_Name, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Property_Name, Container);
      end if;

      Set_Property_Idt (Property_Name, Property_Idt);

      if not Is_Empty (Property_Fields) then
         Set_Property_Field (Property_Name, Property_Fields);
      end if;

      return Property_Name;
   end Add_New_Property_Name;

   -----------------------------------
   -- Add_New_Component_Element_Ref --
   -----------------------------------

   function Add_New_Component_Element_Ref
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Is_Self_Bool : Boolean := False)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Property_Reference);

      Component_Element_Ref : constant Node_Id := New_Node
                                   (K_Component_Element_Reference, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Component_Element_Ref, Container);
      end if;

      Set_Ident (Component_Element_Ref, Ident);

      Set_Is_Self (Component_Element_Ref, Is_Self_Bool);

      return Component_Element_Ref;
   end Add_New_Component_Element_Ref;

   ----------------------------
   -- Add_New_Property_Field --
   ----------------------------

   function Add_New_Property_Field
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Is_Upper_Bound : Boolean := False;
      Is_Lower_Bound : Boolean := False)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Property_Name);

      Property_Field : constant Node_Id := New_Node
                                   (K_Property_Field, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Property_Field, Container);
      end if;

      Set_Entity (Property_Field, Ident);

      Set_Is_Upper_Bound (Property_Field, Is_Upper_Bound);
      Set_Is_Lower_Bound (Property_Field, Is_Lower_Bound);

      return Property_Field;
   end Add_New_Property_Field;

end Ocarina.Builder.AADL_Ba.Expressions;
