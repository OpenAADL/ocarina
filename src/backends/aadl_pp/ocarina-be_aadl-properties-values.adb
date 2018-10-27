------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B E _ A A D L . P R O P E R T I E S . V A L U E S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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
with Charset;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.AADL_Values;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Components;
with Ocarina.BE_AADL.Annexes;
with Ocarina.BE_AADL.Components.Arrays;

package body Ocarina.BE_AADL.Properties.Values is

   use Ocarina.Output;
   use Charset;

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.BE_AADL.Components;
   use Ocarina.BE_AADL.Components.Arrays;
   use Ocarina.BE_AADL.Annexes;

   procedure Print_And_Boolean_Term (Node : Node_Id);
   procedure Print_Not_Boolean_Term (Node : Node_Id);
   procedure Print_Parenthesis_Boolean_Term (Node : Node_Id);
   procedure Print_Boolean_Term (Node : Node_Id);
   procedure Print_Number_Range_Term (Node : Node_Id);
   procedure Print_Referable_Element_Category (Node : Node_Id);
   procedure Print_Record_Term_Element (Node : Node_Id);
   procedure Print_Record_Type_Element (Node : Node_Id);
   procedure Print_Reference_Term (Node : Node_Id);
   procedure Print_Minus_Numeric_Term (Node : Node_Id);
   procedure Print_Unit_Definition (Node : Node_Id);
   procedure Print_Component_Classifier_Term (Node : Node_Id);
   procedure Print_Named_Element_Identifier (N : Named_Element);

   -------------------------------------
   -- Print_Component_Classifier_Term --
   -------------------------------------

   procedure Print_Component_Classifier_Term (Node : Node_Id) is
   begin
      case AADL_Version is
         when AADL_V1 =>
            Print_Component_Category (Component_Cat (Node));
            Write_Space;
         when AADL_V2 =>
            Print_Token (T_Classifier);
            Write_Space;
            Print_Token (T_Left_Parenthesis);
      end case;

      if Identifier (Node) /= No_Node then
         Print_Entity_Reference (Node);
      end if;

      case AADL_Version is
         when AADL_V2 =>
            Print_Token (T_Right_Parenthesis);
         when others =>
            null;
      end case;
   end Print_Component_Classifier_Term;

   ----------------------------
   -- Print_Not_Boolean_Term --
   ----------------------------

   procedure Print_Not_Boolean_Term (Node : Node_Id) is
   begin
      Print_Token (T_Not);
      Write_Space;
      Print_Boolean_Term (Boolean_Term (Node));
   end Print_Not_Boolean_Term;

   -----------------------------
   -- Print_Number_Range_Term --
   -----------------------------

   procedure Print_Number_Range_Term (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Number_Range_Term);

      Delta_Term : constant Node_Id :=
        Ocarina.ME_AADL.AADL_Tree.Nodes.Delta_Term (Node);
   begin
      Print_Numeric_Term (Lower_Bound (Node));
      Write_Space;

      Print_Token (T_Interval);
      Write_Space;

      Print_Numeric_Term (Upper_Bound (Node));

      if Present (Delta_Term) then
         Write_Space;
         Print_Token (T_Delta);

         Write_Space;
         Print_Numeric_Term (Delta_Term);
      end if;
   end Print_Number_Range_Term;

   -----------------------
   -- Print_Number_Type --
   -----------------------

   procedure Print_Number_Type (Node : Node_Id) is
      Number_Kind : constant Node_Kind := Kind (Node);
      Type_Range  : constant Node_Id   :=
        Ocarina.ME_AADL.AADL_Tree.Nodes.Type_Range (Node);
      Unit_Design : constant Node_Id := Unit_Designator (Node);
   begin
      if Number_Kind = K_Real_Type then
         Print_Token (T_AADLReal);
      else
         Print_Token (T_AADLInteger);
      end if;

      if Present (Type_Range) then
         Write_Space;
         Print_Range (Type_Range);
      end if;

      if Present (Unit_Design) then
         Write_Space;
         if Kind (Unit_Design) = K_Units_Type then
            Print_Units_Type (Unit_Design);
         else
            Print_Token (T_Units);
            Write_Space;
            Print_Entity_Reference (Unit_Design);
         end if;
      end if;
   end Print_Number_Type;

   ------------------------
   -- Print_Boolean_Term --
   ------------------------

   procedure Print_Boolean_Term (Node : Node_Id) is
   begin
      case Kind (Node) is
         when K_Literal =>
            Print_Literal (Node);
         when K_Not_Boolean_Term =>
            Print_Not_Boolean_Term (Node);
         when K_And_Boolean_Term =>
            Print_And_Boolean_Term (Node);
         when K_Or_Boolean_Term =>
            Print_Or_Boolean_Term (Node);
         when K_Parenthesis_Boolean_Term =>
            Print_Parenthesis_Boolean_Term (Node);
         when K_Property_Term =>
            Print_Unique_Property_Constant_Identifier (Node);
         when others =>
            Node_Not_Handled (Node);
      end case;
   end Print_Boolean_Term;

   ---------------------------
   -- Print_Or_Boolean_Term --
   ---------------------------

   procedure Print_Or_Boolean_Term (Node : Node_Id) is
   begin
      Print_Boolean_Term (First_Term (Node));
      Write_Space;

      Print_Token (T_Or);
      Write_Space;

      Print_Boolean_Term (Second_Term (Node));
   end Print_Or_Boolean_Term;

   ------------------------------------
   -- Print_Parenthesis_Boolean_Term --
   ------------------------------------

   procedure Print_Parenthesis_Boolean_Term (Node : Node_Id) is
   begin
      Print_Token (T_Left_Parenthesis);
      Print_Boolean_Term (Boolean_Term (Node));
      Print_Token (T_Right_Parenthesis);
   end Print_Parenthesis_Boolean_Term;

   -----------------
   -- Print_Range --
   -----------------

   procedure Print_Range (Node : Node_Id) is
   begin
      Print_Numeric_Term (Lower_Bound (Node));
      Write_Space;
      Print_Token (T_Interval);
      Write_Space;
      Print_Numeric_Term (Upper_Bound (Node));
   end Print_Range;

   ----------------------
   -- Print_Range_Type --
   ----------------------

   procedure Print_Range_Type (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Range_Type);

      Number_Type : constant Node_Id :=
        Ocarina.ME_AADL.AADL_Tree.Nodes.Number_Type (Node);
      Range_Type_Kind : constant Node_Kind := Kind (Number_Type);
   begin
      Print_Tokens ((T_Range, T_Of));
      Write_Space;

      if Range_Type_Kind = K_Integer_Type
        or else Range_Type_Kind = K_Real_Type
      then
         Print_Number_Type (Number_Type);
      else
         Print_Entity_Reference (Number_Type);
      end if;
   end Print_Range_Type;

   --------------------------------------
   -- Print_Referable_Element_Category --
   --------------------------------------

   procedure Print_Referable_Element_Category (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Referable_Element_Category);
   begin
      case Referable_Element_Category'Val (Category (Node)) is
         when REC_Component_Category =>
            Print_Component_Category (Component_Cat (Node));

         when REC_Connections =>
            Print_Token (T_Connections);

         when REC_Server_Subprogram =>
            Print_Tokens ((T_Server, T_Subprogram));

         when REC_Identifier =>
            Print_Identifier (Identifier (Node));

         when REC_Subprogram_Call_Sequence =>
            Print_Component_Category (Component_Cat (Node));
            Write_Space;

      end case;
   end Print_Referable_Element_Category;

   -------------------------
   -- Print_Computed_Term --
   -------------------------

   procedure Print_Computed_Term (Node : Node_Id) is
   begin
      Print_Token (T_Compute);
      Write_Space;
      Print_Token (T_Left_Parenthesis);
      Write_Space;
      Print_Identifier (Identifier (Node));
      Write_Space;
      Print_Token (T_Right_Parenthesis);
   end Print_Computed_Term;

   -----------------------
   -- Print_Record_Term --
   -----------------------

   procedure Print_Record_Term (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Record_Term);

      List_Node : Node_Id;
   begin
      Print_Token (T_Left_Square_Bracket);
      Write_Space;

      List_Node :=
        First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.List_Items (Node));

      while Present (List_Node) loop
         case Kind (List_Node) is
            when K_Record_Term_Element =>
               Print_Record_Term_Element (List_Node);
               Print_Token (T_Semicolon);
               Write_Space;
            when others =>
               Node_Not_Handled (Node);
         end case;

         List_Node := Next_Node (List_Node);
      end loop;

      Write_Space;
      Print_Token (T_Right_Square_Bracket);
   end Print_Record_Term;

   -------------------------------
   -- Print_Record_Term_Element --
   -------------------------------

   procedure Print_Record_Term_Element (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Record_Term_Element);
   begin
      Print_Identifier (Identifier (Node));
      Write_Space;
      Print_Token (T_Association);
      Write_Space;
      Print_Property_Value (Property_Expression (Node));
   end Print_Record_Term_Element;

   -----------------------
   -- Print_Record_Type --
   -----------------------

   procedure Print_Record_Type (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Record_Type);

      List_Node : Node_Id;
   begin
      Print_Token (T_Record);
      Write_Space;
      Print_Token (T_Left_Parenthesis);

      List_Node :=
        First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.List_Items (Node));

      while Present (List_Node) loop
         if List_Node /=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.List_Items (Node))
         then
            Write_Space;
         end if;

         case Kind (List_Node) is
            when K_Record_Type_Element =>
               Print_Record_Type_Element (List_Node);
            when others =>
               Node_Not_Handled (Node);
         end case;

         List_Node := Next_Node (List_Node);
      end loop;

      Print_Token (T_Right_Parenthesis);
   end Print_Record_Type;

   -------------------------------
   -- Print_Record_Type_Element --
   -------------------------------

   procedure Print_Record_Type_Element (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Record_Type_Element);
   begin
      Print_Identifier (Identifier (Node));
      Write_Space;
      Print_Token (T_Colon);
      if Is_List (Node) then
         Write_Space;
         Print_Token (T_List);
         Write_Space;
         Print_Token (T_Of);
      end if;
      Write_Space;
      Print_Property_Type_Designator (Property_Type_Designator (Node));
      Print_Token (T_Semicolon);
   end Print_Record_Type_Element;

   --------------------------
   -- Print_Reference_Term --
   --------------------------

   procedure Print_Reference_Term (Node : Node_Id) is
   begin
      Print_Token (T_Reference);
      Write_Space;
      case AADL_Version is
         when AADL_V2 =>
            Print_Token (T_Left_Parenthesis);
            Write_Space;
            Print_Contained_Element_Path (Reference_Term (Node));
            Write_Space;
            Print_Token (T_Right_Parenthesis);
         when AADL_V1 =>
            Print_Entity_Reference (Reference_Term (Node));
      end case;
   end Print_Reference_Term;

   --------------------------
   -- Print_Reference_Type --
   --------------------------

   procedure Print_Reference_Type (L : List_Id) is
      List_Node : Node_Id;
   begin
      Print_Token (T_Reference);

      if not Is_Empty (L) then
         Write_Space;
         Print_Token (T_Left_Parenthesis);
         Write_Eol;

         if not Is_Empty (L) then
            List_Node := First_Node (L);

            while Present (List_Node) loop
               if List_Node /= First_Node (L) then
                  Print_Token (T_Comma);
                  Write_Space;
               end if;

               case Kind (List_Node) is
                  when K_Referable_Element_Category =>
                     Print_Referable_Element_Category (List_Node);
                  when K_Reference_Category =>
                     Print_Named_Element (List_Node);
                  when others =>
                     Node_Not_Handled (List_Node);
               end case;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;

         Print_Token (T_Right_Parenthesis);

      end if;
   end Print_Reference_Type;

   ------------------------
   -- Print_Numeric_Term --
   ------------------------

   procedure Print_Numeric_Term (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Minus_Numeric_Term
         or else Kind (Node) = K_Signed_AADLNumber
         or else Kind (Node) = K_Property_Term
         or else Kind (Node) = K_Unique_Property_Const_Identifier
         or else Kind (Node) = K_Entity_Reference);
   begin
      case Kind (Node) is
         when K_Minus_Numeric_Term =>
            Print_Minus_Numeric_Term (Node);
         when K_Signed_AADLNumber =>
            Print_Signed_AADLNumber (Node);
         when K_Property_Term                 |
           K_Entity_Reference                 |
           K_Unique_Property_Const_Identifier =>
            Print_Unique_Property_Constant_Identifier (Node);
         when others =>
            raise Program_Error;
      end case;
   end Print_Numeric_Term;

   ------------------------------
   -- Print_Minus_Numeric_Term --
   ------------------------------

   procedure Print_Minus_Numeric_Term (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Minus_Numeric_Term);
   begin
      Print_Token (T_Minus);
      Print_Numeric_Term (Numeric_Term (Node));
   end Print_Minus_Numeric_Term;

   -----------------------------
   -- Print_Signed_AADLNumber --
   -----------------------------

   procedure Print_Signed_AADLNumber (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Signed_AADLNumber);

      Value : constant Node_Id := Number_Value (Node);
      Unit  : constant Node_Id := Unit_Identifier (Node);

   begin
      if Kind (Value) = K_Literal then
         --  Node is a Signed_AADLReal or a Signed_AADLInteger

         Print_Literal (Value);

         if Present (Unit) then
            Write_Space;
            Print_Identifier (Unit);
         end if;
      else
         Print_Unique_Property_Constant_Identifier (Value);
      end if;
   end Print_Signed_AADLNumber;

   --------------------------
   -- Print_Property_Value --
   --------------------------

   procedure Print_Property_Value (Node : Node_Id) is
   begin
      case Kind (Node) is
         when K_Identifier =>
            Print_Identifier (Node);

         when K_Signed_AADLNumber | K_Minus_Numeric_Term =>
            Print_Numeric_Term (Node);

         when K_Literal =>
            Print_Literal (Node);

         when K_Number_Range_Term =>
            Print_Number_Range_Term (Node);

         when K_Not_Boolean_Term      |
           K_And_Boolean_Term         |
           K_Or_Boolean_Term          |
           K_Parenthesis_Boolean_Term =>
            Print_Boolean_Term (Node);

         when K_Unique_Property_Const_Identifier |
           K_Property_Term                       |
           K_Enumeration_Term                    =>
            Print_Unique_Property_Constant_Identifier (Node);

         when K_Reference_Term =>
            Print_Reference_Term (Node);

         when K_Component_Classifier_Term =>
            Print_Component_Classifier_Term (Node);

         when K_Record_Term =>
            Print_Record_Term (Node);

         when K_Computed_Term =>
            Print_Computed_Term (Node);

         when others =>
            Node_Not_Handled (Node);
      end case;
   end Print_Property_Value;

   ------------------------------------
   -- Print_Property_Type_Designator --
   ------------------------------------

   procedure Print_Property_Type_Designator (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Unique_Property_Type_Identifier
         or else Kind (Node) = K_String_Type
         or else Kind (Node) = K_Boolean_Type
         or else Kind (Node) = K_Real_Type
         or else Kind (Node) = K_Integer_Type
         or else Kind (Node) = K_Range_Type
         or else Kind (Node) = K_Enumeration_Type
         or else Kind (Node) = K_Reference_Type
         or else Kind (Node) = K_Classifier_Type
         or else Kind (Node) = K_Units_Type
         or else Kind (Node) = K_Record_Type);
   begin
      case Kind (Node) is
         when K_Unique_Property_Type_Identifier =>
            Print_Entity_Reference (Node);

         when K_String_Type =>
            Print_Token (T_AADLString);
         when K_Boolean_Type =>
            Print_Token (T_AADLBoolean);

         when K_Real_Type | K_Integer_Type =>
            Print_Number_Type (Node);

         when K_Range_Type =>
            Print_Range_Type (Node);

         when K_Enumeration_Type =>
            Print_Enumeration_Type (Node);

         when K_Reference_Type =>
            Print_Reference_Type (List_Items (Node));

         when K_Classifier_Type =>
            Print_Classifier_Type (List_Items (Node));

         when K_Units_Type =>
            Print_Units_Type (Node);

         when K_Record_Type =>
            Print_Record_Type (Node);

         when others =>
            Node_Not_Handled (Node);
      end case;
   end Print_Property_Type_Designator;

   -----------------------------------------------
   -- Print_Unique_Property_Constant_Identifier --
   -----------------------------------------------

   procedure Print_Unique_Property_Constant_Identifier (Node : Node_Id) is
      pragma Assert
        (Kind (Node) = K_Unique_Property_Const_Identifier
         or else Kind (Node) = K_Property_Term
         or else Kind (Node) = K_Enumeration_Term
         or else Kind (Node) = K_Entity_Reference);
   begin
      if AADL_Version = AADL_V1 then
         Print_Tokens ((T_Value, T_Left_Parenthesis));
      end if;

      case Kind (Node) is
         when K_Entity_Reference              |
           K_Unique_Property_Type_Identifier  |
           K_Property_Term                    |
           K_Enumeration_Term                 |
           K_Unique_Property_Const_Identifier =>
            Print_Entity_Reference (Node);
         when K_Real_Type | K_Integer_Type =>
            Print_Number_Type (Node);
         when others =>
            Node_Not_Handled (Node);
      end case;

      if AADL_Version = AADL_V1 then
         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_Unique_Property_Constant_Identifier;

   ---------------------------
   -- Print_Unit_Definition --
   ---------------------------

   procedure Print_Unit_Definition (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Unit_Definition);
   begin
      Print_Identifier (Identifier (Node));
      Write_Space;

      Print_Token (T_Association);
      Write_Space;

      Print_Identifier (Unit_Identifier (Node));
      Write_Space;

      Print_Token (T_Multiply);
      Write_Space;

      Print_Literal (Numeric_Literal (Node));
   end Print_Unit_Definition;

   ----------------------
   -- Print_Units_Type --
   ----------------------

   procedure Print_Units_Type (Node : Node_Id) is
      Definitions : constant List_Id := Unit_Definitions (Node);
      List_Node   : Node_Id;
   begin
      Print_Tokens ((T_Units, T_Left_Parenthesis));

      if No (Definitions) then
         Print_Identifier (Base_Identifier (Node));
         Print_Token (T_Right_Parenthesis);
      else
         Write_Eol;

         Increment_Indentation;
         Write_Indentation;
         Print_Identifier (Base_Identifier (Node));

         Print_Token (T_Comma);
         Write_Eol;
         Write_Indentation;

         if not Is_Empty (Definitions) then
            List_Node := First_Node (Definitions);

            while Present (List_Node) loop
               if List_Node /= First_Node (Definitions) then
                  Print_Token (T_Comma);
                  Write_Eol;
                  Write_Indentation;
               end if;

               case Kind (List_Node) is
                  when K_Unit_Definition =>
                     Print_Unit_Definition (List_Node);
                  when others =>
                     Node_Not_Handled (List_Node);
               end case;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;

         Decrement_Indentation;
         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_Units_Type;

   ----------------------------
   -- Print_And_Boolean_Term --
   ----------------------------

   procedure Print_And_Boolean_Term (Node : Node_Id) is
   begin
      Print_Boolean_Term (First_Term (Node));
      Write_Space;

      Print_Token (T_And);
      Write_Space;

      Print_Boolean_Term (Second_Term (Node));
   end Print_And_Boolean_Term;

   -------------------
   -- Print_Literal --
   -------------------

   procedure Print_Literal (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Literal);
   begin
      Write_Str (Ocarina.AADL_Values.Image (Value (Node)));
   end Print_Literal;

   ---------------------------
   -- Print_Classifier_Type --
   ---------------------------

   procedure Print_Classifier_Type (L : List_Id) is
      List_Node : Node_Id;
   begin
      Print_Token (T_Classifier);
      if not Is_Empty (L) then
         Write_Space;
         Print_Token (T_Left_Parenthesis);
         Write_Eol;

         if not Is_Empty (L) then
            List_Node := First_Node (L);

            while Present (List_Node) loop
               if List_Node /= First_Node (L) then
                  Print_Token (T_Comma);
                  Write_Space;
               end if;

               case Kind (List_Node) is
                  when K_Component_Category =>
                     Print_Component_Category (Category (List_Node));

                  when K_Classifier_Category_Ref =>
                     Print_Named_Element (List_Node);

                  when others =>
                     Node_Not_Handled (List_Node);
               end case;

               List_Node := Next_Node (List_Node);
            end loop;
         end if;

         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_Classifier_Type;

   ----------------------------
   -- Print_Enumeration_Type --
   ----------------------------

   procedure Print_Enumeration_Type (Node : Node_Id) is
      List_Node : Node_Id;
   begin
      Print_Tokens ((T_Enumeration, T_Left_Parenthesis));

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (Node));

         while Present (List_Node) loop
            if List_Node /=
              First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Identifiers (Node))
            then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            case Kind (List_Node) is
               when K_Identifier =>
                  Print_Identifier (List_Node);
               when others =>
                  Node_Not_Handled (Node);
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Print_Token (T_Right_Parenthesis);
   end Print_Enumeration_Type;

   ------------------------------------
   -- Print_Named_Element_Identifier --
   ------------------------------------

   procedure Print_Named_Element_Identifier (N : Named_Element) is
      S : String := Named_Element'Image (N);
   begin

      To_Lower (S);
      for J in S'Range loop
         if S (J) = '_' then
            S (J) := ' ';
         end if;
      end loop;
      Write_Str (S (4 .. S'Last));

   end Print_Named_Element_Identifier;

   -------------------------
   -- Print_Named_Element --
   -------------------------

   procedure Print_Named_Element (Node : Node_Id) is
      Class_Ref : constant Node_Id := Classifier_Ref (Node);
   begin
      case Named_Element'Val (Category (Node)) is
         when PO_Port_Group =>
            Print_Tokens ((T_Port, T_Group));
         when PO_Server_Subprogram =>
            Print_Tokens ((T_Server, T_Subprogram));
         when PO_Connections =>
            Print_Token (T_Connections);
         when PO_Port_Connections =>
            Print_Tokens ((T_Port, T_Connections));
         when PO_Port_Group_Connections =>
            Print_Tokens ((T_Port, T_Group, T_Connections));
         when PO_Event_Port_Connections =>
            Print_Tokens ((T_Event, T_Port, T_Connections));
         when PO_Data_Port_Connections =>
            Print_Tokens ((T_Data, T_Port, T_Connections));
         when PO_Event_Data_Port_Connections =>
            Print_Tokens ((T_Event, T_Data, T_Port, T_Connections));
         when PO_Parameter_Connections =>
            Print_Tokens ((T_Parameter, T_Connections));
         when PO_Mode =>
            Print_Token (T_Mode);
         when PO_Flow =>
            Print_Token (T_Flow);
         when PO_Port =>
            Print_Token (T_Port);
         when PO_Event_Port =>
            Print_Tokens ((T_Event, T_Port));
         when PO_Data_Port =>
            Print_Tokens ((T_Data, T_Port));
         when PO_Event_Data_Port =>
            Print_Tokens ((T_Event, T_Data, T_Port));
         when PO_Access_Connection =>
            Print_Tokens ((T_Access, T_Connections));
         when PO_Parameter =>
            Print_Token (T_Parameter);
         when PO_Identifier =>
            Print_Identifier (Identifier (Node));
         when PO_Feature_Group =>
            Print_Tokens ((T_Feature, T_Group));
         when PO_Feature =>
            Print_Token (T_Feature);
         when PO_Connection =>
            Print_Named_Element_Identifier (PO_Connection);
         when PO_Feature_Group_Connection =>
            Print_Tokens ((T_Feature, T_Group, T_Connections));
         when PO_Component_Category =>
            Print_Component_Category (Component_Cat (Node));
         when PO_Access =>
            Print_Token (T_Access);
         when PO_Component_Classifier =>
            Print_Component_Category (Component_Cat (Node));
            Write_Space;
            Print_Token (T_Classifier);
         when PO_Classifier =>
            Print_Token (T_Classifier);
         when PO_Package =>
            Print_Token (T_Package);
         when PO_Component_Implementation =>
            if Component_Category'Val (Component_Cat (Node)) = CC_Unknown then
               Print_Identifier (Identifier (Node));
            else
               Print_Component_Category (Component_Cat (Node));
            end if;
            Write_Space;
            Print_Token (T_Implementation);

            if Present (Class_Ref) then
               Write_Space;
               Print_Entity_Reference (Class_Ref);
            end if;
         when PO_Component_Access =>
            if Component_Category'Val (Component_Cat (Node)) = CC_Unknown then
               Print_Identifier (Identifier (Node));
            else
               Print_Component_Category (Component_Cat (Node));
            end if;
            Write_Space;
            Print_Token (T_Access);

         when others =>
            Print_Named_Element_Identifier
              (Named_Element'Val (Category (Node)));
      end case;
   end Print_Named_Element;

   ----------------------------------
   -- Print_Contained_Element_Path --
   ----------------------------------

   procedure Print_Contained_Element_Path (Node : Node_Id) is
      Contained_Elts : List_Id;
      List_Node      : Node_Id;
      Ann_Path       : Node_Id;

   begin
      Contained_Elts := List_Items (Node);
      Ann_Path       := Annex_Path (Node);

      if not Is_Empty (Contained_Elts) then
         List_Node := First_Node (Contained_Elts);

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Identifier =>
                  Print_Identifier (List_Node);

               when K_Array_Selection =>
                  Print_Array_Selections (List_Node);

               when others =>
                  raise Program_Error;
            end case;

            exit when No (Next_Node (List_Node));
            Print_Token (T_Dot);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if Present (Ann_Path) then
         Print_Annex_Path (Ann_Path);
      end if;

   end Print_Contained_Element_Path;

end Ocarina.BE_AADL.Properties.Values;
