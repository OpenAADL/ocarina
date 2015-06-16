------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B E _ A A D L . P R O P E R T I E S            --
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

with Ocarina.Output;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.BE_AADL.Identifiers;
with Ocarina.BE_AADL.Components;
with Ocarina.BE_AADL.Components.Modes;
with Ocarina.BE_AADL.Properties.Values;

package body Ocarina.BE_AADL.Properties is

   use Ocarina.Output;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.BE_AADL.Identifiers;
   use Ocarina.BE_AADL.Components;
   use Ocarina.BE_AADL.Components.Modes;
   use Ocarina.BE_AADL.Properties.Values;

   procedure Print_Applies_To (Node : Node_Id);
   procedure Print_Property_Values (Node : Node_Id);

   ----------------------
   -- Print_Applies_To --
   ----------------------

   procedure Print_Applies_To (Node : Node_Id) is
      List_Node : Node_Id;
   begin
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;
      Print_Tokens ((T_Applies, T_To, T_Left_Parenthesis));

      if Is_All (Node) then
         Write_Space;
         Print_Token (T_All);
      else
         if not Is_Empty (Owner_Categories (Node)) then
            List_Node := First_Node (Owner_Categories (Node));

            while Present (List_Node) loop
               if List_Node /= First_Node (Owner_Categories (Node)) then
                  Print_Token (T_Comma);
                  Write_Space;
               end if;

               Print_Named_Element (List_Node);
               List_Node := Next_Node (List_Node);
            end loop;
         end if;
      end if;

      Print_Token (T_Right_Parenthesis);
      Decrement_Indentation;
   end Print_Applies_To;

   -------------------------------------------
   -- Print_Contained_Property_Associations --
   -------------------------------------------

   procedure Print_Contained_Property_Associations (List : List_Id) is
      List_Node : Node_Id;

   begin
      if not Is_Empty (List) then
         Write_Eol;
         Increment_Indentation;

         List_Node := First_Node (List);

         while Present (List_Node) loop
            Write_Indentation;

            if List_Node = First_Node (List) then
               Print_Token (T_Left_Curly_Bracket);
            else
               Write_Space;
            end if;

            Print_Property_Association (List_Node, Contained => True);

            if List_Node = Last_Node (List) then
               Print_Token (T_Right_Curly_Bracket);
            else
               Write_Eol;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
      end if;
   end Print_Contained_Property_Associations;

   --------------------------------
   -- Print_Property_Association --
   --------------------------------

   procedure Print_Property_Association
     (Node      : Node_Id;
      Contained : Boolean := False)
   is
      Prop_Value : constant Node_Id := Property_Association_Value (Node);
      Applies_To : constant List_Id := Applies_To_Prop (Node);
      Bindings   : constant Node_Id := In_Binding (Node);
      Modes      : constant Node_Id := In_Modes (Node);
      List_Node  : Node_Id;
   begin
      if not Contained then
         Write_Indentation;
      end if;

      Print_Identifier (Identifier (Node));
      Write_Space;

      if Is_Additive_Association (Node) then
         Print_Token (T_Additive_Association);
      else
         Print_Token (T_Association);
      end if;
      Write_Space;

      if Is_Constant (Node) then
         Print_Token (T_Constant);
         Write_Space;
      end if;

      if Is_Access (Node) then
         Print_Token (T_Access);
         Write_Space;
      end if;

      --  The value associated with the property

      Print_Property_Values (Prop_Value);

      --  applies to

      if not Is_Empty (Applies_To) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         Print_Tokens ((T_Applies, T_To));
         Write_Space;
         List_Node := First_Node (Applies_To);

         while Present (List_Node) loop
            Print_Contained_Element_Path (List_Node);

            exit when No (Next_Node (List_Node));
            Print_Token (T_Comma);

            List_Node := Next_Node (List_Node);
         end loop;

         Decrement_Indentation;
      end if;

      --  in bindings

      if Present (Bindings) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         Print_Tokens ((T_In, T_Binding, T_Left_Parenthesis));
         List_Node := First_Node (Binding (Bindings));

         while Present (List_Node) loop
            Write_Indentation;
            Print_Entity_Reference (List_Node);

            exit when No (Next_Node (List_Node));
            Print_Token (T_Comma);
            Write_Space;

            List_Node := Next_Node (List_Node);
         end loop;

         Print_Token (T_Right_Parenthesis);
         Decrement_Indentation;
      end if;

      --  in modes

      Print_In_Modes (Modes);
      Print_Token (T_Semicolon);

      if not Contained then
         Write_Eol;
      end if;

   end Print_Property_Association;

   -------------------------------------------
   -- Print_Property_Definition_Declaration --
   -------------------------------------------

   procedure Print_Property_Definition_Declaration (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Property_Definition_Declaration);

   begin
      Write_Indentation;
      Print_Identifier (Identifier (Node));
      Write_Space;
      Print_Token (T_Colon);
      Write_Space;

      if Is_Access (Node) then
         Print_Token (T_Access);
         Write_Space;
      end if;

      if Is_Inherit (Node) then
         Print_Token (T_Inherit);
         Write_Space;
      end if;

      if Is_List (Property_Name_Type (Node)) then
         for J in 1 .. Multiplicity (Property_Name_Type (Node)) loop
            Print_Tokens ((T_List, T_Of));
            Write_Space;
         end loop;
      end if;

      Print_Property_Type_Designator
        (Property_Type_Designator (Property_Name_Type (Node)));

      if Default_Value (Node) /= No_Node then
         Write_Space;
         Print_Token (T_Association);
         Write_Space;
         Print_Property_Values (Default_Value (Node));
      end if;

      Print_Applies_To (Applies_To (Node));
      Print_Token (T_Semicolon);
      Write_Eol;
      Write_Eol;
   end Print_Property_Definition_Declaration;

   -------------------------------------
   -- Print_Property_Type_Declaration --
   -------------------------------------

   procedure Print_Property_Type_Declaration (Node : Node_Id) is
   begin
      Write_Indentation;
      Print_Identifier (Identifier (Node));
      Write_Space;
      Print_Tokens ((T_Colon, T_Type));
      Write_Space;
      Print_Property_Type_Designator (Property_Type_Designator (Node));
      Print_Token (T_Semicolon);
      Write_Eol;
      Write_Eol;
   end Print_Property_Type_Declaration;

   ---------------------------
   -- Print_Property_Values --
   ---------------------------

   procedure Print_Property_Values (Node : Node_Id) is
      pragma Assert
        (Present (Node)
         and then (Kind (Node) = K_Property_Value or else DNKE (Node)));

      List_Node : Node_Id;
   begin
      if Single_Value (Node) = No_Node then
         --  Print Property_List_Value with new line and indents

         Print_Token (T_Left_Parenthesis);
         List_Node := First_Node (Multi_Value (Node));

         while Present (List_Node) loop
            if List_Node /= First_Node (Multi_Value (Node)) then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            Print_Property_Value (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;

         Print_Token (T_Right_Parenthesis);
      else
         --  Print Property_Expression without new line

         Print_Property_Value (Single_Value (Node));
      end if;
   end Print_Property_Values;

   -----------------------------
   -- Print_Constant_Property --
   -----------------------------

   procedure Print_Constant_Property (Node : Node_Id) is

      --  Constant_Type is
      --       AADLInteger_Type
      --    or AADLReal_Type
      --    or AADLString_Type
      --    or AADLBoolean_Type
      --    or Identifier_Identifier
      --  (see ocarina-nodes.idl for more details)

      Unit_Ident : constant Node_Id := Unique_Unit_Identifier (Node);
      --  Only used when Const_Type is AADLInteger_Type or
      --  AADLReal_Type.

      List_Node : Node_Id;
   begin
      Write_Indentation;
      Print_Identifier (Identifier (Node));
      Write_Space;

      Print_Tokens ((T_Colon, T_Constant));
      Write_Space;

      if Single_Value (Constant_Value (Node)) = No_Node then
         for J in 1 .. Multiplicity (Node) loop
            Print_Tokens ((T_List, T_Of));
            Write_Space;
         end loop;
      end if;

      case Kind (Constant_Type (Node)) is
         when K_Integer_Type =>
            Print_Token (T_AADLInteger);
         when K_Real_Type =>
            Print_Token (T_AADLReal);
         when K_Boolean_Type =>
            Print_Token (T_AADLBoolean);
         when K_String_Type =>
            Print_Token (T_AADLString);
         when K_Unique_Property_Type_Identifier =>
            Print_Entity_Reference (Constant_Type (Node));
         when K_Classifier_Type =>
            Print_Classifier_Type (List_Items (Constant_Type (Node)));
         when others =>
            Node_Not_Handled (Constant_Type (Node));
      end case;

      Write_Space;

      --  try to print unit identifier

      if Present (Unit_Ident) then
         Print_Entity_Reference (Unit_Ident);
         Write_Space;
      end if;

      Print_Token (T_Association);
      Write_Space;

      if Single_Value (Constant_Value (Node)) /= No_Node then
         Print_Property_Value (Single_Value (Constant_Value (Node)));
      else
         Print_Token (T_Left_Parenthesis);

         if Multi_Value (Constant_Value (Node)) /= No_List then
            List_Node := First_Node (Multi_Value (Constant_Value (Node)));

            while List_Node /= No_Node loop
               Print_Property_Value (List_Node);
               List_Node := Next_Node (List_Node);

               if List_Node /= No_Node then
                  Print_Token (T_Comma);
                  Write_Space;
               end if;
            end loop;
         end if;

         Print_Token (T_Right_Parenthesis);
      end if;

      Print_Token (T_Semicolon);
      Write_Eol;
   end Print_Constant_Property;

end Ocarina.BE_AADL.Properties;
