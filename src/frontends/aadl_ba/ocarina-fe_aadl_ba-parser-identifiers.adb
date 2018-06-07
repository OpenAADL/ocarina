------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.FE_AADL_BA.PARSER.IDENTIFIERS                   --
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

with Locations;
with Ocarina.Namet;
with Ocarina.AADL_Values;
with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.FE_AADL_BA.Lexer;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;

package body Ocarina.FE_AADL_BA.Parser.Identifiers is

   -----------------------------
   -- Make_Current_Identifier --
   -----------------------------

   function Make_Current_Identifier (Entity  : Node_Id) return Node_Id is
      use Ocarina.FE_AADL_BA.Lexer;
      use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
      use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

      Node : constant Node_Id := New_Node (K_Identifier, Token_Location);

   begin
      Set_Name (Node, Token_Name);
      Set_Display_Name (Node, Token_Display_Name);
      Set_Corresponding_Entity (Node, Entity);

      return Node;
   end Make_Current_Identifier;

   ------------------
   -- P_Identifier --
   ------------------

   function P_Identifier (Container : Types.Node_Id) return Node_Id is
      use Locations;
      use Ocarina.ME_AADL_BA.Tokens;
      use Ocarina.FE_AADL_BA.Lexer;

      Loc : Location;
   begin
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Identifier then
         return Make_Current_Identifier (Container);
      else
         Restore_Lexer (Loc);
         return No_Node;
      end if;
   end P_Identifier;

   -----------------------------
   -- P_Identifier_With_Value --
   -----------------------------

   --  in_event_data_port_identifier ( ( value_constant | others ) )

   function P_Identifier_With_Value
     (Container : Types.Node_Id)
     return Node_Id is
      use Locations;
      use Ocarina.AADL_Values;
      use Ocarina.ME_AADL_BA.Tokens;
      use Ocarina.FE_AADL_BA.Lexer;
      use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
      use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

      Loc            : Location;
      Node           : Node_Id;
      Value_Node     : Node_Id  := No_Node;
      Bool_Is_Others : Boolean  := False;
   begin
      Node := P_Identifier (Container);

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Parenthesis then
         Save_Lexer (Loc);
         Scan_Token;

         case Token is
            when T_Real_Literal =>
               Value_Node := New_Node (K_Literal, Token_Location);
               Set_Value (Value_Node,
                          New_Real_Value (Float_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));

            when T_Integer_Literal =>
               Value_Node := New_Node (K_Literal, Token_Location);
               Set_Value (Value_Node,
                          New_Integer_Value (Integer_Literal_Value,
                                             False,
                                             Numeric_Literal_Base,
                                             Numeric_Literal_Exp));

            when T_Identifier =>
               Restore_Lexer (Loc);
               Value_Node := P_Identifier (No_Node);

            when T_Others =>
               Value_Node := No_Node;
               Bool_Is_Others := True;

            when others =>
               DPE (PC_Identifier_With_Value,
                    Expected_Tokens => (T_Real_Literal, T_Integer_Literal,
                                        T_Identifier, T_Others));
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;

         if No (Value_Node)
           and then not Bool_Is_Others
         then
            DPE (PC_Identifier_With_Value, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Right_Parenthesis then
            DPE (PC_Identifier_With_Value,
                 Expected_Token => T_Right_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

      else
         Restore_Lexer (Loc);
      end if;

      if Present (Value_Node)
        or else Bool_Is_Others
      then
         Set_Kind (Node, K_Identifier_With_Value);
         Set_Value_Constant (Node, Value_Node);
         Set_Is_Others (Node, Bool_Is_Others);
      end if;

      if No (Node) then
         DPE (PC_Identifier_With_Value, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Node;
      end if;

   end P_Identifier_With_Value;

   -----------------------------------
   -- P_Unique_Classifier_Reference --
   -----------------------------------

   --  <core AADL rule> : see AADL proposed draft

   --  unique_component_classifier_reference :=
   --    unique_component_type_reference
   --  | unique_component_implementation_reference

   --  unique_component_type_reference ::=
   --    [ package_name :: ] component_type_identifier

   --  unique_component_implemantation_reference ::=
   --    [ package_name :: ] component_implementation_identifier

   --  package_name ::=
   --    { package_identifier :: }* package_identifier

   function P_Unique_Classifier_Reference
     (Container : Types.Node_Id)
     return Node_Id
   is
      use Locations;
      use Namet;
      use Ocarina.ME_AADL_BA.Tokens;
      use Ocarina.FE_AADL_BA.Lexer;
      use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
      use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

      Loc             : Location;
      Escape          : Boolean  := False;
      Item            : Node_Id;
      List_Node       : Node_Id;
      Comp_Type_Ident : Node_Id;
      Comp_Impl_Ident : Node_Id  := No_Node;
      Pack_Name_List  : List_Id  := No_List;

      Full_Ident      : constant Node_Id
        := New_Node (K_Identifier, Token_Location);
      Classifier_Ref  : constant Node_Id
        := New_Node (K_Component_Classifier_Ref, Token_Location);
   begin
      Pack_Name_List := New_List (K_List_Id, Token_Location);

      loop
         Item := P_Identifier (No_Node);
         if No (Item) then
            Scan_Token;
            DPE (PC_Unique_Component_Classifier_Ref,
                 Expected_Token => T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         if Present (Item) then
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Colon_Colon then
               Append_Node_To_List (Item, Pack_Name_List);

            elsif Token = T_Dot then
               Comp_Type_Ident := Item;

               Comp_Impl_Ident := P_Identifier (No_Node);
               if No (Comp_Impl_Ident) then
                  Scan_Token;
                  DPE (PC_Unique_Component_Classifier_Ref,
                       Expected_Token => T_Identifier);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Escape := True;
            else
               Restore_Lexer (Loc);
               Comp_Type_Ident := Item;

               Escape := True;
            end if;
         else
            Pack_Name_List := No_List;
            exit;
         end if;

         if Escape then
            exit;
         end if;
      end loop;

      if not Is_Empty (Pack_Name_List) then
         Set_Loc (Node_Id (Pack_Name_List),
                  Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                         (Pack_Name_List)));
      end if;

      if No (Classifier_Ref) then
         DPE (PC_Unique_Component_Classifier_Ref, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         --  Classifier_Ref node is already builded
         --  XXX fixme : todo builder for Unique_Classifier_Reference

         Set_BE_Container (Classifier_Ref, Container);
         Set_Package_Name (Classifier_Ref, Pack_Name_List);
         Set_Component_Type (Classifier_Ref, Comp_Type_Ident);
         Set_Component_Impl (Classifier_Ref, Comp_Impl_Ident);

         --  Build Full_Identifier Name
         if not Is_Empty (Package_Name (Classifier_Ref)) then
            List_Node := First_Node (Package_Name (Classifier_Ref));
            Get_Name_String (Name (List_Node));

            List_Node := Next_Node (List_Node);
            while Present (List_Node) loop
               Add_Str_To_Name_Buffer ("::");
               Get_Name_String_And_Append (Name (List_Node));

               List_Node := Next_Node (List_Node);
            end loop;

            Add_Str_To_Name_Buffer ("::");
         end if;

         if Present (Component_Type (Classifier_Ref)) then
            Get_Name_String_And_Append (Name (Component_Type
                                                (Classifier_Ref)));
         end if;

         if Present (Component_Impl (Classifier_Ref)) then
            Add_Str_To_Name_Buffer (".");
            Get_Name_String_And_Append (Name (Component_Impl
                                                (Classifier_Ref)));
         end if;

         Set_Name (Full_Ident, Name_Find);

         --  Build Full_Identifier Display_Name
         if not Is_Empty (Package_Name (Classifier_Ref)) then
            List_Node := First_Node (Package_Name (Classifier_Ref));
            Get_Name_String (Display_Name (List_Node));

            List_Node := Next_Node (List_Node);
            while Present (List_Node) loop
               Add_Str_To_Name_Buffer ("::");
               Get_Name_String_And_Append (Display_Name (List_Node));

               List_Node := Next_Node (List_Node);
            end loop;

            Add_Str_To_Name_Buffer ("::");
         end if;

         if Present (Component_Type (Classifier_Ref)) then
            Get_Name_String_And_Append (Display_Name (Component_Type
                                                        (Classifier_Ref)));
         end if;

         if Present (Component_Impl (Classifier_Ref)) then
            Add_Str_To_Name_Buffer (".");
            Get_Name_String_And_Append (Display_Name (Component_Impl
                                                        (Classifier_Ref)));
         end if;

         Set_Display_Name (Full_Ident, Name_Find);

         Set_Full_Identifier (Classifier_Ref, Full_Ident);

         return Classifier_Ref;
      end if;

   end P_Unique_Classifier_Reference;

end Ocarina.FE_AADL_BA.Parser.Identifiers;
