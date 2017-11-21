------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . F E _ A A D L . P A R S E R . I D E N T I F I E R S    --
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

with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Lexer;
with Ocarina.Namet;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Locations;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;

package body Ocarina.FE_AADL.Parser.Identifiers is

   -----------------------------
   -- Make_Current_Identifier --
   -----------------------------

   function Make_Current_Identifier (Entity : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      Node : constant Node_Id := New_Node (K_Identifier, Token_Location);

   begin
      Set_Name (Node, Token_Name);
      Set_Display_Name (Node, Token_Display_Name);
      Set_Corresponding_Entity (Node, Entity);

      return Node;
   end Make_Current_Identifier;

   ---------------------------
   -- P_Expected_Identifier --
   ---------------------------

   function P_Expected_Identifier (Expected_Id : Node_Id) return Boolean is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      Loc : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Identifier and then Token_Name = Name (Expected_Id) then
         return True;
      else
         DPE (PC_Defining_Identifier, Display_Name (Expected_Id));
         Restore_Lexer (Loc);
         return False;
      end if;
   end P_Expected_Identifier;

   ----------------------------
   -- P_Expected_Identifiers --
   ----------------------------

   function P_Expected_Identifiers
     (Identifiers : List_Id;
      Delimiter   : Ocarina.ME_AADL.Tokens.Token_Type) return Boolean
   is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      Identifier   : Node_Id;   --  Current identifier in list of identifiers
      Current_Name : Name_Id;   --  Name of current identifier
      Loc          : Location;

   begin
      if Is_Empty (Identifiers) then
         return True;
      end if;

      Identifier := First_Node (Identifiers);

      while Present (Identifier) loop
         Current_Name := Name (Identifier);
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Identifier then
            if Token_Name /= Current_Name then
               DPE (PC_Defining_Name, Display_Name (Identifier));
               Restore_Lexer (Loc);
               return False;
            end if;
         else
            DPE (PC_Defining_Name, Display_Name (Identifier));
            Restore_Lexer (Loc);
            return False;
         end if;

         Identifier := Next_Node (Identifier);

         if Present (Identifier) then
            --  Parse delimiter
            Save_Lexer (Loc);
            Scan_Token;

            if Token /= Delimiter then
               DPE (PC_Defining_Name, Delimiter);
               Restore_Lexer (Loc);
               return False;
            end if;
         end if;
      end loop;

      return True;
   end P_Expected_Identifiers;

   ------------------
   -- P_Identifier --
   ------------------

   function P_Identifier (Container : Ocarina.Types.Node_Id) return Node_Id is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

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
   -- P_Identifier_Refined_To --
   -----------------------------

   --  ( Identifier : [ refined to ] ) or ( [ Identifier : ] [ refined to ] )

   procedure P_Identifier_Refined_To
     (Option              :     Refinement_Type;
      Optional_Identifier :     Boolean;
      Code                :     Parsing_Code;
      Refinement_Code     :     Parsing_Code;
      Skip_Until_Token    :     Ocarina.ME_AADL.Tokens.Token_Type;
      Identifier          : out Node_Id;
      Is_Refinement       : out Boolean;
      OK                  : out Boolean)
   is
      use Locations;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;

      Loc : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;    --  parse identifier
      if Token /= T_Identifier then
         Restore_Lexer (Loc);
         if not Optional_Identifier then
            OK := False;
            return;
         end if;

         Identifier := No_Node;
      else
         Identifier := Make_Current_Identifier (No_Node);

         Scan_Token;    --  parse ':'
         if Token /= T_Colon then
            if Option = RT_Refinement then
               DPE (Refinement_Code, T_Colon);
            else
               DPE (Code, T_Colon);
            end if;
            Skip_Tokens (Skip_Until_Token);
            OK := False;
            return;
         end if;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Refined then
         if No (Identifier) then
            DPE (Code, EMC_No_Defining_Identifier);
            DPE (Code, EMC_Refinement_Is_Not_Allowed);
            Skip_Tokens (Skip_Until_Token);
            OK := False;
            return;
         end if;

         if Option = RT_Not_Refinable then
            DPE (Code, EMC_Refinement_Is_Not_Allowed);
            Skip_Tokens (Skip_Until_Token);
            OK := False;
            return;
         end if;

         Scan_Token;
         if Token /= T_To then
            DPE (Refinement_Code, T_To);
            Skip_Tokens (T_Semicolon);
            OK := False;
            return;
         end if;

         Is_Refinement := True;
      else
         if Option = RT_Refinement then
            DPE (Refinement_Code, T_Refined);
            Skip_Tokens (Skip_Until_Token);
            OK := False;
            return;
         end if;

         Restore_Lexer (Loc);
         Is_Refinement := False;
      end if;

      OK := True;
   end P_Identifier_Refined_To;

   ------------------------
   -- P_Entity_Reference --
   ------------------------

   function P_Entity_Reference
     (Container : Ocarina.Types.Node_Id) return Node_Id
   is
      pragma Unreferenced (Container);
   begin
      return P_Entity_Reference (PC_Items_List);
   end P_Entity_Reference;

   ------------------------
   -- P_Entity_Reference --
   ------------------------

   --  [ package_name :: ]* identifier [.identifier]*

   function P_Entity_Reference (Code : Parsing_Code) return Node_Id is
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Entities;
      use Ocarina.Namet;
      use Locations;
      use Ocarina.FE_AADL.Lexer;

      Loc                  : Location;
      Loc2                 : Location;
      Entity_Loc           : Location;
      Current_Identifier   : Node_Id;
      Processing_Namespace : Boolean          := True;
      Location_Set         : Boolean          := False;
      Unique_Name          : constant Node_Id :=
        New_Node (K_Entity_Reference, No_Location);
   begin
      Set_Entity (Unique_Name, No_Node);
      Save_Lexer (Loc);

      loop
         Scan_Token;
         Save_Lexer (Loc2);
         Save_Lexer (Entity_Loc);

         if not Location_Set then
            Set_Loc (Unique_Name, Entity_Loc);
            Set_Path (Unique_Name, New_List (K_List_Id, Entity_Loc));
            Set_Namespace_Path (Unique_Name, New_List (K_List_Id, Entity_Loc));

            Location_Set := True;
         end if;

         if Token = T_Identifier then
            Current_Identifier := New_Node (K_Identifier, Token_Location);
            Set_Name (Current_Identifier, Token_Name);
            Set_Display_Name (Current_Identifier, Token_Display_Name);

            if Processing_Namespace then
               Append_Node_To_List
                 (Current_Identifier,
                  Namespace_Path (Unique_Name));
            else
               Add_Path_Element_To_Entity_Reference
                 (Unique_Name,
                  Current_Identifier);
            end if;

            Save_Lexer (Loc2);
            Scan_Token;

            if Token = T_Colon_Colon then
               if not Processing_Namespace then
                  DPE (Code, T_Identifier);
                  Restore_Lexer (Loc);
                  return No_Node;
               end if;

            elsif Token = T_Dot then
               declare
                  Node : Node_Id;
               begin
                  if Processing_Namespace then
                     Node :=
                       Remove_Last_Node_From_List
                         (Namespace_Path (Unique_Name));

                     if Node /= No_Node then
                        --  The last identifier was in fact the first
                        --  element of the entity name, not an element
                        --  of the namespace name.

                        Add_Path_Element_To_Entity_Reference
                          (Unique_Name,
                           Node);
                     else
                        DPE (Code, T_Identifier);
                        Restore_Lexer (Loc);
                        return No_Node;
                     end if;

                     Processing_Namespace := False;
                  end if;
               end;

            else
               --  There is nothing more to parse

               if Processing_Namespace then
                  declare
                     Node : Node_Id;
                  begin
                     Node :=
                       Remove_Last_Node_From_List
                         (Namespace_Path (Unique_Name));

                     if Node /= No_Node then
                        Add_Path_Element_To_Entity_Reference
                          (Unique_Name,
                           Node);
                     else
                        DPE (Code, T_Identifier);
                        Restore_Lexer (Loc);
                        return No_Node;
                     end if;
                  end;
               end if;

               Restore_Lexer (Loc2);

               declare
                  List_Node : Node_Id;
               begin
                  if First_Node (Namespace_Path (Unique_Name)) /= No_Node then
                     --  set the namespace name

                     Set_Namespace_Identifier
                       (Unique_Name,
                        New_Node (K_Identifier, Entity_Loc));
                     Set_Corresponding_Entity
                       (Namespace_Identifier (Unique_Name),
                        Unique_Name);

                     List_Node := First_Node (Namespace_Path (Unique_Name));

                     while List_Node /= No_Node loop
                        if List_Node =
                          First_Node (Namespace_Path (Unique_Name))
                        then
                           Get_Name_String (Name (List_Node));
                        else
                           Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
                           Get_Name_String_And_Append (Name (List_Node));
                        end if;

                        List_Node := Next_Node (List_Node);
                     end loop;

                     Set_Name (Namespace_Identifier (Unique_Name), Name_Find);

                     --  set the namespace display name

                     List_Node := First_Node (Namespace_Path (Unique_Name));

                     while List_Node /= No_Node loop
                        if List_Node =
                          First_Node (Namespace_Path (Unique_Name))
                        then
                           Get_Name_String (Display_Name (List_Node));
                        else
                           Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
                           Get_Name_String_And_Append
                             (Display_Name (List_Node));
                        end if;

                        List_Node := Next_Node (List_Node);
                     end loop;

                     Set_Display_Name
                       (Namespace_Identifier (Unique_Name),
                        Name_Find);
                  else
                     Set_Namespace_Identifier (Unique_Name, No_Node);
                  end if;

                  --  set the entity name

                  Set_Identifier
                    (Unique_Name,
                     New_Node (K_Identifier, Entity_Loc));
                  Set_Corresponding_Entity
                    (Identifier (Unique_Name),
                     Unique_Name);
                  List_Node := First_Node (Path (Unique_Name));

                  while List_Node /= No_Node loop
                     if List_Node = First_Node (Path (Unique_Name)) then
                        Get_Name_String (Name (Item (List_Node)));
                     else
                        Add_Str_To_Name_Buffer (Image (T_Dot));
                        Get_Name_String_And_Append (Name (Item (List_Node)));
                     end if;

                     List_Node := Next_Node (List_Node);
                  end loop;

                  Set_Name (Identifier (Unique_Name), Name_Find);

                  --  set the entity display name

                  List_Node := First_Node (Path (Unique_Name));

                  while List_Node /= No_Node loop
                     if List_Node = First_Node (Path (Unique_Name)) then
                        Set_Str_To_Name_Buffer
                          (Get_Name_String (Display_Name (Item (List_Node))));
                     else
                        Add_Str_To_Name_Buffer (Image (T_Dot));
                        Get_Name_String_And_Append
                          (Display_Name (Item (List_Node)));
                     end if;

                     List_Node := Next_Node (List_Node);
                  end loop;

                  Set_Display_Name (Identifier (Unique_Name), Name_Find);
               end;

               Set_Full_Identifier
                 (Unique_Name,
                  New_Node (K_Identifier, Entity_Loc));
               Set_Corresponding_Entity
                 (Full_Identifier (Unique_Name),
                  Unique_Name);

               if Namespace_Identifier (Unique_Name) /= No_Node then
                  Get_Name_String (Name (Namespace_Identifier (Unique_Name)));
                  Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
                  Get_Name_String_And_Append (Name (Identifier (Unique_Name)));
                  Set_Name (Full_Identifier (Unique_Name), Name_Find);
                  Get_Name_String
                    (Display_Name (Namespace_Identifier (Unique_Name)));
                  Add_Str_To_Name_Buffer (Image (T_Colon_Colon));
                  Get_Name_String_And_Append
                    (Display_Name (Identifier (Unique_Name)));
                  Set_Display_Name (Full_Identifier (Unique_Name), Name_Find);
               else
                  Set_Name
                    (Full_Identifier (Unique_Name),
                     Name (Identifier (Unique_Name)));
                  Set_Display_Name
                    (Full_Identifier (Unique_Name),
                     Display_Name (Identifier (Unique_Name)));
               end if;

               return Unique_Name;
            end if;

         else
            DPE (Code, T_Identifier);
            Restore_Lexer (Loc);
            return No_Node;
         end if;
      end loop;

   end P_Entity_Reference;

end Ocarina.FE_AADL.Parser.Identifiers;
