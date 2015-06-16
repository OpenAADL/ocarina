------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . F E _ A A D L . P A R S E R                --
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

with Locations; use Locations;
with Ocarina.Namet;     use Ocarina.Namet;
with Ocarina.Output;    use Ocarina.Output;

with Ocarina.FE_AADL.Lexer;             use Ocarina.FE_AADL.Lexer;
with Ocarina.FE_AADL.Parser.Namespaces; use Ocarina.FE_AADL.Parser.Namespaces;
with Ocarina.ME_AADL.Tokens;            use Ocarina.ME_AADL.Tokens;
with Ocarina.ME_AADL.AADL_Tree.Nodes;   use Ocarina.ME_AADL.AADL_Tree.Nodes;

with Ocarina.Files;                    use Ocarina.Files;
with Ocarina.ME_AADL.AADL_Tree.Nutils; use Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Options;                  use Ocarina.Options;
with Ocarina.Parser;                   use Ocarina.Parser;
with Ocarina.Property_Sets;            use Ocarina.Property_Sets;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

package body Ocarina.FE_AADL.Parser is

   Language : constant String := "aadl";
   Suffix   : constant String := ".aadl";

   procedure Exit_On_Error (Error : Boolean; Reason : String);

   -------------------
   -- Exit_On_Error --
   -------------------

   procedure Exit_On_Error (Error : Boolean; Reason : String) is
   begin
      if Error then
         Set_Standard_Error;
         Write_Line (Reason);
         OS_Exit (1);
      end if;
   end Exit_On_Error;

   ----------
   -- Init --
   ----------

   procedure Init is
      C : Character;
   begin
      First_Parsing     := True;
      Add_Pre_Prop_Sets := False;

      Initialize_Option_Scan;
      loop
         C := Getopt ("* y s f I:");
         case C is
            when ASCII.NUL =>
               exit;

            when 'y' =>
               Auto_Load_AADL_Files := True;

            when 'I' =>
               Add_Library_Path (Parameter);

            when 'f' | 's' =>
               Add_Pre_Prop_Sets := True;

            when others =>
               null;

         end case;
      end loop;
      Add_Library_Path (Get_Name_String (Default_Library_Path));

      Ocarina.Parser.Register_Parser (Language, Process'Access);
   end Init;

   ------------------
   -- P_Items_List --
   ------------------

   --  ( { Item }+ | none_statement )

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Code      : Parsing_Code) return Integer
   is
      Loc      : Location;
      Nb_Items : Integer := 0;
      Item     : Node_Id;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_None then
         if not P_None_Statement then
            return -1;
         end if;
      else
         Restore_Lexer (Loc);
         loop
            Save_Lexer (Loc);
            Item := Func.all (Container);
            if Present (Item) then
               Nb_Items := Nb_Items + 1;
            else
               --  Error when parsing item, restore lexer

               Restore_Lexer (Loc);
               if Nb_Items = 0 then
                  --  list must contain at least one element, {Item}+

                  Nb_Items := -1;
                  DPE (Code, EMC_List_Is_Empty);
               end if;
               exit;
            end if;
         end loop;
      end if;

      return Nb_Items;
   end P_Items_List;

   ------------------
   -- P_Items_List --
   ------------------

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Code      : Parsing_Code) return List_Id
   is
      Loc   : Location;
      Items : List_Id;
      Item  : Node_Id;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      Items := New_List (K_List_Id, Token_Location);
      if Token = T_None then
         if not P_None_Statement then
            return No_List;
         end if;

      else
         Restore_Lexer (Loc);
         loop
            Save_Lexer (Loc);
            Item := Func.all (Container);
            if Present (Item) then
               Append_Node_To_List (Item, Items);
            else
               --  Error when parsing item, restore lexer
               Restore_Lexer (Loc);
               if Is_Empty (Items) then
                  --  list must contain at least one element, { Item }+
                  DPE (Code, EMC_List_Is_Empty);
               end if;
               exit;
            end if;
         end loop;
      end if;

      return Items;
   end P_Items_List;

   ------------------
   -- P_Items_List --
   ------------------

   function P_Items_List
     (Func         : P_Refinable_Item_Function_Ptr;
      Container    : Node_Id;
      Refinable    : Boolean;
      Code         : Parsing_Code;
      At_Least_One : Boolean := True) return Integer
   is
      Loc   : Location;
      Item  : Node_Id;
      Items : Integer := 0;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_None then
         if not P_None_Statement then
            return -1;
         end if;

      else
         Restore_Lexer (Loc);
         loop
            Save_Lexer (Loc);
            Item := Func.all (Container, Refinable);
            if Present (Item) then
               Items := Items + 1;
            else
               --  Error when parsing item, restore lexer

               Restore_Lexer (Loc);
               if At_Least_One and then Items = 0 then
                  --  list must contain at least one element, {Item}+

                  DPE (Code, EMC_List_Is_Empty);
                  Items := -1;
               end if;
               exit;
            end if;
         end loop;
      end if;

      return Items;
   end P_Items_List;

   --------------------
   -- P_Element_List --
   --------------------

   --  parse ( { Element }* Element Delimiter )

   function P_Elements_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Delimiter : Ocarina.ME_AADL.Tokens.Token_Type) return List_Id
   is
      Element       : Node_Id;
      Elements_List : List_Id;
      Loc           : Location;

   begin
      Elements_List := New_List (K_List_Id, Token_Location);
      loop
         Element := Func.all (Container);
         if Present (Element) then
            Append_Node_To_List (Element, Elements_List);
         else
            Skip_Tokens (Delimiter);
            return No_List;
         end if;

         Save_Lexer (Loc);
         Scan_Token;

         if Token = Delimiter then
            exit;
         else
            Restore_Lexer (Loc);
         end if;
      end loop;

      Set_Loc
        (Node_Id (Elements_List),
         Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (First_Node (Elements_List)));
      return Elements_List;
   end P_Elements_List;

   ------------------
   -- P_Items_List --
   ------------------

   --  ( { Item Separator }* Item )

   function P_Items_List
     (Func      : P_Item_Function_Ptr;
      Container : Node_Id;
      Separator : Ocarina.ME_AADL.Tokens.Token_Type) return List_Id
   is
      Item  : Node_Id;
      Items : List_Id;
      Loc   : Location;

   begin
      Items := New_List (K_List_Id, Token_Location);
      loop
         Item := Func.all (Container);
         if Present (Item) then
            Append_Node_To_List (Item, Items);
         else
            return No_List;
         end if;

         Save_Lexer (Loc);
         Scan_Token;
         if Token /= Separator then
            Restore_Lexer (Loc);
            exit;
         end if;
      end loop;

      Set_Loc
        (Node_Id (Items),
         Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (First_Node (Items)));
      return Items;
   end P_Items_List;

   ------------------
   -- P_Items_List --
   ------------------

   --  ( [ { Item Separator }* Item Delimiter ] )
   --    or ( [ { Item Separator }* Item Separator Delimiter ] )

   function P_Items_List
     (Func            : P_Item_Function_Ptr;
      Container       : Node_Id;
      Separator       : Ocarina.ME_AADL.Tokens.Token_Type;
      Delimiter       : Ocarina.ME_AADL.Tokens.Token_Type;
      Code            : Parsing_Code;
      With_Terminator : Boolean := False) return List_Id
   is
      Item      : Node_Id;
      Items     : List_Id;
      Start_Loc : Location;
      Loc       : Location;

   begin
      Items := New_List (K_List_Id, Token_Location);
      loop
         Item := Func.all (Container);
         if Present (Item) then
            Append_Node_To_List (Item, Items);
         else
            return No_List;
         end if;

         Save_Lexer (Start_Loc);
         Scan_Token;

         if Token = Separator then

            if With_Terminator then
               Save_Lexer (Loc);
               Scan_Token;

               if Token = Delimiter then
                  exit;
               end if;
            end if;

         elsif Token = Delimiter then
            exit;

         else
            DPE (Code, (Separator, Delimiter));
            Skip_Tokens (Delimiter);
            return No_List;
         end if;
      end loop;

      Set_Loc
        (Node_Id (Items),
         Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (First_Node (Items)));
      return Items;
   end P_Items_List;

   ----------------------
   -- P_None_Statement --
   ----------------------

   --  none_statement ::= none ;

   function P_None_Statement return Boolean is
      Loc : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Semicolon then
         return True;
      else
         DPE (PC_None_Statement, T_Semicolon);
         Restore_Lexer (Loc);
         return False;
      end if;
   end P_None_Statement;

   --------------------------------------
   -- Process_Predefined_Property_Sets --
   --------------------------------------

   function Process_Predefined_Property_Sets
     (AADL_Root : Node_Id) return Node_Id
   is
      New_Root  : Node_Id := AADL_Root;
      File_Name : Name_Id;
      Buffer    : Location;
   begin
      for J in Ocarina_Property_Sets'Range loop
         Set_Str_To_Name_Buffer (Image (Ocarina_Property_Sets (J)));
         Add_Str_To_Name_Buffer (Suffix);
         File_Name := Search_File (Name_Find);
         if File_Name = No_Name then
            New_Root := No_Node;
            exit;
         end if;
         Buffer := Lexer.Load_File (File_Name);
         if Buffer = No_Location then
            New_Root := No_Node;
            exit;
         end if;
         New_Root := Process (New_Root, Buffer);
      end loop;
      Exit_On_Error (No (New_Root), "Cannot parse ocarina property sets");
      return New_Root;
   end Process_Predefined_Property_Sets;

   -------------
   -- Process --
   -------------

   function Process
     (AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location) return Node_Id
   is
      New_Root  : Node_Id := AADL_Root;
      File_Name : Name_Id;
      Buffer    : Location;

   begin
      if First_Parsing then
         First_Parsing := False;

         --  First parse the files declaring the standard property sets.

         for J in Standard_Property_Sets'Range loop
            Set_Str_To_Name_Buffer (Image (Standard_Property_Sets (J)));
            Add_Str_To_Name_Buffer (Suffix);
            File_Name := Search_File (Name_Find);
            if File_Name = No_Name then
               New_Root := No_Node;
               Exit_On_Error
                 (No (New_Root),
                  "Cannot find standard property set " &
                  Image (Standard_Property_Sets (J)));
               exit;
            end if;
            Buffer := Lexer.Load_File (File_Name);
            if Buffer = No_Location then
               New_Root := No_Node;
               exit;
            end if;
            New_Root := Process (New_Root, Buffer);
         end loop;
         Exit_On_Error (No (New_Root), "Cannot parse standard property sets");

         --  When the user explicitely wants to, parse the files
         --  declaring the Ocarina property sets.

         if Add_Pre_Prop_Sets then
            New_Root := Process_Predefined_Property_Sets (New_Root);
         end if;
      end if;

      Buffer := From;
      if To /= No_Location then
         Buffer.EOF := To.Last_Pos;
      end if;
      Restore_Lexer (Buffer);

      return P_AADL_Specification (New_Root);
   end Process;

end Ocarina.FE_AADL.Parser;
