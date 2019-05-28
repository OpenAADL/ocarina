------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . C _ T R E E . G E N E R A T O R     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2019 ESA & ISAE.      --
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

with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;
with Utils;          use Utils;
with Outfiles;       use Outfiles;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ocarina.Backends.Utils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.Messages;

package body Ocarina.Backends.C_Tree.Generator is

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.C_Tree.Nodes;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Values;
   use Ocarina.Backends.Messages;

   procedure Generate_Define_Statement (N : Node_Id);
   procedure Generate_Pointer_Type (N : Node_Id);
   procedure Generate_Constant_Type (N : Node_Id);
   procedure Generate_Array_Declaration (N : Node_Id);
   procedure Generate_Base_Type (N : Node_Id);
   procedure Generate_C_Comment (N : Node_Id);
   procedure Generate_Call_Profile (N : Node_Id);
   procedure Generate_Macro_Call (N : Node_Id);
   procedure Generate_Doxygen_C_Comment (N : Node_Id);
   procedure Generate_HI_Distributed_Application (N : Node_Id);
   procedure Generate_HI_Node (N : Node_Id);
   procedure Generate_Assignment_Statement (N : Node_Id);
   procedure Generate_Defining_Identifier (N : Node_Id);
   procedure Generate_Expression (N : Node_Id);
   procedure Generate_Enum_Aggregate (N : Node_Id);
   procedure Generate_Array_Values (N : Node_Id);
   procedure Generate_Array_Value (N : Node_Id);
   procedure Generate_For_Statement (N : Node_Id);
   procedure Generate_Full_Type_Declaration (N : Node_Id);
   procedure Generate_Function_Implementation (N : Node_Id);
   procedure Generate_Function_Specification (N : Node_Id);
   procedure Generate_If_Statement (N : Node_Id);
   procedure Generate_Literal (N : Node_Id);
   procedure Generate_Member_Declaration (N : Node_Id);
   procedure Generate_Variable_Declaration (N : Node_Id);
   procedure Generate_Parameter (N : Node_Id);
   procedure Generate_Parameter_List (L : List_Id);
   procedure Generate_Return_Statement (N : Node_Id);
   procedure Generate_Struct_Aggregate (N : Node_Id);
   procedure Generate_Type_Conversion (N : Node_Id);
   procedure Generate_Union_Aggregate (N : Node_Id);
   procedure Generate_While_Statement (N : Node_Id);
   procedure Generate_Source_File (N : Node_Id);
   procedure Generate_Header_File (N : Node_Id);
   procedure Generate_HI_Unit (N : Node_Id);
   procedure Generate_Included_Files (N : List_Id);
   procedure Generate_Include_Clause (N : Node_Id);
   procedure Generate_Ifdef_Clause (N : Node_Id);
   procedure Generate_Variable_Address (N : Node_Id);
   procedure Generate_Member_Designator (N : Node_Id);
   procedure Generate_Switch_Alternative (N : Node_Id);
   procedure Generate_Switch_Statement (N : Node_Id);
   procedure Generate_Extern_Entity_Declaration (N : Node_Id);

   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   procedure Generate_Statement_Delimiter (N : Node_Id);

   function Get_File_Name (N : Node_Id) return Name_Id;
   --  Generate a C file name from the package node given as parameter

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (N : Node_Id) return Name_Id is
      Header_Suffix : constant String := ".h";
      Source_Suffix : constant String := ".c";
   begin
      --  The File name corresponding is the lowerd name of N

      Get_Name_String
        (Conventional_Base_Name (Name (Defining_Identifier (N))));

      --  Adding file suffix

      if Kind (N) = K_Header_File then
         Add_Str_To_Name_Buffer (Header_Suffix);
      else
         Add_Str_To_Name_Buffer (Source_Suffix);
      end if;

      return Name_Find;
   end Get_File_Name;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Header_File =>
            Generate_Header_File (N);

         when K_Source_File =>
            Generate_Source_File (N);

         when K_C_Comment =>
            Generate_C_Comment (N);

         when K_HI_Distributed_Application =>
            Generate_HI_Distributed_Application (N);

         when K_HI_Unit =>
            Generate_HI_Unit (N);

         when K_HI_Node =>
            Generate_HI_Node (N);

         when K_Include_Clause =>
            Generate_Include_Clause (N);

         when K_Ifdef_Clause =>
            Generate_Ifdef_Clause (N);

         when K_Assignment_Statement =>
            Generate_Assignment_Statement (N);

         when K_Doxygen_C_Comment =>
            Generate_Doxygen_C_Comment (N);

         when K_Call_Profile =>
            Generate_Call_Profile (N);

         when K_Macro_Call =>
            Generate_Macro_Call (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Expression =>
            Generate_Expression (N);

         when K_Enum_Aggregate =>
            Generate_Enum_Aggregate (N);

         when K_For_Statement =>
            Generate_For_Statement (N);

         when K_While_Statement =>
            Generate_While_Statement (N);

         when K_Full_Type_Declaration =>
            Generate_Full_Type_Declaration (N);

         when K_If_Statement =>
            Generate_If_Statement (N);

         when K_Function_Implementation =>
            Generate_Function_Implementation (N);

         when K_Function_Specification =>
            Generate_Function_Specification (N);

         when K_Literal =>
            Generate_Literal (N);

         when K_Extern_Entity_Declaration =>
            Generate_Extern_Entity_Declaration (N);

         when K_Array_Values =>
            Generate_Array_Values (N);

         when K_Array_Value =>
            Generate_Array_Value (N);

         when K_Member_Declaration =>
            Generate_Member_Declaration (N);

         when K_Variable_Declaration =>
            Generate_Variable_Declaration (N);

         when K_Return_Statement =>
            Generate_Return_Statement (N);

         when K_Struct_Aggregate =>
            Generate_Struct_Aggregate (N);

         when K_Type_Conversion =>
            Generate_Type_Conversion (N);

         when K_Union_Aggregate =>
            Generate_Union_Aggregate (N);

         when K_Define_Statement =>
            Generate_Define_Statement (N);

         when K_Pointer_Type =>
            Generate_Pointer_Type (N);

         when K_Constant_Type =>
            Generate_Constant_Type (N);

         when K_Variable_Address =>
            Generate_Variable_Address (N);

         when K_Member_Designator =>
            Generate_Member_Designator (N);

         when K_Switch_Statement =>
            Generate_Switch_Statement (N);

         when K_Switch_Alternative =>
            Generate_Switch_Alternative (N);

         when K_Array_Declaration =>
            Generate_Array_Declaration (N);

         when K_Float .. K_Void =>
            Generate_Base_Type (N);

         when others =>
            Display_Error ("other element in generator", Fatal => False);
            null;
      end case;
   end Generate;

   --------------------------
   -- Generate_C_Comment --
   --------------------------

   procedure Generate_C_Comment (N : Node_Id) is
      --  This procedure does the following :

      --  * It generates a C comment basing on the name of node N

      --  * If the name it too long, and depending on the location of
      --    the comment in the source code, the procedure splits the
      --    comment into more than a line.

      --  The comment is assumed to be a sequence of caracters,
      --  beginning and ending with a NON-SPACE caracter.

      --  A word is :

      --  a space character, or else a sequence of non space
      --  characters located between two spaces.

      --  The maximum length of a line, in colums
      Max_Line_Length : constant Natural := 78;

      function Are_There_More_Words return Boolean;
      --  This function returns True if there are words in the buffer

      function Next_Word_Length return Natural;
      --  This function returns the size of the next word to be
      --  got. It returns zero when the buffer is empty.

      function Get_Next_Word return String;
      --  This function extracts the next word from the buffer

      --------------------------
      -- Are_There_More_Words --
      --------------------------

      function Are_There_More_Words return Boolean is
      begin
         return (Name_Len /= 0);
      end Are_There_More_Words;

      ----------------------
      -- Next_Word_Length --
      ----------------------

      function Next_Word_Length return Natural is
         L : Natural;
      begin
         if not Are_There_More_Words then
            L := 0;
         elsif Name_Buffer (1) = ' ' then
            L := 1;
         else
            L := 0;
            while L + 1 <= Name_Len and then Name_Buffer (L + 1) /= ' ' loop
               L := L + 1;
            end loop;
         end if;
         return L;
      end Next_Word_Length;

      -------------------
      -- Get_Next_Word --
      -------------------

      function Get_Next_Word return String is
         L : constant Natural := Next_Word_Length;
      begin
         if L = 0 then
            return "";
         else
            declare
               Next_Word : constant String := Name_Buffer (1 .. L);
            begin
               if Name_Len = L then
                  Name_Len := 0;
               else
                  Set_Str_To_Name_Buffer (Name_Buffer (L + 1 .. Name_Len));
               end if;
               return Next_Word;
            end;
         end if;
      end Get_Next_Word;

      First_Line   : Boolean := True;
      Used_Columns : Natural;
   begin
      Get_Name_String (Name (Defining_Identifier (N)));

      while Are_There_More_Words loop
         Used_Columns := N_Space;
         if First_Line then
            First_Line := False;
         else
            Write_Indentation;
         end if;

         --  We consume 4 colums

         Used_Columns := Used_Columns + 2;
         Write_Str ("/*");

         if Has_Header_Spaces (N) then
            Used_Columns := Used_Columns + 2;
            Write_Str ("  ");
         end if;

         Used_Columns := Used_Columns + Next_Word_Length;
         Write_Str (Get_Next_Word);

         while Are_There_More_Words
           and then (Used_Columns + Next_Word_Length < Max_Line_Length)
         loop
            Used_Columns := Used_Columns + Next_Word_Length;
            Write_Str (Get_Next_Word);
         end loop;
         Write_Str ("*/");

         if Are_There_More_Words then
            Write_Eol;
         end if;
      end loop;
      Write_Eol;
   end Generate_C_Comment;

   --------------------------
   -- Generate_Doxygen_C_Comment --
   --------------------------

   procedure Generate_Doxygen_C_Comment (N : Node_Id) is
      --  This procedure does the following :

      --  * It generates a C comment basing on the name of node N

      --  * If the name it too long, and depending on the location of
      --    the comment in the source code, the procedure splits the
      --    comment into more than a line.

      --  The comment is assumed to be a sequence of caracters,
      --  beginning and ending with a NON-SPACE caracter.

      --  A word is :

      --  a space character, or else a sequence of non space
      --  characters located between two spaces.

      --  The maximum length of a line, in colums
      Max_Line_Length : constant Natural := 78;

      function Are_There_More_Words return Boolean;
      --  This function returns True if there are words in the buffer

      function Next_Word_Length return Natural;
      --  This function returns the size of the next word to be
      --  got. It returns zero when the buffer is empty.

      function Get_Next_Word return String;
      --  This function extracts the next word from the buffer

      --------------------------
      -- Are_There_More_Words --
      --------------------------

      function Are_There_More_Words return Boolean is
      begin
         return (Name_Len /= 0);
      end Are_There_More_Words;

      ----------------------
      -- Next_Word_Length --
      ----------------------

      function Next_Word_Length return Natural is
         L : Natural;
      begin
         if not Are_There_More_Words then
            L := 0;
         elsif Name_Buffer (1) = ' ' then
            L := 1;
         else
            L := 0;
            while L + 1 <= Name_Len and then Name_Buffer (L + 1) /= ' ' loop
               L := L + 1;
            end loop;
         end if;
         return L;
      end Next_Word_Length;

      -------------------
      -- Get_Next_Word --
      -------------------

      function Get_Next_Word return String is
         L : constant Natural := Next_Word_Length;
      begin
         if L = 0 then
            return "";
         else
            declare
               Next_Word : constant String := Name_Buffer (1 .. L);
            begin
               if Name_Len = L then
                  Name_Len := 0;
               else
                  Set_Str_To_Name_Buffer (Name_Buffer (L + 1 .. Name_Len));
               end if;
               return Next_Word;
            end;
         end if;
      end Get_Next_Word;

      Used_Columns : Natural;
   begin
      Used_Columns := N_Space;
      Used_Columns := Used_Columns + 3;
      Write_Eol;
      Write_Str ("/*!");

      if For_Struct (N) then
         Write_Eol;
         Write_Str (" * \struct ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Union (N) then
         Write_Eol;
         Write_Str (" * \union ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Enum (N) then
         Write_Eol;
         Write_Str (" * \enum ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Function (N) then
         Write_Eol;
         Write_Str (" * \fn ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Variable (N) then
         Write_Eol;
         Write_Str (" * \var ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Define (N) then
         Write_Eol;
         Write_Str (" * \def ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Typedef (N) then
         Write_Eol;
         Write_Str (" * \typedef ");
         Write_Name (Name (Element (N)));
      end if;

      if For_File (N) then
         Write_Eol;
         Write_Str (" * \file ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Namespace (N) then
         Write_Eol;
         Write_Str (" * \namespace ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Package (N) then
         Write_Eol;
         Write_Str (" * \package ");
         Write_Name (Name (Element (N)));
      end if;

      if For_Interface (N) then
         Write_Eol;
         Write_Str (" * \interface ");
         Write_Name (Name (Element (N)));
      end if;

      if Summary (N) /= No_Node then
         Write_Eol;
         Write_Str (" * \brief ");
         Write_Name (Name (Summary (N)));
         Write_Eol;
         Write_Str (" *");
      end if;

      Write_Eol;
      Get_Name_String (Name (Description (N)));
      while Are_There_More_Words loop
         Used_Columns := N_Space;

         --  We consume 4 colums

         Used_Columns := Used_Columns + 2;
         Write_Str (" * ");
         if Has_Header_Spaces (N) then
            Used_Columns := Used_Columns + 2;
            Write_Str ("  ");
         end if;

         Used_Columns := Used_Columns + Next_Word_Length;
         Write_Str (Get_Next_Word);

         while Are_There_More_Words
           and then (Used_Columns + Next_Word_Length < Max_Line_Length)
         loop
            Used_Columns := Used_Columns + Next_Word_Length;
            Write_Str (Get_Next_Word);
         end loop;

         if Are_There_More_Words then
            Write_Eol;
         end if;
      end loop;
      Write_Eol;
      Write_Str (" */");
      Write_Eol;
   end Generate_Doxygen_C_Comment;

   -----------------------------------
   -- Generate_Assignment_Statement --
   -----------------------------------

   procedure Generate_Assignment_Statement (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Equal);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Expression (N));
      Decrement_Indentation;
   end Generate_Assignment_Statement;

   --------------------------------
   -- Generate_Array_Declaration --
   --------------------------------

   procedure Generate_Array_Declaration (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
      Write (Tok_Left_Hook);
      Generate (Array_Size (N));
      Write (Tok_Right_Hook);
   end Generate_Array_Declaration;

   ----------------------------------------
   -- Generate_Extern_Entity_Declaration --
   ----------------------------------------

   procedure Generate_Extern_Entity_Declaration (N : Node_Id) is
   begin
      Write (Tok_Extern);
      Write_Space;
      Generate (Entity (N));
   end Generate_Extern_Entity_Declaration;

   ---------------------------
   -- Generate_Array_Values --
   ---------------------------

   procedure Generate_Array_Values (N : Node_Id) is
      D : Node_Id := First_Node (Values (N));
   begin
      Write (Tok_Left_Brace);
      while Present (D) loop
         Generate (D);
         D := Next_Node (D);
         if Present (D) then
            Write (Tok_Comma);
         end if;
      end loop;
      Write (Tok_Right_Brace);
   end Generate_Array_Values;

   --------------------------
   -- Generate_Array_Value --
   --------------------------

   procedure Generate_Array_Value (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
      Write (Tok_Left_Hook);
      Generate (Array_Item (N));
      Write (Tok_Right_Hook);
   end Generate_Array_Value;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (N : Node_Id) is
   begin
      if Is_Pointer (N) then
         Write (Tok_Asterisk);
      end if;

      if Is_Variable_Address (N) then
         Write (Tok_Ampersand);
      end if;

      Write_Name (Name (N));
   end Generate_Defining_Identifier;

   -------------------------
   -- Generate_Expression --
   -------------------------

   procedure Generate_Expression (N : Node_Id) is
      L_Expr : constant Node_Id     := Left_Expression (N);
      Op     : constant Operator_Id := Operator (N);
      R_Expr : constant Node_Id     := Right_Expression (N);
   begin
      --  Each expression having a right part and a left part is
      --  systematically put between two parentheses.

      if Get_Name_String (Operator_Image (Standard.Integer (Op))) = "*"
        or else
          Get_Name_String (Operator_Image (Standard.Integer (Op))) = "/"
        or else
          Get_Name_String (Operator_Image (Standard.Integer (Op))) = "&&"
        or else
          Get_Name_String (Operator_Image (Standard.Integer (Op))) = "||"
      then
         if Kind (L_Expr) = K_Expression then
            Write (Tok_Left_Paren);
            Generate (L_Expr);
            Write (Tok_Right_Paren);
         else
            Generate (L_Expr);
         end if;

         Write_Space;
         Write_Name (Operator_Image (Standard.Integer (Op)));
         Write_Space;

         if Kind (R_Expr) = K_Expression then
            Write (Tok_Left_Paren);
            Generate (R_Expr);
            Write (Tok_Right_Paren);
         else
            Generate (R_Expr);
         end if;

      elsif
        Get_Name_String (Operator_Image (Standard.Integer (Op))) = "!"
      then
         Write_Name (Operator_Image (Standard.Integer (Op)));
         if Kind (L_Expr) = K_Expression then
            Write (Tok_Left_Paren);
            Generate (L_Expr);
            Write (Tok_Right_Paren);
         else
            Generate (L_Expr);
         end if;
      elsif
        Get_Name_String (Operator_Image (Standard.Integer (Op))) = "++"
      then
         Generate (L_Expr);
         Write_Name (Operator_Image (Standard.Integer (Op)));
      else
         Generate (L_Expr);
         Write_Space;
         Write_Name (Operator_Image (Standard.Integer (Op)));
         Write_Space;
         Generate (R_Expr);
      end if;

   end Generate_Expression;

   ----------------------------
   -- Generate_For_Statement --
   ----------------------------

   procedure Generate_For_Statement (N : Node_Id) is
      D : Node_Id := First_Node (Statements (N));
   begin
      Write (Tok_For);
      Write_Space;
      Write (Tok_Left_Paren);
      Generate (Pre_Cond (N));
      Write (Tok_Semicolon);
      Generate (Condition (N));
      Write (Tok_Semicolon);
      Generate (Post_Cond (N));
      Write (Tok_Right_Paren);
      Write_Eol;
      Write (Tok_Left_Brace);
      Increment_Indentation;
      while Present (D) loop
         Write_Indentation;
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_For_Statement;

   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------

   procedure Generate_Full_Type_Declaration (N : Node_Id) is
   begin
      Write (Tok_Typedef);
      Write_Space;
      Generate (Type_Definition (N));
      Write_Space;
      Generate (Type_Name (N));
--      Write_Name (Name (Defining_Identifier (N)));
   end Generate_Full_Type_Declaration;

   ---------------------------
   -- Generate_If_Statement --
   ---------------------------

   procedure Generate_If_Statement (N : Node_Id) is
      T : constant List_Id := Statements (N);
      E : constant List_Id := Else_Statements (N);
      I : Node_Id;

   begin
      --  Enter If_Statement

      Write_Str ("/* :: Yes if commentary :: */");
      Write (Tok_If);
      Write_Space;
      Write (Tok_Left_Paren);
      Generate (Condition (N));
      Write (Tok_Right_Paren);
      Write_Eol;
      Write_Indentation;
      Write (Tok_Left_Brace);
      Write_Eol;
      Write_Indentation;

      --  If_Statement cannot be empty. A null statement is always
      --  there if needed.

      Increment_Indentation;
      I := First_Node (T);
      while Present (I) loop
         Write_Indentation;
         Generate (I);
         Generate_Statement_Delimiter (I);
         I := Next_Node (I);
      end loop;
      Write_Eol;
      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);

      --  Else_Statement can be empty

      if not Is_Empty (E) then
         Write_Indentation;
         Write (Tok_Else);
         Write_Eol;
         Write (Tok_Left_Brace);
         Write_Eol;
         Increment_Indentation;
         I := First_Node (E);
         while Present (I) loop
            Write_Indentation;
            Generate (I);
            Generate_Statement_Delimiter (I);
            I := Next_Node (I);
         end loop;
         Decrement_Indentation;
         Write_Eol;
         Write_Indentation;
         Write (Tok_Right_Brace);
         Write_Eol;
      end if;
   end Generate_If_Statement;

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (N : Node_Id) is
   begin
      Write_Str (Image (Value (N)));
   end Generate_Literal;

   -----------------------------
   -- Generate_While_Statement --
   -----------------------------

   procedure Generate_While_Statement (N : Node_Id) is
      D : Node_Id := First_Node (Statements (N));
   begin
      Write (Tok_While);
      Write_Space;
      Write (Tok_Left_Paren);
      Generate (Condition (N));
      Write (Tok_Right_Paren);
      Write_Eol;
      Write_Indentation;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;
      while Present (D) loop
         Write_Indentation;
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_While_Statement;

   ------------------------
   -- Generate_Parameter --
   ------------------------

   procedure Generate_Parameter (N : Node_Id) is
   begin
      Generate (Parameter_Type (N));
      Write_Space;
      Name_Buffer (1 .. Var_Name_Len) := (others => ' ');
      Get_Name_String (Name (Defining_Identifier (N)));

      if Var_Name_Len > Name_Len then
         Name_Len := Var_Name_Len;
      end if;

      Write_Str (Name_Buffer (1 .. Name_Len));
   end Generate_Parameter;

   -----------------------------
   -- Generate_Parameter_List --
   -----------------------------

   procedure Generate_Parameter_List (L : List_Id) is
      N : Node_Id;

   begin
      --  If we got there, then L is not empty.
      if Is_Empty (L) then
         Write (Tok_Left_Paren);
         Write_Str ("void");
         Write (Tok_Right_Paren);
         return;
      end if;

      Write_Eol;
      Increment_Indentation;
      Increment_Indentation;
      Write_Indentation;
      Write (Tok_Left_Paren);

      N := First_Node (L);
      loop
         Generate_Parameter (N);
         exit when No (Next_Node (N));
         Write (Tok_Comma);
         N := Next_Node (N);
         Write_Eol;
         Write_Indentation;
      end loop;

      Write (Tok_Right_Paren);
      Decrement_Indentation;
      Decrement_Indentation;
      Write_Indentation;
   end Generate_Parameter_List;

   -------------------------------
   -- Generate_Return_Statement --
   -------------------------------

   procedure Generate_Return_Statement (N : Node_Id) is
      E : constant Node_Id := Expression (N);
   begin
      Write (Tok_Return);

      if Present (E) then
         Write_Space;
         Write (Tok_Left_Paren);
         Generate (E);
         Write (Tok_Right_Paren);
      end if;
   end Generate_Return_Statement;

   ---------------------------
   -- Generate_Call_Profile --
   ---------------------------

   procedure Generate_Call_Profile (N : Node_Id) is
      L : constant List_Id := Parameters (N);
      P : Node_Id;

   begin
      Generate (Defining_Identifier (N));

      Write_Space;

      Write (Tok_Left_Paren);
      if not Is_Empty (L) then
         P := First_Node (L);
         loop
            Generate (P);
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Comma);
            Write_Space;
         end loop;
      end if;
      Write (Tok_Right_Paren);
   end Generate_Call_Profile;

   -------------------------
   -- Generate_Macro_Call --
   -------------------------

   procedure Generate_Macro_Call (N : Node_Id) is
      L : constant List_Id := Parameters (N);
      P : Node_Id;
   begin
      Generate (Defining_Identifier (N));

      Write (Tok_Left_Paren);
      if not Is_Empty (L) then
         P := First_Node (L);
         loop
            Generate (P);
            P := Next_Node (P);
            exit when No (P);
            Write (Tok_Comma);
            Write_Space;
         end loop;
      end if;
      Write (Tok_Right_Paren);
   end Generate_Macro_Call;

   --------------------------------------
   -- Generate_Function_Implementation --
   --------------------------------------

   procedure Generate_Function_Implementation (N : Node_Id) is
      D : constant List_Id := Declarations (N);
      S : constant List_Id := Statements (N);
      P : constant Node_Id := Specification (N);
      M : Node_Id;
   begin
      Write_Indentation;
      Generate (P);

      Write_Eol;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      if not Is_Empty (D) then
         M := First_Node (D);
         while Present (M) loop
            Write_Indentation;
            Generate (M);
            Generate_Statement_Delimiter (M);
            M := Next_Node (M);
         end loop;
      end if;

      Write_Eol;

      if not Is_Empty (S) then
         M := First_Node (S);
         while Present (M) loop
            Write_Indentation;
            Generate (M);
            Generate_Statement_Delimiter (M);
            M := Next_Node (M);
         end loop;
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
      Write_Eol;
   end Generate_Function_Implementation;

   ---------------------------------------
   -- Generate_Function_Specification --
   ---------------------------------------

   procedure Generate_Function_Specification (N : Node_Id) is
      P : constant List_Id := Parameters (N);
      T : constant Node_Id := Return_Type (N);
   begin
      --  If we deal with a main subprogram, then we generate its
      --  withed packages

      if T /= No_Node then
         Generate (T);
      end if;

      if Present (Defining_Identifier (N)) then
         Write_Space;
         Write_Name (Name (Defining_Identifier (N)));
      end if;

      Write_Space;
      Generate_Parameter_List (P);

   end Generate_Function_Specification;

   ------------------------------
   -- Generate_Struct_Aggregate --
   ------------------------------

   procedure Generate_Struct_Aggregate (N : Node_Id) is
      P : Node_Id := First_Node (Struct_Members (N));
   begin
      Write (Tok_Struct);
      Write_Eol;
      Write_Indentation;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
         Write_Eol;
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Struct_Aggregate;

   -----------------------------
   -- Generate_Enum_Aggregate --
   -----------------------------

   procedure Generate_Enum_Aggregate (N : Node_Id) is
      P : Node_Id := First_Node (Enum_Members (N));
   begin
      Write (Tok_Enum);
      Write_Eol;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         P := Next_Node (P);
         if Present (P) then
            Write (Tok_Comma);
         end if;
         Write_Eol;
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Enum_Aggregate;

   ------------------------------
   -- Generate_Union_Aggregate --
   ------------------------------

   procedure Generate_Union_Aggregate (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Union);
      Write_Eol;
      Write_Indentation;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;
      if not Is_Empty (Union_Members (N)) then
         P := First_Node (Union_Members (N));
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
            Write_Eol;
         end loop;
      end if;
      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Union_Aggregate;

   -------------------------------
   -- Generate_Switch_Statement --
   -------------------------------

   procedure Generate_Switch_Statement (N : Node_Id) is
      P : Node_Id;
   begin

      if Is_Empty (Alternatives (N)) then
         return;
      end if;

      Write (Tok_Switch);
      Write_Space;
      Write (Tok_Left_Paren);
      Generate (Expression (N));
      Write (Tok_Right_Paren);
      Write_Eol;
      Write_Indentation;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      P := First_Node (Alternatives (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         P := Next_Node (P);
         Write_Eol;
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Switch_Statement;

   ---------------------------------
   -- Generate_Switch_Alternative --
   ---------------------------------

   procedure Generate_Switch_Alternative (N : Node_Id) is
      P : Node_Id;
   begin
      if Is_Empty (Labels (N)) then
         Write (Tok_Default);
         Write (Tok_Colon);
      else
         P := First_Node (Labels (N));
         while Present (P) loop
            Write (Tok_Case);
            Write_Space;
            Generate (P);
            Write (Tok_Colon);
            P := Next_Node (P);
         end loop;
      end if;
      Write_Eol;
      Write_Indentation;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;
      if not Is_Empty (Statements (N)) then
         P := First_Node (Statements (N));
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
            Write_Eol;
         end loop;
      end if;

      Write_Indentation;
      Write (Tok_Break);
      Write (Tok_Semicolon);
      Write_Eol;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Switch_Alternative;

   -----------------------------------
   -- Generate_Variable_Declaration --
   -----------------------------------

   procedure Generate_Variable_Declaration (N : Node_Id) is
   begin
      if Is_Static (N) then
         Write (Tok_Static);
         Write_Space;
      end if;
      Generate (Used_Type (N));
      Write_Space;
      Generate (Defining_Identifier (N));
      if not No (Initialization_Value (N)) then
         Write_Space;
         Write (Tok_Equal);
         Write_Space;
         Generate (Initialization_Value (N));
      end if;
   end Generate_Variable_Declaration;

   ---------------------------------
   -- Generate_Member_Declaration --
   ---------------------------------

   procedure Generate_Member_Declaration (N : Node_Id) is
   begin
      Generate (Used_Type (N));
      Write_Space;
      Generate (Defining_Identifier (N));
   end Generate_Member_Declaration;

   -----------------------------------------
   -- Generate_HI_Distributed_Application --
   -----------------------------------------

   procedure Generate_HI_Distributed_Application (N : Node_Id) is
      P                     : Node_Id := First_Node (HI_Nodes (N));
      Application_Directory : Name_Id;
   begin
      --  Create the application directory (a lower case string)

      Get_Name_String (Name (N));
      Application_Directory := To_Lower (Name_Find);

      Create_Directory (Application_Directory);

      --  Process the application nodes

      Enter_Directory (Application_Directory);

      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;

      Leave_Directory;
   end Generate_HI_Distributed_Application;

   ----------------------
   -- Generate_HI_Node --
   ----------------------

   procedure Generate_HI_Node (N : Node_Id) is
      U                   : Node_Id          := First_Node (Units (N));
      Partition_Directory : constant Name_Id := To_Lower (Name (N));
   begin
      --  Create the node directory

      Create_Directory (Partition_Directory);
      Enter_Directory (Partition_Directory);

      while Present (U) loop
         Generate (U);
         U := Next_Node (U);
      end loop;

      Leave_Directory;
   end Generate_HI_Node;

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Write_Name (Token_Image (T));
   end Write;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (T : Token_Type) is
   begin
      Write (T);
      Write_Eol;
   end Write_Line;

   ----------------------------------
   -- Generate_Statement_Delimiter --
   ----------------------------------

   procedure Generate_Statement_Delimiter (N : Node_Id) is
   begin
      if No (N)
        or else Kind (N) = K_Define_Statement
        or else Kind (N) = K_Switch_Statement
        or else Kind (N) = K_Switch_Alternative
        or else Kind (N) = K_While_Statement
        or else Kind (N) = K_If_Statement
        or else Kind (N) = K_Function_Implementation
      then
         Write_Eol;
      elsif Kind (N) /= K_C_Comment
        and then Kind (N) /= K_Doxygen_C_Comment
      then
         Write_Line (Tok_Semicolon);
      end if;
   end Generate_Statement_Delimiter;

   ------------------------------
   -- Generate_Type_Conversion --
   ------------------------------

   procedure Generate_Type_Conversion (N : Node_Id) is
   begin
      Increment_Indentation;
      Write (Tok_Left_Paren);
      Generate (Subtype_Mark (N));
      Write (Tok_Right_Paren);
      Generate (Expression (N));
      Decrement_Indentation;
   end Generate_Type_Conversion;

   --------------------------
   -- Generate_Source_File --
   --------------------------

   procedure Generate_Source_File (N : Node_Id) is
      Fd : File_Descriptor;
      D  : Node_Id := First_Node (Declarations (N));
   begin
      if No (N) then
         return;
      end if;
      Fd := Set_Output (Get_File_Name (N));

      if not Is_Empty (Included_Headers (N)) then
         Generate_Included_Files (Included_Headers (N));
      end if;

      while Present (D) loop
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
      end loop;
      Write_Eol;

      --  Always leave a blank line at the end of a C-source file

      Release_Output (Fd);
   end Generate_Source_File;

   -----------------------------
   -- Generate_Included_Files --
   -----------------------------

   procedure Generate_Included_Files (N : List_Id) is
      H : Node_Id := First_Node (N);
   begin
      while Present (H) loop
         Write (Tok_Sharp);
         Write (Tok_Include);
         Write_Space;

         if Is_Local (H) then
            Write (Tok_Quote);
         else
            Write (Tok_Less);
         end if;

         Generate (Header_Name (H));
         Write (Tok_Dot);
         Write_Str ("h");

         if Is_Local (H) then
            Write (Tok_Quote);
         else
            Write (Tok_Greater);
         end if;

         Write_Eol;
         H := Next_Node (H);
      end loop;
   end Generate_Included_Files;

   --------------------------
   -- Generate_Header_File --
   --------------------------

   procedure Generate_Header_File (N : Node_Id) is
      Fd : File_Descriptor;
      D  : Node_Id := First_Node (Declarations (N));
      NA : Name_Id;
   begin
      if No (D) then
         return;
      end if;
      NA := Name (Defining_Identifier (N));
      NA := To_Upper (NA);

      Fd := Set_Output (Get_File_Name (N));

      Write (Tok_Sharp);
      Write (Tok_Ifndef);
      Write_Space;
      Write (Tok_Underscore);
      Write (Tok_Underscore);
      Write_Str ("OCARINA_GENERATED_");
      Write_Name (NA);
      Write (Tok_Underscore);
      Write_Str ("H");
      Write (Tok_Underscore);
      Write_Eol;

      Write (Tok_Sharp);
      Write (Tok_Define);
      Write_Space;
      Write (Tok_Underscore);
      Write (Tok_Underscore);
      Write_Str ("OCARINA_GENERATED_");
      Write_Name (NA);
      Write (Tok_Underscore);
      Write_Str ("H");
      Write (Tok_Underscore);
      Write_Space;
      Write_Eol;

      if not Is_Empty (Included_Headers (N)) then
         Generate_Included_Files (Included_Headers (N));
      end if;

      while Present (D) loop
         Generate (D);
         Generate_Statement_Delimiter (D);
         Write_Eol;
         D := Next_Node (D);
      end loop;
      Write (Tok_Sharp);
      Write (Tok_Endif);

      Write_Eol;

      --  Always leave a blank line at the end of a C-source file

      Release_Output (Fd);
   end Generate_Header_File;

   ------------------------
   -- Generate_Base_Type --
   ------------------------

   procedure Generate_Base_Type (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Int =>
            Write_Str ("int");

         when K_Float =>
            Write_Str ("float");

         when K_Char =>
            Write_Str ("char");

         when K_Void =>
            Write_Str ("void");

         when others =>
            Display_Error ("other element in generator", Fatal => False);
            null;
      end case;

   end Generate_Base_Type;

   ----------------------
   -- Generate_HI_Unit --
   ----------------------

   procedure Generate_HI_Unit (N : Node_Id) is
      S : Node_Id := First_Node (Sources (N));
      H : Node_Id := First_Node (Headers (N));
   begin
      while Present (S) loop
         Generate (S);
         S := Next_Node (S);
      end loop;
      while Present (H) loop
         Generate (H);
         H := Next_Node (H);
      end loop;
   end Generate_HI_Unit;

   -------------------------------
   -- Generate_Define_Statement --
   -------------------------------

   procedure Generate_Define_Statement (N : Node_Id) is
      V : constant Node_Id := Defined_Value (N);
      I : constant Node_Id := Defining_Identifier (N);
   begin
      Write (Tok_Sharp);
      Write (Tok_Define);
      Write_Space;
      Generate (I);
      Write_Space;
      Generate (V);
   end Generate_Define_Statement;

   ---------------------------
   -- Generate_Pointer_Type --
   ---------------------------

   procedure Generate_Pointer_Type (N : Node_Id) is
   begin
      Generate (Used_Type (N));
      Write (Tok_Asterisk);
   end Generate_Pointer_Type;

   ----------------------------
   -- Generate_Constant_Type --
   ----------------------------

   procedure Generate_Constant_Type (N : Node_Id) is
   begin
      Write (Tok_Const);
      Write_Space;
      Generate (Used_Type (N));
   end Generate_Constant_Type;

   -------------------------------
   -- Generate_Variable_Address --
   -------------------------------

   procedure Generate_Variable_Address (N : Node_Id) is
   begin
      Write (Tok_Ampersand);
      Write (Tok_Left_Paren);
      Generate (Expression (N));
      Write (Tok_Right_Paren);
   end Generate_Variable_Address;

   --------------------------------
   -- Generate_Member_Designator --
   --------------------------------

   procedure Generate_Member_Designator (N : Node_Id) is
   begin
      Generate (Aggregate_Name (N));
      if Is_Pointer (N) then
         Write (Tok_Arrow);
      else
         Write (Tok_Dot);
      end if;
      Generate (Defining_Identifier (N));
   end Generate_Member_Designator;

   -----------------------------
   -- Generate_Include_Clause --
   -----------------------------

   procedure Generate_Include_Clause (N : Node_Id) is
   begin
      Write (Tok_Sharp);
      Write (Tok_Include);
      Write_Space;

      if Is_Local (N) then
         Write (Tok_Quote);
      else
         Write (Tok_Less);
      end if;

      Generate (Header_Name (N));
      Write (Tok_Dot);
      Write_Str ("h");

      if Is_Local (N) then
         Write (Tok_Quote);
      else
         Write (Tok_Greater);
      end if;

      Write_Eol;
   end Generate_Include_Clause;

   ---------------------------
   -- Generate_Ifdef_Clause --
   ---------------------------

   procedure Generate_Ifdef_Clause (N : Node_Id) is
      S : Node_Id;
   begin
      Write (Tok_Sharp);
      if Negation (N) then
         Write (Tok_Ifndef);
      else
         Write (Tok_Ifdef);
      end if;

      Write_Space;

      Generate (Clause (N));

      Write_Eol;

      S := First_Node (Then_Statements (N));
      while Present (S) loop
         Generate (S);
         S := Next_Node (S);
      end loop;

      if not Is_Empty (Else_Statements (N)) then
         S := First_Node (Else_Statements (S));
         while Present (S) loop
            Generate (S);
            S := Next_Node (S);
         end loop;
      end if;

      Write (Tok_Sharp);
      Write (Tok_Endif);

      Write_Eol;

   end Generate_Ifdef_Clause;

end Ocarina.Backends.C_Tree.Generator;
