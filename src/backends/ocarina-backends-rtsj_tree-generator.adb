------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.RTSJ_TREE.GENERATOR                    --
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

with Ocarina.Namet;    use Ocarina.Namet;
with Ocarina.Output;   use Ocarina.Output;
with Utils;    use Utils;
with Outfiles; use Outfiles;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ocarina.Backends.Utils;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.RTSJ_Values;
with Ocarina.Backends.Messages;

package body Ocarina.Backends.RTSJ_Tree.Generator is

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.RTSJ_Values;
   use Ocarina.Backends.Messages;

   procedure Generate_Variable_Declaration (N : Node_Id);
   procedure Generate_Assignment_Statement (N : Node_Id);
   procedure Generate_Return_Statement (N : Node_Id);
   procedure Generate_Null_Statement;
   procedure Generate_Try_Statement (N : Node_Id);
   procedure Generate_Catch_Statement (N : Node_Id);
   procedure Generate_Package_Statement (N : Node_Id);
   procedure Generate_Class_Statement (N : Node_Id);
   procedure Generate_Visibility (N : Node_Id);
   procedure Generate_Java_Comment (N : Node_Id);
   procedure Generate_New_Statement (N : Node_Id);
   procedure Generate_Base_Type (N : Node_Id);
   procedure Generate_Enumerator (N : Node_Id);
   procedure Generate_Defining_Identifier (N : Node_Id);
   procedure Generate_Expression (N : Node_Id);
   procedure Generate_Array_Declaration (N : Node_Id);
   procedure Generate_Array_Value (N : Node_Id);
   procedure Generate_Full_Array_Declaration (N : Node_Id);
   procedure Generate_Literal (N : Node_Id);
   procedure Generate_Pointed_Notation (N : Node_Id);
   procedure Generate_Function_Specification (N : Node_Id);
   procedure Generate_Function_Implementation (N : Node_Id);
   procedure Generate_Call_Function (N : Node_Id);
   procedure Generate_Parameter_Specification (N : Node_Id);
   procedure Generate_Cast_Statement (N : Node_Id);
   procedure Generate_Switch_Statement (N : Node_Id);
   procedure Generate_Case_Statement (N : Node_Id);
   procedure Generate_Throw_Statement (N : Node_Id);
   procedure Generate_For_Statement (N : Node_Id);
   procedure Generate_Source_File (N : Node_Id);
   procedure Generate_Imported_Files (N : List_Id);
   procedure Generate_HI_Distributed_Application (N : Node_Id);
   procedure Generate_HI_Node (N : Node_Id);
   procedure Generate_HI_Unit (N : Node_Id);

   procedure Generate_Statement_Delimiter (N : Node_Id);
   procedure Generate_Comment_Box (M : Name_Id);

   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);
   procedure Write_List (L : List_Id; Has_Brackets : Boolean);
   function Get_File_Name (N : Node_Id) return Name_Id;

   --------------
   -- Generate --
   --------------
   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Variable_Declaration =>
            Generate_Variable_Declaration (N);

         when K_Assignment_Statement =>
            Generate_Assignment_Statement (N);

         when K_Return_Statement =>
            Generate_Return_Statement (N);

         when K_Null_Statement =>
            Generate_Null_Statement;

         when K_Try_Statement =>
            Generate_Try_Statement (N);

         when K_Catch_Statement =>
            Generate_Catch_Statement (N);

         when K_Package_Statement =>
            Generate_Package_Statement (N);

         when K_Class_Statement =>
            Generate_Class_Statement (N);

         when K_Java_Comment =>
            Generate_Java_Comment (N);

         when K_New_Statement =>
            Generate_New_Statement (N);

         when K_Int .. K_Void =>
            Generate_Base_Type (N);

         when K_Visibility =>
            Generate_Visibility (N);

         when K_Enumerator =>
            Generate_Enumerator (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Expression =>
            Generate_Expression (N);

         when K_Array_Declaration =>
            Generate_Array_Declaration (N);

         when K_Array_Value =>
            Generate_Array_Value (N);

         when K_Full_Array_Declaration =>
            Generate_Full_Array_Declaration (N);

         when K_Literal =>
            Generate_Literal (N);

         when K_Pointed_Notation =>
            Generate_Pointed_Notation (N);

         when K_Function_Specification =>
            Generate_Function_Specification (N);

         when K_Function_Implementation =>
            Generate_Function_Implementation (N);

         when K_Call_Function =>
            Generate_Call_Function (N);

         when K_Parameter_Specification =>
            Generate_Parameter_Specification (N);

         when K_Cast_Statement =>
            Generate_Cast_Statement (N);

         when K_Switch_Statement =>
            Generate_Switch_Statement (N);

         when K_Case_Statement =>
            Generate_Case_Statement (N);

         when K_Throw_Statement =>
            Generate_Throw_Statement (N);

         when K_For_Statement =>
            Generate_For_Statement (N);

         when K_Source_File =>
            Generate_Source_File (N);

         when K_HI_Distributed_Application =>
            Generate_HI_Distributed_Application (N);

         when K_HI_Node =>
            Generate_HI_Node (N);

         when K_HI_Unit =>
            Generate_HI_Unit (N);

         when others =>
            null;
      end case;

   end Generate;

   -----------------------------------
   -- Generate_Variable_Declaration --
   -----------------------------------
   procedure Generate_Variable_Declaration (N : Node_Id) is
      Vis : constant List_Id := Visibility (N);
      P   : Node_Id;
   begin
      --  Visibility
      if not Is_Empty (Vis) then
         P := First_Node (Vis);
         while Present (P) loop
            Generate_Defining_Identifier (P);
            Write_Space;
            P := Next_Node (P);
         end loop;
      end if;

      Generate (Used_Type (N));
      Write_Space;
      Generate (Defining_Identifier (N));

      --  Value
      if Value (N) /= No_Node then
         Write_Space;
         Write (Tok_Equal);
         Write_Space;
         Generate (Value (N));
      end if;
   end Generate_Variable_Declaration;

   -----------------------------------
   -- Generate_Assignment_Statement --
   -----------------------------------
   procedure Generate_Assignment_Statement (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Equal);
      Write_Space;
      Generate (Expression (N));
   end Generate_Assignment_Statement;

   -------------------------------
   -- Generate_Return_Statement --
   -------------------------------
   procedure Generate_Return_Statement (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);
   begin
      Write (Tok_Return);

      if Present (Expr) then
         Write_Space;
         Write (Tok_Left_Paren);
         Generate (Expr);
         Write (Tok_Right_Paren);
      end if;
   end Generate_Return_Statement;

   -----------------------------
   -- Generate_Null_Statement --
   -----------------------------
   procedure Generate_Null_Statement is
   begin
      Write (Tok_Null);
   end Generate_Null_Statement;

   ----------------------------
   -- Generate_Try_Statement --
   ----------------------------
   procedure Generate_Try_Statement (N : Node_Id) is
      D   : constant List_Id := Statements (N);
      C   : constant List_Id := Catch_Statements (N);
      Tmp : Node_Id;
   begin
      Write (Tok_Try);
      Write_Space;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      --  Generate all statements of Try_Statement
      Tmp := First_Node (D);
      while Present (Tmp) loop
         Write_Indentation;
         Generate (Tmp);
         Generate_Statement_Delimiter (Tmp);
         Tmp := Next_Node (Tmp);
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
      Write_Eol;

      --  Find all Catch_Statement
      --  List Catch_Statements can't be empty, there is
      --  at least one Catch_Statement with a Try_Statement
      Tmp := First_Node (C);
      while Present (Tmp) loop
         Write_Indentation;
         Generate (Tmp);
         Tmp := Next_Node (Tmp);
         Write_Eol;
      end loop;

   end Generate_Try_Statement;

   ------------------------------
   -- Generate_Catch_Statement --
   ------------------------------
   procedure Generate_Catch_Statement (N : Node_Id) is
      D   : constant Node_Id := Exception_Caught (N);
      S   : constant List_Id := Statements (N);
      Tmp : Node_Id;
   begin
      Write (Tok_Catch);
      Write_Space;
      Write (Tok_Left_Paren);

      --  Generate exception caught
      Generate (D);

      Write (Tok_Right_Paren);
      Write_Space;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      --  Find all statements
      Tmp := First_Node (S);
      while Present (Tmp) loop
         Write_Indentation;
         Generate (Tmp);
         Tmp := Next_Node (Tmp);
         Write_Eol;
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Catch_Statement;

   --------------------------------
   -- Generate_Package_Statement --
   --------------------------------
   procedure Generate_Package_Statement (N : Node_Id) is
   begin
      Write (Tok_Package);
      Write_Space;
      Generate (Defining_Identifier (N));
   end Generate_Package_Statement;

   ------------------------------
   -- Generate_Class_Statement --
   ------------------------------
   procedure Generate_Class_Statement (N : Node_Id) is
      Vis : Node_Id          := First_Node (Visibility (N));
      Ext : constant Node_Id := Extends_Statement (N);
      Imp : constant List_Id := Implements_Statement (N);
      Thr : constant List_Id := Throws_Statement (N);
      Att : constant List_Id := Attributes (N);
      Met : constant List_Id := Methods (N);
      Cla : constant List_Id := Classes (N);
      P   : Node_Id;
   begin
      --  Visibility
      while Present (Vis) loop
         Generate_Defining_Identifier (Vis);
         Write_Space;
         Vis := Next_Node (Vis);
      end loop;

      Write (Tok_Class);
      Write_Space;
      Generate (Defining_Identifier (N));

      if Ext /= No_Node then
         Write_Space;
         Write (Tok_Extends);
         Write_Space;

         --  Classes extended
         Generate (Ext);
      end if;

      if not Is_Empty (Imp) then
         Write_Space;
         Write (Tok_Implements);
         Write_Space;

         --  Classes implemented
         Write_List (Imp, False);
      end if;

      if not Is_Empty (Thr) then
         Write (Tok_Throws);
         Write_Space;

         --  Exceptions thrown
         Write_List (Thr, False);
      end if;

      Write_Space;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      --  Body of the class : attributes
      if not Is_Empty (Att) then
         Write_Eol;
         P := First_Node (Att);
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
      end if;

      --  Body of the class : classes
      if not Is_Empty (Cla) then
         Write_Eol;
         P := First_Node (Cla);
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
      end if;

      --  Body of the class : methods
      if not Is_Empty (Met) then
         P := First_Node (Met);
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Class_Statement;

   -------------------------
   -- Generate_Visibility --
   -------------------------
   procedure Generate_Visibility (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Public =>
            --  Write (Tok_Public);
            Generate (Defining_Identifier (N));

         when K_Protected =>
            Write (Tok_Protected);

         when K_Private =>
            Write (Tok_Private);

         when K_Abstract =>
            Write (Tok_Abstract);

         when K_Static =>
            Write (Tok_Static);

         when K_Final =>
            Write (Tok_Final);

         when others =>
            null;
      end case;
   end Generate_Visibility;

   ---------------------------
   -- Generate_Java_Comment --
   ---------------------------
   procedure Generate_Java_Comment (N : Node_Id) is

      --  This procedure does the following :

      --  * It generates a Java comment basing on the name of node N

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
            Write_Str (" ");
         end if;

         Used_Columns := Used_Columns + Next_Word_Length;
         Write_Str (Get_Next_Word);

         while Are_There_More_Words
           and then (Used_Columns + Next_Word_Length < Max_Line_Length)
         loop
            Used_Columns := Used_Columns + Next_Word_Length;
            Write_Str (Get_Next_Word);
         end loop;
         Write_Str (" ");
         Write_Str ("*/");

         if Are_There_More_Words then
            Write_Eol;
         end if;
      end loop;
      Write_Eol;
   end Generate_Java_Comment;

   ----------------------------
   -- Generate_New_Statement --
   ----------------------------
   procedure Generate_New_Statement (N : Node_Id) is
      Params    : constant List_Id := Parameters (N);
      New_Array : constant Boolean := Is_Array (N);
      P         : Node_Id;
   begin
      Write (Tok_New);
      Write_Space;
      Generate (Defining_Identifier (N));
      Write_Space;
      if New_Array then
         P := First_Node (Params);
         Write (Tok_Left_Hook);
         Generate (P);
         Write (Tok_Right_Hook);
      else
         Write_List (Params, True);
      end if;
   end Generate_New_Statement;

   ------------------------
   -- Generate_Base_Type --
   ------------------------
   procedure Generate_Base_Type (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Int =>
            Write_Str ("int");

         when K_Short =>
            Write_Str ("short");

         when K_Float =>
            Write_Str ("float");

         when K_Double =>
            Write_Str ("double");

         when K_Char =>
            Write_Str ("char");

         when K_String =>
            Write_Str ("String");

         when K_Boolean =>
            Write_Str ("boolean");

         when K_Byte =>
            Write_Str ("byte");

         when K_Void =>
            Write_Str ("void");

         when others =>
            Display_Error ("Other element in generator", Fatal => False);

      end case;
   end Generate_Base_Type;

   -------------------------
   -- Generate_Enumerator --
   -------------------------
   procedure Generate_Enumerator (N : Node_Id) is
      L   : constant List_Id := Enum_Members (N);
      P   : Node_Id;
      Len : Natural          := 0;
   begin
      if not Is_Empty (L) then
         Len := Length (L);
         P   := First_Node (L);
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            if Len /= 1 then
               Generate_Statement_Delimiter (P);
            end if;
            Len := Len - 1;
            P   := Next_Node (P);
         end loop;
      end if;
   end Generate_Enumerator;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------
   procedure Generate_Defining_Identifier (N : Node_Id) is
   begin
      Write_Name (Name (N));
   end Generate_Defining_Identifier;

   -------------------------
   -- Generate_Expression --
   -------------------------
   procedure Generate_Expression (N : Node_Id) is
      L_Expr : constant Node_Id     := Left_Expression (N);
      Op     : constant Operator_Id := Operator_Expression (N);
      R_Expr : constant Node_Id     := Right_Expression (N);
   begin
      Generate (L_Expr);
      Write_Space;

      if Op /= Operator_Type'Pos (Op_None) then
         Write_Name (Operator_Image (Standard.Integer (Op)));
      end if;

      if R_Expr /= No_Node then
         Write_Space;
         Generate (R_Expr);
      end if;
   end Generate_Expression;

   ----------------------------------
   -- Generate_Statement_Delimiter --
   ----------------------------------
   procedure Generate_Statement_Delimiter (N : Node_Id) is
   begin
      if No (N)
        or else Kind (N) = K_Function_Implementation
        or else Kind (N) = K_Class_Statement
        or else Kind (N) = K_Switch_Statement
      then
         Write_Eol;
      elsif Kind (N) /= K_Java_Comment then
         Write_Line (Tok_Semicolon);
      end if;
   end Generate_Statement_Delimiter;

   ---------------------------------
   --  Generate_Array_Declaration --
   ---------------------------------
   procedure Generate_Array_Declaration (N : Node_Id) is
      P : constant Node_Id := Array_Size (N);
   begin
      Generate (Defining_Identifier (N));
      Write (Tok_Left_Hook);
      if P /= No_Node then
         Generate (P);
      end if;
      Write (Tok_Right_Hook);
   end Generate_Array_Declaration;

   --------------------------
   -- Generate_Array_Value --
   --------------------------
   procedure Generate_Array_Value (N : Node_Id) is
      P : constant Node_Id := Array_Item (N);
   begin
      Generate (Defining_Identifier (N));
      Write (Tok_Left_Hook);
      if P /= No_Node then
         Generate (P);
      end if;
      Write (Tok_Right_Hook);
   end Generate_Array_Value;

   -------------------------------------
   -- Generate_Full_Array_Declaration --
   -------------------------------------
   procedure Generate_Full_Array_Declaration (N : Node_Id) is
      S   : constant Node_Id := Array_Declaration (N);
      L   : constant List_Id := Array_Assignments (N);
      P   : Node_Id;
      Len : Natural          := 0;
   begin
      Generate (S);
      Generate_Statement_Delimiter (S);

      if not Is_Empty (L) then
         Len := Length (L);
         P   := First_Node (L);
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            if Len /= 1 then
               Generate_Statement_Delimiter (P);
            end if;
            Len := Len - 1;
            P   := Next_Node (P);
         end loop;
      end if;
   end Generate_Full_Array_Declaration;

   ----------------------
   -- Generate_Literal --
   ----------------------
   procedure Generate_Literal (N : Node_Id) is
   begin
      Write_Str (Image (Value_Literal (N)));
   end Generate_Literal;

   -------------------------------
   -- Generate_Pointed_Notation --
   -------------------------------
   procedure Generate_Pointed_Notation (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
      Write (Tok_Dot);
      Generate (Right_Member (N));
   end Generate_Pointed_Notation;

   -------------------------------------
   -- Generate_Function_Specification --
   -------------------------------------
   procedure Generate_Function_Specification (N : Node_Id) is
      Vis : constant List_Id := Visibility (N);
      Ret : constant Node_Id := Return_Type (N);
      Par : constant List_Id := Parameters (N);
      Thr : constant List_Id := Throws (N);
      P   : Node_Id;
   begin
      if not Is_Empty (Vis) then
         P := First_Node (Vis);
         --  Visibility
         while Present (P) loop
            Generate_Defining_Identifier (P);
            Write_Space;
            P := Next_Node (P);
         end loop;
      end if;

      if Ret /= No_Node then
         Generate (Ret);
         Write_Space;
      end if;
      Generate (Defining_Identifier (N));
      Write_Space;
      Write_List (Par, True);
      if not Is_Empty (Thr) then
         Write_Space;
         Write (Tok_Throws);
         Write_Space;
         Write_List (Thr, False);
      end if;
   end Generate_Function_Specification;

   --------------------------------------
   -- Generate_Function_Implementation --
   --------------------------------------
   procedure Generate_Function_Implementation (N : Node_Id) is
      Dec : constant List_Id := Declarations (N);
      Spe : constant Node_Id := Specification (N);
      Sta : constant List_Id := Statements (N);
      P   : Node_Id;
   begin
      Generate_Comment_Box (Name (Defining_Identifier (Spe)));
      Write_Indentation;
      Generate (Spe);
      Write_Space;
      Write (Tok_Left_Brace);
      Increment_Indentation;

      if not Is_Empty (Dec) then
         P := First_Node (Dec);
         Write_Eol;
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
      end if;

      Write_Eol;

      if not Is_Empty (Sta) then
         P := First_Node (Sta);
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
      Write_Eol;
   end Generate_Function_Implementation;

   ----------------------------
   -- Generate_Call_Function --
   ----------------------------
   procedure Generate_Call_Function (N : Node_Id) is
      Par : constant List_Id := Parameters (N);
   begin
      Generate (Defining_Identifier (N));
      Write_Space;
      Write_List (Par, True);
   end Generate_Call_Function;

   --------------------------------------
   -- Generate_Parameter_Specification --
   --------------------------------------
   procedure Generate_Parameter_Specification (N : Node_Id) is
   begin
      Generate (Parameter_Type (N));
      Write_Space;
      Generate (Defining_Identifier (N));
   end Generate_Parameter_Specification;

   -----------------------------
   -- Generate_Cast_Statement --
   -----------------------------
   procedure Generate_Cast_Statement (N : Node_Id) is
   begin
      Write (Tok_Left_Paren);
      Generate (Cast_Type (N));
      Write (Tok_Right_Paren);
      Write_Space;
      Generate (Defining_Identifier (N));
   end Generate_Cast_Statement;

   -------------------------------
   -- Generate_Switch_Statement --
   -------------------------------
   procedure Generate_Switch_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Switch);
      Write_Space;
      Write (Tok_Left_Paren);
      Generate (Expr (N));
      Write (Tok_Right_Paren);
      Write_Space;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      P := First_Node (Case_Statements (N));
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

   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------
   procedure Generate_Case_Statement (N : Node_Id) is
      P   : Node_Id;
      Len : Natural := 0;
   begin
      Write (Tok_Case);
      Write_Space;

      P   := First_Node (Labels (N));
      Len := Length (Labels (N));
      while Present (P) loop
         Generate (P);
         if Len > 1 then
            Write_Space;
            Write (Tok_Vertical_Bar);
            Write_Space;
         end if;
         Len := Len - 1;
         P   := Next_Node (P);
      end loop;
      Write (Tok_Colon);
      Write_Space;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      P := First_Node (Statements (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_Case_Statement;

   ------------------------------
   -- Generate_Throw_Statement --
   ------------------------------
   procedure Generate_Throw_Statement (N : Node_Id) is
   begin
      Write (Tok_Throw);
      Write_Space;
      Generate (Defining_Identifier (N));
   end Generate_Throw_Statement;

   ----------------------------
   -- Generate_For_Statement --
   ----------------------------
   procedure Generate_For_Statement (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_For);
      Write_Space;
      Write (Tok_Left_Paren);

      if Init_Statement (N) /= No_Node then
         Generate (Init_Statement (N));
      end if;

      Write (Tok_Semicolon);

      if Iteration_Condition (N) /= No_Node then
         Generate (Iteration_Condition (N));
      end if;

      Write (Tok_Semicolon);

      if Step_Expression (N) /= No_Node then
         Generate (Step_Expression (N));
      end if;

      Write (Tok_Right_Paren);
      Write_Space;
      Write (Tok_Left_Brace);
      Write_Eol;
      Increment_Indentation;

      P := First_Node (Statements (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_Right_Brace);
   end Generate_For_Statement;

   --------------------------
   -- Generate_Source_File --
   --------------------------
   procedure Generate_Source_File (N : Node_Id) is
      Desc : File_Descriptor;
      D    : Node_Id := First_Node (Declarations (N));
      S    : Node_Id := First_Node (Statements (N));
   begin
      if No (N) then
         return;
      end if;

      Desc := Set_Output (To_RTSJ_Conventional_Name (Get_File_Name (N), True));

      while Present (D) loop
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
      end loop;

      if not Is_Empty (Imported_Headers (N)) then
         Generate_Imported_Files (Imported_Headers (N));
      end if;

      while Present (S) loop
         Generate (S);
         Generate_Statement_Delimiter (S);
         S := Next_Node (S);
      end loop;

      Release_Output (Desc);
   end Generate_Source_File;

   -----------------------------
   -- Generate_Imported_Files --
   -----------------------------
   procedure Generate_Imported_Files (N : List_Id) is
      P : Node_Id := First_Node (N);
   begin
      while Present (P) loop
         Write (Tok_Import);
         Write_Space;
         Generate (Import_Name (P));
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;
      Write_Eol;
   end Generate_Imported_Files;

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

   ----------------------
   -- Generate_HI_Unit --
   ----------------------
   procedure Generate_HI_Unit (N : Node_Id) is
      S : Node_Id := First_Node (Sources (N));
   begin
      while Present (S) loop
         Generate (S);
         S := Next_Node (S);
      end loop;
   end Generate_HI_Unit;

   --------------------------
   -- Generate_Comment_Box --
   --------------------------
   procedure Generate_Comment_Box (M : Name_Id) is
   begin
      Get_Name_String (M);
      Write_Eol;
      Write_Indentation;
      Write_Str ("/*");
      for I in 1 .. Name_Len + 4 loop
         Write_Char ('*');
      end loop;
      Write_Str ("*/");
      Write_Eol;
      Write_Indentation;
      Write_Str ("/*  ");
      Write_Name (M);
      Write_Str ("  */");
      Write_Eol;
      Write_Indentation;
      Write_Str ("/*");
      for I in 1 .. Name_Len + 4 loop
         Write_Char ('*');
      end loop;
      Write_Str ("*/");
      Write_Eol;
   end Generate_Comment_Box;

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

   ----------------
   -- Write_List --
   ----------------
   procedure Write_List (L : List_Id; Has_Brackets : Boolean) is
      Tmp : Node_Id;
      Len : Natural := 0;
   begin
      if Has_Brackets then
         Write (Tok_Left_Paren);
      end if;

      if not Is_Empty (L) then
         Len := Length (L);
         Tmp := First_Node (L);
         while Present (Tmp) loop
            Generate (Tmp);

            if Len > 1 then
               Write (Tok_Comma);
               Write_Space;
            end if;

            Tmp := Next_Node (Tmp);
            Len := Len - 1;
         end loop;
      end if;

      if Has_Brackets then
         Write (Tok_Right_Paren);
      end if;
   end Write_List;

   -------------------
   -- Get_File_Name --
   -------------------
   function Get_File_Name (N : Node_Id) return Name_Id is
      Source_Suffix : constant String := ".java";
   begin
      --  The file name corresponding is the lowerd name of N
      Get_Name_String
        (Conventional_Base_Name (Name (Defining_Identifier (N))));

      --  Adding File_Suffix
      Add_Str_To_Name_Buffer (Source_Suffix);

      return Name_Find;
   end Get_File_Name;

end Ocarina.Backends.RTSJ_Tree.Generator;
