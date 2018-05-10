------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . A D A _ T R E E . G E N E R A T O R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.Ada_Tree.Generator is

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Ada_Tree.Nodes;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Ada_Values;

   procedure Generate_Access_Type_Definition (N : Node_Id);
   procedure Generate_Ada_Comment (N : Node_Id);
   procedure Generate_Aspect (N : Node_Id);
   procedure Generate_HI_Distributed_Application (N : Node_Id);
   procedure Generate_HI_Node (N : Node_Id);
   procedure Generate_Unit_Packages (N : Node_Id);
   procedure Generate_Array_Aggregate (N : Node_Id);
   procedure Generate_Array_Type_Definition (N : Node_Id);
   procedure Generate_Assignment_Statement (N : Node_Id);
   procedure Generate_Attribute_Designator (N : Node_Id);
   procedure Generate_Attribute_Definition_Clause (N : Node_Id);
   procedure Generate_Block_Statement (N : Node_Id);
   procedure Generate_Case_Label (N : Node_Id);
   procedure Generate_Case_Statement (N : Node_Id);
   procedure Generate_Component_Association (N : Node_Id);
   procedure Generate_Component_Declaration (N : Node_Id);
   procedure Generate_Decimal_Type_Definition (N : Node_Id);
   procedure Generate_Defining_Identifier (N : Node_Id);
   procedure Generate_Delay_Statement (N : Node_Id);
   procedure Generate_Derived_Type_Definition (N : Node_Id);
   procedure Generate_Designator (N : Node_Id);
   procedure Generate_Element_Association (N : Node_Id);
   procedure Generate_Elsif_Statement (N : Node_Id);
   procedure Generate_Enumeration_Type_Definition (N : Node_Id);
   procedure Generate_Enumeration_Representation_Clause (N : Node_Id);
   procedure Generate_Exception_Declaration (N : Node_Id);
   procedure Generate_Explicit_Dereference (N : Node_Id);
   procedure Generate_Expression (N : Node_Id);
   procedure Generate_For_Statement (N : Node_Id);
   procedure Generate_Full_Type_Declaration (N : Node_Id);
   procedure Generate_If_Statement (N : Node_Id);
   procedure Generate_Indexed_Component (N : Node_Id);
   procedure Generate_Literal (N : Node_Id);
   procedure Generate_Loop_Statement (N : Node_Id);
   procedure Generate_Main_Subprogram_Implementation (N : Node_Id);
   procedure Generate_Null_Statement;
   procedure Generate_Object_Declaration (N : Node_Id);
   procedure Generate_Object_Instantiation (N : Node_Id);
   procedure Generate_Package_Declaration (N : Node_Id);
   procedure Generate_Package_Implementation (N : Node_Id);
   procedure Generate_Package_Instantiation (N : Node_Id);
   procedure Generate_Package_Specification (N : Node_Id);
   procedure Generate_Parameter (N : Node_Id);
   procedure Generate_Parameter_Association (N : Node_Id);
   procedure Generate_Parameter_List (L : List_Id);
   procedure Generate_Pragma_Statement (N : Node_Id);
   procedure Generate_Protected_Object_Spec (N : Node_Id);
   procedure Generate_Protected_Object_Body (N : Node_Id);
   procedure Generate_Qualified_Expression (N : Node_Id);
   procedure Generate_Range_Constraint (N : Node_Id);
   procedure Generate_Raise_Statement (N : Node_Id);
   procedure Generate_Record_Aggregate (N : Node_Id);
   procedure Generate_Record_Definition (N : Node_Id);
   procedure Generate_Record_Type_Definition (N : Node_Id);
   procedure Generate_Private_Type_Definition (N : Node_Id);
   procedure Generate_Return_Statement (N : Node_Id);
   procedure Generate_Selected_Component (N : Node_Id);
   procedure Generate_Subprogram_Call (N : Node_Id);
   procedure Generate_Subprogram_Implementation (N : Node_Id);
   procedure Generate_Subprogram_Specification (N : Node_Id);
   procedure Generate_Used_Type (N : Node_Id);
   procedure Generate_Used_Package (N : Node_Id);
   procedure Generate_Type_Conversion (N : Node_Id);
   procedure Generate_Variant_Part (N : Node_Id);
   procedure Generate_Withed_Package (N : Node_Id);
   procedure Generate_Exit_When_Statement (N : Node_Id);

   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   procedure Generate_Statement_Delimiter (N : Node_Id);
   procedure Generate_Comment_Box (M : Name_Id);

   type Pragma_W is (W_On, W_Off);
   procedure Generate_Pragma_Warnings (W : Pragma_W);
   --  Generate pragma Warnings (Off|On);

   --  The entities declared below are relared to the package
   --  generation in different files

   function Get_File_Name (N : Node_Id) return Name_Id;
   --  Generate an Ada file name from the package node given as
   --  parameter

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (N : Node_Id) return Name_Id is
      pragma Assert
        (Kind (N) = K_Package_Specification
         or else Kind (N) = K_Package_Implementation
         or else Kind (N) = K_Subprogram_Specification
         or else Kind (N) = K_Subprogram_Implementation);

      Package_Spec_Suffix : constant String := ".ads";
      Package_Body_Suffix : constant String := ".adb";
   begin
      --  The File name corresponding to a package is the lowerd fully
      --  qualified name of the package. All '.' separators are
      --  replaced by '-'.

      if Kind (N) = K_Subprogram_Implementation
        or else Kind (N) = K_Subprogram_Specification
      then
         --  If the user supplied a custom file name, we use it

         if Has_Custom_File_Name (Main_Subprogram_Unit (N)) then
            Get_Name_String (File_Name (Main_Subprogram_Unit (N)));
         else
            Get_Name_String
              (Conventional_Base_Name
                 (Fully_Qualified_Name
                    (Defining_Identifier (Main_Subprogram_Unit (N)))));
         end if;
      else
         if Has_Custom_File_Name (Package_Declaration (N)) then
            Get_Name_String (File_Name (Package_Declaration (N)));
         else
            Get_Name_String
              (Conventional_Base_Name
                 (Fully_Qualified_Name
                    (Defining_Identifier (Package_Declaration (N)))));
         end if;
      end if;

      --  Adding file suffix

      if Kind (N) = K_Package_Specification
        or else Kind (N) = K_Subprogram_Specification
      then
         Add_Str_To_Name_Buffer (Package_Spec_Suffix);
      else
         Add_Str_To_Name_Buffer (Package_Body_Suffix);
      end if;

      return Name_Find;
   end Get_File_Name;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Private_Type_Definition =>
            Generate_Private_Type_Definition (N);

         when K_Access_Type_Definition =>
            Generate_Access_Type_Definition (N);

         when K_Ada_Comment =>
            Generate_Ada_Comment (N);

         when K_HI_Distributed_Application =>
            Generate_HI_Distributed_Application (N);

         when K_HI_Node =>
            Generate_HI_Node (N);

         when K_HI_Unit =>
            Generate_Unit_Packages (N);

         when K_Array_Aggregate =>
            Generate_Array_Aggregate (N);

         when K_Array_Type_Definition =>
            Generate_Array_Type_Definition (N);

         when K_Assignment_Statement =>
            Generate_Assignment_Statement (N);

         when K_Attribute_Definition_Clause =>
            Generate_Attribute_Definition_Clause (N);

         when K_Attribute_Designator =>
            Generate_Attribute_Designator (N);

         when K_Block_Statement =>
            Generate_Block_Statement (N);

         when K_Case_Label =>
            Generate_Case_Label (N);

         when K_Case_Statement =>
            Generate_Case_Statement (N);

         when K_Component_Association =>
            Generate_Component_Association (N);

         when K_Component_Declaration =>
            Generate_Component_Declaration (N);

         when K_Decimal_Type_Definition =>
            Generate_Decimal_Type_Definition (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Delay_Statement =>
            Generate_Delay_Statement (N);

         when K_Derived_Type_Definition =>
            Generate_Derived_Type_Definition (N);

         when K_Designator =>
            Generate_Designator (N);

         when K_Element_Association =>
            Generate_Element_Association (N);

         when K_Elsif_Statement =>
            Generate_Elsif_Statement (N);

         when K_Enumeration_Type_Definition =>
            Generate_Enumeration_Type_Definition (N);

         when K_Enumeration_Representation_Clause =>
            Generate_Enumeration_Representation_Clause (N);

         when K_Exception_Declaration =>
            Generate_Exception_Declaration (N);

         when K_Explicit_Dereference =>
            Generate_Explicit_Dereference (N);

         when K_Expression =>
            Generate_Expression (N);

         when K_For_Statement =>
            Generate_For_Statement (N);

         when K_Full_Type_Declaration =>
            Generate_Full_Type_Declaration (N);

         when K_If_Statement =>
            Generate_If_Statement (N);

         when K_Indexed_Component =>
            Generate_Indexed_Component (N);

         when K_Literal =>
            Generate_Literal (N);

         when K_Loop_Statement =>
            Generate_Loop_Statement (N);

         when K_Main_Subprogram_Implementation =>
            Generate_Main_Subprogram_Implementation (N);

         when K_Null_Statement =>
            Generate_Null_Statement;

         when K_Object_Declaration =>
            Generate_Object_Declaration (N);

         when K_Object_Instantiation =>
            Generate_Object_Instantiation (N);

         when K_Package_Declaration =>
            Generate_Package_Declaration (N);

         when K_Package_Implementation =>
            Generate_Package_Implementation (N);

         when K_Package_Instantiation =>
            Generate_Package_Instantiation (N);

         when K_Package_Specification =>
            Generate_Package_Specification (N);

         when K_Parameter_Association =>
            Generate_Parameter_Association (N);

         when K_Pragma_Statement =>
            Generate_Pragma_Statement (N);

         when K_Protected_Object_Spec =>
            Generate_Protected_Object_Spec (N);

         when K_Protected_Object_Body =>
            Generate_Protected_Object_Body (N);

         when K_Qualified_Expression =>
            Generate_Qualified_Expression (N);

         when K_Range_Constraint =>
            Generate_Range_Constraint (N);

         when K_Raise_Statement =>
            Generate_Raise_Statement (N);

         when K_Record_Aggregate =>
            Generate_Record_Aggregate (N);

         when K_Record_Definition =>
            Generate_Record_Definition (N);

         when K_Record_Type_Definition =>
            Generate_Record_Type_Definition (N);

         when K_Return_Statement =>
            Generate_Return_Statement (N);

         when K_Selected_Component =>
            Generate_Selected_Component (N);

         when K_Subprogram_Call =>
            Generate_Subprogram_Call (N);

         when K_Subprogram_Specification =>
            Generate_Subprogram_Specification (N);

         when K_Subprogram_Implementation =>
            Generate_Subprogram_Implementation (N);

         when K_Type_Conversion =>
            Generate_Type_Conversion (N);

         when K_Used_Type =>
            Generate_Used_Type (N);

         when K_Used_Package =>
            Generate_Used_Package (N);

         when K_Variant_Part =>
            Generate_Variant_Part (N);

         when K_Exit_When_Statement =>
            Generate_Exit_When_Statement (N);

         when K_Withed_Package =>
            Generate_Withed_Package (N);

         when K_Boolean .. K_String =>
            Write_Name (Image (Base_Type (N)));

         when others =>
            null;
      end case;
   end Generate;

   --------------------------------------
   -- Generate_Private_Type_Definition --
   --------------------------------------

   procedure Generate_Private_Type_Definition (N : Node_Id) is
      pragma Unreferenced (N);
   begin
      Write (Tok_Private);
   end Generate_Private_Type_Definition;

   -------------------------------------
   -- Generate_Access_Type_Definition --
   -------------------------------------

   procedure Generate_Access_Type_Definition (N : Node_Id) is
   begin
      if Is_Not_Null (N) then
         Write (Tok_Not);
         Write_Space;
         Write (Tok_Null);
         Write_Space;
      end if;

      Write (Tok_Access);
      Write_Space;

      if Is_All (N) then
         Write (Tok_All);
         Write_Space;
      end if;

      if Is_Constant (N) then
         Write (Tok_Constant);
         Write_Space;
      end if;

      Generate (Subtype_Indication (N));
   end Generate_Access_Type_Definition;

   --------------------------
   -- Generate_Ada_Comment --
   --------------------------

   procedure Generate_Ada_Comment (N : Node_Id) is
      --  This procedure does the following:

      --  * It generates an Ada comment basing on the name of node N

      --  * If the name it too long, and depending on the location of
      --    the comment in the source code, the procedure splits the
      --    comment into more than a line.

      --  The comment is assumed to be a sequence of caracters,
      --  beginning and ending with a NON-SPACE caracter.

      --  A word is:

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

      procedure Skip_Next_Word;
      --  Skips the next word

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

      --------------------
      -- Skip_Next_Word --
      --------------------

      procedure Skip_Next_Word is
      begin
         if Name_Len = Next_Word_Length then
            Name_Len := 0;
         elsif Next_Word_Length > 0 then
            Set_Str_To_Name_Buffer
              (Name_Buffer (Next_Word_Length + 1 .. Name_Len));
         end if;
      end Skip_Next_Word;

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
         Write_Str ("--");

         if Has_Header_Spaces (N) then
            Used_Columns := Used_Columns + 2;
            Write_Str ("  ");
         end if;

         --  If the first word of the line, would be a space, skip it

         if Next_Word_Length = 1 and then Name_Buffer (1) = ' ' then
            Skip_Next_Word;
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
   end Generate_Ada_Comment;

   ----------------------------
   -- Generate_Unit_Packages --
   ----------------------------

   procedure Generate_Unit_Packages (N : Node_Id) is
      P : Node_Id := First_Node (Packages (N));
   begin
      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;
   end Generate_Unit_Packages;

   ------------------------------
   -- Generate_Array_Aggregate --
   ------------------------------

   procedure Generate_Array_Aggregate (N : Node_Id) is
      E : Node_Id;
   begin
      Write (Tok_Left_Paren);

      E := First_Node (Elements (N));
      loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write (Tok_Comma);
         Write_Eol;
         Write_Indentation;
      end loop;

      Write (Tok_Right_Paren);
   end Generate_Array_Aggregate;

   ------------------------------------
   -- Generate_Array_Type_Definition --
   ------------------------------------

   procedure Generate_Array_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      Write (Tok_Array);
      Write_Space;
      Write (Tok_Left_Paren);
      R := First_Node (Range_Constraints (N));
      loop
         Generate (R);
         R := Next_Node (R);
         exit when No (R);
         Write (Tok_Comma);
         Write_Space;
      end loop;
      Write (Tok_Right_Paren);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_Of);
      Write_Space;

      if Aliased_Present (N) then
         Write (Tok_Aliased);
         Write_Space;
      end if;

      Generate (Component_Definition (N));
      Decrement_Indentation;
   end Generate_Array_Type_Definition;

   -----------------------------------
   -- Generate_Assignment_Statement --
   -----------------------------------

   procedure Generate_Assignment_Statement (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Colon_Equal);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Expression (N));
      Decrement_Indentation;
   end Generate_Assignment_Statement;

   ------------------------------------------
   -- Generate_Attribute_Definition_Clause --
   ------------------------------------------

   procedure Generate_Attribute_Definition_Clause (N : Node_Id) is
   begin
      Write (Tok_For);
      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));
      Write (Tok_Apostrophe);
      Write_Name (Attribute_Designator (N));
      Write_Space;
      Write (Tok_Use);
      Write_Space;
      Generate (Expression (N));
   end Generate_Attribute_Definition_Clause;

   -----------------------------------
   -- Generate_Attribute_Designator --
   -----------------------------------

   procedure Generate_Attribute_Designator (N : Node_Id) is
   begin
      Generate (Prefix (N));
      Write (Tok_Apostrophe);
      Write_Name (Name (N));
   end Generate_Attribute_Designator;

   ------------------------------
   -- Generate_Block_Statement --
   ------------------------------

   procedure Generate_Block_Statement (N : Node_Id) is
      D : Node_Id;
   begin
      if Present (Defining_Identifier (N)) then
         Write_Eol;
         Decrement_Indentation;
         Write_Indentation (-1);
         Increment_Indentation;
         Generate (Defining_Identifier (N));
         Write_Line (Tok_Colon);
         Write_Indentation;
      end if;

      if not Is_Empty (Declarative_Part (N)) then
         Write (Tok_Declare);
         Write_Eol;
         Increment_Indentation;
         D := First_Node (Declarative_Part (N));
         loop
            Write_Indentation;
            Generate (D);
            Generate_Statement_Delimiter (D);
            D := Next_Node (D);
            exit when No (D);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write (Tok_Begin);
      Write_Eol;
      Increment_Indentation;
      D := First_Node (Statements (N));
      loop
         Write_Indentation;
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
         exit when No (D);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      if not Is_Empty (Exception_Handler (N)) then
         declare
            Excp_Handler_Alternative : Node_Id;
         begin
            Write (Tok_Exception);
            Write_Eol;
            Increment_Indentation;

            --  Generation of the exception handler

            Write_Indentation;
            Excp_Handler_Alternative := First_Node (Exception_Handler (N));
            while Present (Excp_Handler_Alternative) loop
               Write (Tok_When);
               Write_Space;

               --  Generate the different part of the component
               --  association but add a new line after "=>"

               Generate (Defining_Identifier (Excp_Handler_Alternative));
               Write_Space;
               Write (Tok_Arrow);
               Write_Eol;
               Increment_Indentation;
               Write_Indentation;
               Generate (Expression (Excp_Handler_Alternative));
               Generate_Statement_Delimiter
                 (Expression (Excp_Handler_Alternative));
               Decrement_Indentation;

               Excp_Handler_Alternative :=
                 Next_Node (Excp_Handler_Alternative);
            end loop;
            Decrement_Indentation;
            Write_Indentation;
         end;
      end if;
      Write (Tok_End);
   end Generate_Block_Statement;

   -------------------------
   -- Generate_Case_Label --
   -------------------------

   procedure Generate_Case_Label (N : Node_Id) is
   begin
      Write_Str (Image (Value (N)));
   end Generate_Case_Label;

   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------

   procedure Generate_Case_Statement (N : Node_Id) is
      D : Node_Id;
      M : Node_Id;
   begin
      Write (Tok_Case);
      Write_Space;
      Generate (Expression (N));
      Write_Space;
      Write_Line (Tok_Is);
      D := First_Node (Case_Statement_Alternatives (N));
      Increment_Indentation;

      while Present (D) loop
         if Is_Empty (Discret_Choice_List (D)) then
            Write_Indentation;
            Generate_Pragma_Warnings (W_Off);
            Write_Line (Tok_Semicolon);
         end if;

         Write_Indentation;
         Write (Tok_When);
         Write_Space;

         if not Is_Empty (Discret_Choice_List (D)) then
            M := First_Node (Discret_Choice_List (D));
            loop
               Generate (M);
               M := Next_Node (M);
               exit when No (M);
               Write_Space;
               Write (Tok_Vertical_Bar);
               Write_Space;
            end loop;
            Write_Space;
            Write_Line (Tok_Arrow);
         else
            Write (Tok_Others);
            Write_Space;
            Write_Line (Tok_Arrow);
         end if;

         Increment_Indentation;

         if Is_Empty (Statements (D)) then
            Write_Indentation;
            Write (Tok_Null);
            Write_Line (Tok_Semicolon);
         else
            M := First_Node (Statements (D));
            while Present (M) loop
               Write_Indentation;
               Generate (M);
               Generate_Statement_Delimiter (M);
               M := Next_Node (M);
            end loop;
         end if;

         Decrement_Indentation;

         if Is_Empty (Discret_Choice_List (D)) then
            Write_Indentation;
            Generate_Pragma_Warnings (W_On);
            Write_Line (Tok_Semicolon);
         end if;

         Write_Eol;

         D := Next_Node (D);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_Case);
   end Generate_Case_Statement;

   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------

   procedure Generate_Component_Association (N : Node_Id) is
   begin
      --  If the developer gives a defining identifier, we generate
      --  it, else we assume that the developer wants to generate a
      --  "others => ..." statement.

      if Present (Defining_Identifier (N)) then
         Generate (Defining_Identifier (N));
      else
         Write (Tok_Others);
      end if;

      Write_Space;
      Write (Tok_Arrow);
      Write_Space;
      Generate (Expression (N));
   end Generate_Component_Association;

   ------------------------------------
   -- Generate_Component_Declaration --
   ------------------------------------

   procedure Generate_Component_Declaration (N : Node_Id) is
      E : constant Node_Id := Expression (N);

   begin
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Colon);
      Write_Space;

      if Aliased_Present (N) then
         Write (Tok_Aliased);
         Write_Space;
      end if;

      Generate (Subtype_Indication (N));

      if Present (E) then
         Write_Space;
         Write (Tok_Colon_Equal);
         Write_Space;
         Generate (E);
      end if;
   end Generate_Component_Declaration;

   --------------------------------------
   -- Generate_Decimal_Type_Definition --
   --------------------------------------

   procedure Generate_Decimal_Type_Definition (N : Node_Id) is
   begin
      Write (Tok_Delta);
      Write_Space;

      Generate (Scale (N));
      Write_Space;

      Write (Tok_Digits);
      Write_Space;

      Write_Str (Image (Total (N)));

   end Generate_Decimal_Type_Definition;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (N : Node_Id) is
      P : Node_Id;

   begin
      P := Parent_Unit_Name (N);

      if Present (P) then
         Generate (P);
         Write (Tok_Dot);
      end if;

      Write_Name (Name (N));
   end Generate_Defining_Identifier;

   ------------------------------
   -- Generate_Delay_Statement --
   ------------------------------

   procedure Generate_Delay_Statement (N : Node_Id) is
   begin
      Write (Tok_Delay);
      Write_Space;

      if Is_Until (N) then
         Write (Tok_Until);
         Write_Space;
      end if;

      Generate (Expression (N));
   end Generate_Delay_Statement;

   --------------------------------------
   -- Generate_Derived_Type_Definition --
   --------------------------------------

   procedure Generate_Derived_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write (Tok_Abstract);
         Write_Space;
      end if;

      if not Is_Subtype (N) then
         Write (Tok_New);
         Write_Space;
      end if;
      Generate (Subtype_Indication (N));

      if Is_Private_Extention (N) then
         Write_Space;
         Write (Tok_With);
         Write_Space;
         Write (Tok_Private);
      else
         R := Record_Extension_Part (N);

         if Present (R) then
            Write_Space;
            Write (Tok_With);
            Write_Space;
            Generate (Record_Extension_Part (N));
         end if;
      end if;
   end Generate_Derived_Type_Definition;

   -------------------------
   -- Generate_Designator --
   -------------------------

   procedure Generate_Designator (N : Node_Id) is
      P : Node_Id;

   begin
      P := Parent_Unit_Name (N);

      if Present (P) then
         Generate (P);
         Write (Tok_Dot);
      end if;

      Write_Name (Name (Defining_Identifier (N)));

      if Is_All (N) then
         Write (Tok_Dot);
         Write (Tok_All);
      end if;
   end Generate_Designator;

   ----------------------------------
   -- Generate_Element_Association --
   ----------------------------------

   procedure Generate_Element_Association (N : Node_Id) is
   begin
      if Present (Index (N)) then
         Generate (Index (N));
      else
         Write (Tok_Others);
      end if;

      Write_Space;
      Write (Tok_Arrow);
      Write_Eol;

      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Expression (N));
      Decrement_Indentation;
   end Generate_Element_Association;

   ------------------------------
   -- Generate_Elsif_Statement --
   ------------------------------

   procedure Generate_Elsif_Statement (N : Node_Id) is
      D : Node_Id;
   begin
      if No (First_Node (Then_Statements (N))) then
         return;
      end if;

      Write (Tok_Elsif);
      Write_Space;
      Generate (Condition (N));
      Write_Eol;
      Write_Indentation;
      Write_Line (Tok_Then);
      Increment_Indentation;
      D := First_Node (Then_Statements (N));
      loop
         Write_Indentation;
         Generate (D);
         exit when No (Next_Node (D));
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
      end loop;
      Decrement_Indentation;
   end Generate_Elsif_Statement;

   ------------------------------------------
   -- Generate_Enumeration_Type_Definition --
   ------------------------------------------

   procedure Generate_Enumeration_Type_Definition (N : Node_Id) is
      E : Node_Id;

   begin
      Write (Tok_Left_Paren);
      E := First_Node (Enumeration_Literals (N));

      while Present (E) loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write_Line (Tok_Comma);
         Write_Indentation;
      end loop;

      Write (Tok_Right_Paren);
   end Generate_Enumeration_Type_Definition;

   ------------------------------------------------
   -- Generate_Enumeration_Representation_Clause --
   ------------------------------------------------

   procedure Generate_Enumeration_Representation_Clause (N : Node_Id) is
   begin
      Write (Tok_For);
      Write_Space;
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Use);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Array_Aggregate (N));
      Decrement_Indentation;
   end Generate_Enumeration_Representation_Clause;

   ------------------------------------
   -- Generate_Exception_Declaration --
   ------------------------------------

   procedure Generate_Exception_Declaration (N : Node_Id) is
   begin
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;
      Write (Tok_Colon);
      Write_Space;
      Write (Tok_Exception);
      if Present (Renamed_Entity (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Renames);
         Write_Space;
         Generate (Renamed_Entity (N));
         Decrement_Indentation;
      end if;
   end Generate_Exception_Declaration;

   -----------------------------------
   -- Generate_Explicit_Dereference --
   -----------------------------------

   procedure Generate_Explicit_Dereference (N : Node_Id) is
   begin
      Generate (Prefix (N));
      Write (Tok_Dot);
      Write (Tok_All);
   end Generate_Explicit_Dereference;

   -------------------------
   -- Generate_Expression --
   -------------------------

   procedure Generate_Expression (N : Node_Id) is
      L_Expr : constant Node_Id     := Left_Expr (N);
      Op     : constant Operator_Id := Operator (N);
      R_Expr : constant Node_Id     := Right_Expr (N);
   begin
      --  Each expression having a right part and a left part is
      --  systematically put between two parentheses.

      if No (R_Expr) then
         if Op = Operator_Type'Pos (Op_Not) then
            Write (Tok_Not);
            Write_Space;
         elsif Op /= Operator_Type'Pos (Op_None) then
            Write_Name (Operator_Image (Standard.Integer (Op)));

            --  Do not generate space after a unary operator
         end if;
      else
         --  Expressions having "|" as operator (case switches
         --  alternatives) and expressions having "&" as operator
         --  (array concatenation) do not require parentheses.

         if Op /= Operator_Type'Pos (Op_Vertical_Bar)
           and then Op /= Operator_Type'Pos (Op_And_Symbol)
         then
            Write (Tok_Left_Paren);
         end if;
      end if;

      Generate (L_Expr);

      if Present (R_Expr) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;

         Write_Name (Operator_Image (Standard.Integer (Op)));
         Write_Space;
         Generate (R_Expr);

         if Op /= Operator_Type'Pos (Op_Vertical_Bar)
           and then Op /= Operator_Type'Pos (Op_And_Symbol)
         then
            Write (Tok_Right_Paren);
         end if;

         Decrement_Indentation;
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
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;
      Write (Tok_In);
      Write_Space;
      Generate (First (Range_Constraint (N)));
      Write_Space;
      Write (Tok_Dot);
      Write (Tok_Dot);
      Write_Space;
      Generate (Last (Range_Constraint (N)));
      Write_Space;
      Write (Tok_Loop);
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
      Write (Tok_End);
      Write_Space;
      Write (Tok_Loop);
   end Generate_For_Statement;

   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------

   procedure Generate_Full_Type_Declaration (N : Node_Id) is
      D : constant Node_Id := Discriminant_Spec (N);

   begin
      if Is_Subtype (N) then
         Write (Tok_Subtype);
      else
         Write (Tok_Type);
      end if;
      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;

      if Present (D) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         Generate (D);
         Write (Tok_Right_Paren);
         Decrement_Indentation;
         Write_Eol;
         Write_Indentation;
      end if;

      if Type_Definition (N) /= No_Node then
         Write (Tok_Is);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Generate (Type_Definition (N));
         Decrement_Indentation;
      else
         Write_Eol;
      end if;
   end Generate_Full_Type_Declaration;

   ---------------------------
   -- Generate_If_Statement --
   ---------------------------

   procedure Generate_If_Statement (N : Node_Id) is
      T : constant List_Id := Then_Statements (N);
      E : constant List_Id := Else_Statements (N);
      I : Node_Id;

   begin
      --  Enter If_Statement

      Write (Tok_If);
      Write_Space;
      Generate (Condition (N));
      Write_Eol;
      Write_Indentation;
      Write (Tok_Then);
      Write_Eol;

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
      Decrement_Indentation;

      --  Elsif_Statements

      if not Is_Empty (Elsif_Statements (N)) then
         I := First_Node (Elsif_Statements (N));
         loop
            Write_Indentation;
            Generate (I);
            Generate_Statement_Delimiter (I);
            I := Next_Node (I);
            exit when No (I);
         end loop;
      end if;

      --  Else_Statement can be empty

      if not Is_Empty (E) then
         Write_Indentation;
         Write (Tok_Else);
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
      end if;

      --  Leave If_Statement

      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_If);
   end Generate_If_Statement;

   ----------------------------------
   -- Generate_Exit_When_Statement --
   ----------------------------------

   procedure Generate_Exit_When_Statement (N : Node_Id) is
   begin
      Write (Tok_Exit);
      Write_Space;
      Write (Tok_When);
      Write_Space;

      --  print the condition condition

      Generate (Condition (N));
   end Generate_Exit_When_Statement;

   --------------------------------
   -- Generate_Indexed_Component --
   --------------------------------

   procedure Generate_Indexed_Component (N : Node_Id) is
      Exp : constant List_Id := Expressions (N);
      E   : Node_Id;
   begin
      Generate (Prefix (N));

      pragma Assert (not Is_Empty (Exp));

      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_Left_Paren);
      E := First_Node (Exp);

      loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write_Line (Tok_Comma);
         Write_Indentation;
      end loop;

      Write (Tok_Right_Paren);
      Decrement_Indentation;
   end Generate_Indexed_Component;

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (N : Node_Id) is
   begin
      if Present (Parent_Designator (N)) then
         Generate (Parent_Designator (N));
         Write (Tok_Dot);
      end if;
      Write_Str (Image (Value (N)));
   end Generate_Literal;

   -----------------------------
   -- Generate_Loop_Statement --
   -----------------------------

   procedure Generate_Loop_Statement (N : Node_Id) is
      D : Node_Id := First_Node (Statements (N));
   begin
      Write (Tok_Loop);
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
      Write (Tok_End);
      Write_Space;
      Write (Tok_Loop);
   end Generate_Loop_Statement;

   ---------------------------------------------
   -- Generate_Main_Subprogram_Implementation --
   ---------------------------------------------

   procedure Generate_Main_Subprogram_Implementation (N : Node_Id) is
      Fd : File_Descriptor;
   begin
      if Present (Subprogram_Specification (N)) then
         Fd := Set_Output (Get_File_Name (Subprogram_Specification (N)));
         Generate (Subprogram_Specification (N));
         Generate_Statement_Delimiter (Subprogram_Specification (N));
         Release_Output (Fd);
      end if;

      if Present (Subprogram_Implementation (N)) then
         Fd := Set_Output (Get_File_Name (Subprogram_Implementation (N)));
         Generate (Subprogram_Implementation (N));
         Generate_Statement_Delimiter (Subprogram_Implementation (N));
         Release_Output (Fd);
      end if;
   end Generate_Main_Subprogram_Implementation;

   -----------------------------
   -- Generate_Null_Statement --
   -----------------------------

   procedure Generate_Null_Statement is
   begin
      Write (Tok_Null);
   end Generate_Null_Statement;

   ---------------------------------
   -- Generate_Object_Declaration --
   ---------------------------------

   procedure Generate_Object_Declaration (N : Node_Id) is
   begin
      Name_Buffer (1 .. Var_Name_Len) := (others => ' ');
      Get_Name_String (Name (Defining_Identifier (N)));

      if Var_Name_Len > Name_Len then
         Name_Len := Var_Name_Len;
      end if;

      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Space;
      Write (Tok_Colon);

      if Constant_Present (N) then
         Write_Space;
         Write (Tok_Constant);
      end if;

      if Aliased_Present (N) then
         Write_Space;
         Write (Tok_Aliased);
      end if;

      Write_Space;
      if Present (Object_Definition (N)) then
         Generate (Object_Definition (N));
      else
         --  This workaround doesn't affect the classic object
         --  declaration because we must give a type. However it makes
         --  the generation of case statement and exception handlers
         --  simpler.

         Write (Tok_Others);
      end if;

      if Present (Discriminant_Spec (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Generate (Discriminant_Spec (N));
         Decrement_Indentation;
      end if;

      if Present (Renamed_Entity (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Renames);
         Write_Space;
         Generate (Renamed_Entity (N));
         Decrement_Indentation;

      --  If an object renames another object, it cannot be
      --  initialized,
      else
         if Present (Expression (N)) then
            Write_Space;
            Write (Tok_Colon_Equal);
            Write_Eol;
            Increment_Indentation;
            Write_Indentation (-1);
            Generate (Expression (N));
            Decrement_Indentation;
         end if;
      end if;
   end Generate_Object_Declaration;

   -----------------------------------
   -- Generate_Object_Instantiation --
   -----------------------------------

   procedure Generate_Object_Instantiation (N : Node_Id) is
   begin
      Write (Tok_New);
      Write_Space;
      Generate (Qualified_Expression (N));
   end Generate_Object_Instantiation;

   ----------------------------------
   -- Generate_Package_Declaration --
   ----------------------------------

   procedure Generate_Package_Declaration (N : Node_Id) is
   begin
      Generate (Package_Specification (N));
      Generate (Package_Implementation (N));
   end Generate_Package_Declaration;

   -------------------------------------
   -- Generate_Package_Implementation --
   -------------------------------------

   procedure Generate_Package_Implementation (N : Node_Id) is
      P  : Node_Id;
      Fd : File_Descriptor;
   begin
      --  If the user wants to generates only the spec, or if the
      --  package body is empty, we don't generate it.

      if Disable_Pkg_Body_Gen or else Is_Empty (Statements (N)) then
         return;
      end if;

      Fd := Set_Output (Get_File_Name (N));

      --  generate Package Headers : comment headers and pragma

      P := First_Node (Package_Headers (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;
      Write_Eol;

      P := First_Node (Withed_Packages (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;
      Write_Eol;

      Write_Indentation;
      Write (Tok_Package);
      Write_Space;
      Write (Tok_Body);
      Write_Space;
      Generate (Defining_Identifier (Package_Declaration (N)));
      Write_Space;

      Generate_Aspect (Aspect_Specification (N));

      Write (Tok_Is);
      Write_Eol (2);

      Increment_Indentation;
      P := First_Node (Statements (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         Write_Eol;
         P := Next_Node (P);
      end loop;
      Decrement_Indentation;
      Write_Indentation;

      if not Is_Empty (Package_Initialization (N)) then
         Write_Line (Tok_Begin);
         Increment_Indentation;
         P := First_Node (Package_Initialization (N));
         loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
            exit when No (P);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;

      Write (Tok_End);
      Write_Space;
      Generate (Defining_Identifier (Package_Declaration (N)));
      Generate_Statement_Delimiter
        (Defining_Identifier (Package_Declaration (N)));

      Release_Output (Fd);
   end Generate_Package_Implementation;

   ------------------------------------
   -- Generate_Package_Instantiation --
   ------------------------------------

   procedure Generate_Package_Instantiation (N : Node_Id) is
      Param : Node_Id;
   begin
      Write (Tok_Package);
      Write_Space;
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Is);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_New);
      Write_Space;
      Generate (Generic_Package (N));
      if not Is_Empty (Parameter_List (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         Param := First_Node (Parameter_List (N));
         loop
            Generate (Param);
            Param := Next_Node (Param);
            exit when No (Param);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;
         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;
      Decrement_Indentation;
   end Generate_Package_Instantiation;

   ------------------------------------
   -- Generate_Package_Specification --
   ------------------------------------

   procedure Generate_Package_Specification (N : Node_Id) is
      P  : Node_Id;
      Fd : File_Descriptor;
   begin
      --  If the user wants to generates only the body, or if the
      --  package spec is empty, we don't generate it.

      if Disable_Pkg_Spec_Gen then
         return;
      end if;

      --  Do not generate empty non instanciated specs

      if not Is_Instantiated_Package (N)
        and then Is_Empty (Visible_Part (N))
        and then Is_Empty (Private_Part (N))
      then
         return;
      end if;

      Fd := Set_Output (Get_File_Name (N));

      --  generate Package Headers : comment headers and pragma

      P := First_Node (Package_Headers (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;
      Write_Eol;

      P := First_Node (Withed_Packages (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;
      Write_Eol;

      if Is_Instantiated_Package (N) then
         Generate (Package_Instantiation (N));
         Generate_Statement_Delimiter (Package_Instantiation (N));
      else
         Write_Indentation;
         Write (Tok_Package);
         Write_Space;
         Generate (Defining_Identifier (Package_Declaration (N)));

         Generate_Aspect (Aspect_Specification (N));
         Write_Space;
         Write (Tok_Is);
         Write_Eol (2);

         Increment_Indentation;
         P := First_Node (Visible_Part (N));
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            Write_Eol;
            P := Next_Node (P);
         end loop;
         Decrement_Indentation;

         if not Is_Empty (Private_Part (N)) then
            Write_Indentation;
            Write (Tok_Private);
            Write_Eol;
            Increment_Indentation;
            P := First_Node (Private_Part (N));
            while Present (P) loop
               Write_Indentation;
               Generate (P);
               Generate_Statement_Delimiter (P);
               Write_Eol;
               P := Next_Node (P);
            end loop;
            Decrement_Indentation;
         end if;

         Write_Indentation;
         Write (Tok_End);
         Write_Space;
         Generate (Defining_Identifier (Package_Declaration (N)));
         Generate_Statement_Delimiter
           (Defining_Identifier (Package_Declaration (N)));
      end if;

      Release_Output (Fd);
   end Generate_Package_Specification;

   ------------------------
   -- Generate_Parameter --
   ------------------------

   procedure Generate_Parameter (N : Node_Id) is
   begin
      Name_Buffer (1 .. Var_Name_Len) := (others => ' ');
      Get_Name_String (Name (Defining_Identifier (N)));

      if Var_Name_Len > Name_Len then
         Name_Len := Var_Name_Len;
      end if;

      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Space;
      Write (Tok_Colon);
      Write_Space;

      if Kind (Parameter_Type (N)) /= K_Access_Type_Definition then
         case Parameter_Mode (N) is
            when Mode_In =>
               null;

            when Mode_Out =>
               Write (Tok_Out);
               Write_Space;

            when Mode_Inout =>
               Write (Tok_In);
               Write_Space;
               Write (Tok_Out);
               Write_Space;
         end case;
      end if;

      Generate (Parameter_Type (N));

      if Present (Expression (N)) then
         Write_Space;
         Write_Line (Tok_Colon_Equal);
         Increment_Indentation;
         Write_Indentation;
         Generate (Expression (N));
         Decrement_Indentation;
      end if;
   end Generate_Parameter;

   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------

   procedure Generate_Parameter_Association (N : Node_Id) is
   begin
      Generate (Selector_Name (N));
      Write_Space;
      Write (Tok_Arrow);
      Write_Space;
      Generate (Actual_Parameter (N));
   end Generate_Parameter_Association;

   -----------------------------
   -- Generate_Parameter_List --
   -----------------------------

   procedure Generate_Parameter_List (L : List_Id) is
      N : Node_Id;

   begin
      --  If we got there, then L is not empty.

      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_Left_Paren);
      N := First_Node (L);
      loop
         Generate_Parameter (N);
         exit when No (Next_Node (N));
         Generate_Statement_Delimiter (N);
         Write_Indentation;
         N := Next_Node (N);
      end loop;
      Write (Tok_Right_Paren);
      Decrement_Indentation;
   end Generate_Parameter_List;

   -------------------------------
   -- Generate_Pragma_Statement --
   -------------------------------

   procedure Generate_Pragma_Statement (N : Node_Id) is
      Args : constant List_Id := Nodes.Argument_List (N);
      Arg  : Node_Id;
   begin
      Write (Tok_Pragma);
      Write_Space;
      Generate (Defining_Identifier (N));

      if not Is_Empty (Args) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         Arg := First_Node (Args);
         loop
            Generate (Arg);
            Arg := Next_Node (Arg);
            exit when No (Arg);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;
         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;
   end Generate_Pragma_Statement;

   ------------------------------
   -- Generate_Pragma_Warnings --
   ------------------------------

   procedure Generate_Pragma_Warnings (W : Pragma_W) is
   begin
      Write (Tok_Pragma);
      Write_Space;
      Write_Name (GN (Pragma_Warnings));
      Write_Space;
      Write (Tok_Left_Paren);

      if W = W_On then
         Write_Str ("On");
      else
         Write_Str ("Off");
      end if;

      Write (Tok_Right_Paren);
   end Generate_Pragma_Warnings;

   ------------------------------------
   -- Generate_Protected_Object_Spec --
   ------------------------------------

   procedure Generate_Protected_Object_Spec (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Protected);
      Write_Space;

      if Is_Type (N) then
         Write (Tok_Type);
         Write_Space;
      end if;

      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Is);
      Write_Eol;

      Increment_Indentation;
      P := First_Node (Visible_Part (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         P := Next_Node (P);
      end loop;
      Decrement_Indentation;

      if not Is_Empty (Private_Part (N)) then
         Write_Indentation;
         Write (Tok_Private);
         Write_Eol;
         Increment_Indentation;
         P := First_Node (Private_Part (N));
         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
         Decrement_Indentation;
      end if;

      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Generate (Defining_Identifier (N));
   end Generate_Protected_Object_Spec;

   ------------------------------------
   -- Generate_Protected_Object_Body --
   ------------------------------------

   procedure Generate_Protected_Object_Body (N : Node_Id) is
      P : Node_Id;
   begin
      Write (Tok_Protected);
      Write_Space;
      Write (Tok_Body);
      Write_Space;
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Is);
      Write_Eol;

      Increment_Indentation;
      P := First_Node (Statements (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         Write_Eol;
         P := Next_Node (P);
      end loop;
      Decrement_Indentation;
      Write_Indentation;

      Write (Tok_End);
      Write_Space;
      Generate (Defining_Identifier (N));
   end Generate_Protected_Object_Body;

   -----------------------------------
   -- Generate_Qualified_Expression --
   -----------------------------------

   procedure Generate_Qualified_Expression (N : Node_Id) is
   begin
      Generate (Subtype_Mark (N));
      Write_Line (Tok_Apostrophe);
      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Aggregate (N));
      Decrement_Indentation;
   end Generate_Qualified_Expression;

   -------------------------------
   -- Generate_Range_Constraint --
   -------------------------------

   procedure Generate_Range_Constraint (N : Node_Id) is
      May_Be_Unconstrained : Boolean := False;
   begin
      if Present (Index_Type (N)) then
         Generate (Index_Type (N));

         if Kind (Index_Type (N)) /= K_Attribute_Designator then
            May_Be_Unconstrained := True;

            Write_Space;
            Write (Tok_Range);
            Write_Space;
         end if;
      end if;

      if Present (First (N)) and then Present (Last (N)) then
         Generate (First (N));
         Write_Space;
         Write (Tok_Dot);
         Write (Tok_Dot);
         Write_Space;
         Generate (Last (N));
      elsif May_Be_Unconstrained then
         Write (Tok_Box);
      end if;
   end Generate_Range_Constraint;

   ------------------------------
   -- Generate_Raise_Statement --
   ------------------------------

   procedure Generate_Raise_Statement (N : Node_Id) is
      E : constant Node_Id := Raised_Error (N);
   begin
      Write (Tok_Raise);

      if Present (E) then
         Write_Space;
         Generate (E);
      end if;
   end Generate_Raise_Statement;

   -------------------------------
   -- Generate_Record_Aggregate --
   -------------------------------

   procedure Generate_Record_Aggregate (N : Node_Id) is
      L : List_Id;
      M : Node_Id;
   begin
      L := Component_Association_List (N);
      Write (Tok_Left_Paren);

      if not Is_Empty (L) then
         M := First_Node (L);
         loop
            Generate (M);
            M := Next_Node (M);
            exit when No (M);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;
      end if;

      Write (Tok_Right_Paren);
   end Generate_Record_Aggregate;

   --------------------------------
   -- Generate_Record_Definition --
   --------------------------------

   procedure Generate_Record_Definition (N : Node_Id) is
      L : constant List_Id := Component_List (N);
      C : Node_Id;

   begin
      if Is_Empty (L) then
         Write (Tok_Null);
         Write_Space;
         Write (Tok_Record);
      else
         Write_Space;
         Write (Tok_Record);
         Write_Eol;
         Increment_Indentation;
         C := First_Node (L);
         while Present (C) loop
            Write_Indentation;
            Generate (C);
            Generate_Statement_Delimiter (C);
            C := Next_Node (C);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
         Write (Tok_End);
         Write_Space;
         Write (Tok_Record);
      end if;
   end Generate_Record_Definition;

   -------------------------------------
   -- Generate_Record_Type_Definition --
   -------------------------------------

   procedure Generate_Record_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write (Tok_Abstract);
         Write_Space;
      end if;

      if Is_Tagged_Type (N) then
         Write (Tok_Tagged);
         Write_Space;
      end if;

      if Is_Limited_Type (N) then
         Write (Tok_Limited);
         Write_Space;
      end if;

      R := Record_Definition (N);

      if Present (R) then
         Generate (R);
      end if;
   end Generate_Record_Type_Definition;

   -------------------------------
   -- Generate_Return_Statement --
   -------------------------------

   procedure Generate_Return_Statement (N : Node_Id) is
      E : constant Node_Id := Expression (N);
   begin
      Write (Tok_Return);

      if Present (E) then
         Write_Space;
         Generate (E);
      end if;
   end Generate_Return_Statement;

   ---------------------------------
   -- Generate_Selected_Component --
   ---------------------------------

   procedure Generate_Selected_Component (N : Node_Id) is
   begin
      Generate (Prefix (N));
      Write (Tok_Dot);
      Generate (Selector_Name (N));
   end Generate_Selected_Component;

   ------------------------------
   -- Generate_Subprogram_Call --
   ------------------------------

   procedure Generate_Subprogram_Call (N : Node_Id) is
      L : constant List_Id := Actual_Parameter_Part (N);
      P : Node_Id;

   begin
      Generate (Defining_Identifier (N));

      if not Is_Empty (L) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         P := First_Node (L);
         loop
            Generate (P);
            P := Next_Node (P);
            exit when No (P);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;
         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;
   end Generate_Subprogram_Call;

   ----------------------------------------
   -- Generate_Subprogram_Implementation --
   ----------------------------------------

   procedure Generate_Subprogram_Implementation (N : Node_Id) is
      D : constant List_Id := Declarations (N);
      S : constant List_Id := Statements (N);
      P : constant Node_Id := Specification (N);
      M : Node_Id;
      W : Node_Id;
   begin

      --  If we deal with a main subprogram, then we generate its
      --  headers

      if not Is_Empty (Package_Headers (N)) then
         W := First_Node (Package_Headers (N));
         while Present (W) loop
            Generate (W);
            Generate_Statement_Delimiter (W);
            Write_Indentation;
            W := Next_Node (W);
         end loop;
         Write_Eol;
         Write_Indentation;
      end if;

      --  If we deal with a main subprogram, then we generate its
      --  withed packages

      if not Is_Empty (Withed_Packages (N)) then
         W := First_Node (Withed_Packages (N));
         while Present (W) loop
            Generate (W);
            Generate_Statement_Delimiter (W);
            Write_Indentation;
            W := Next_Node (W);
         end loop;
         Write_Eol;
         Write_Indentation;
      end if;

      Generate_Comment_Box (Name (Defining_Identifier (P)));
      Write_Eol;

      Write_Indentation;
      Generate (P);

      if not Is_Empty (Parameter_Profile (P)) then
         Write_Eol;
         Write_Indentation;
      else
         Write_Space;
      end if;

      Write (Tok_Is);
      Write_Eol;

      if not Is_Empty (D) then
         Increment_Indentation;
         M := First_Node (D);
         while Present (M) loop
            Write_Indentation;
            Generate (M);
            Generate_Statement_Delimiter (M);
            M := Next_Node (M);
         end loop;
         Decrement_Indentation;
      end if;

      Write_Indentation;
      Write (Tok_Begin);
      Write_Eol;
      Increment_Indentation;

      if not Is_Empty (S) then
         M := First_Node (S);
         while Present (M) loop
            Write_Indentation;
            Generate (M);
            Generate_Statement_Delimiter (M);
            M := Next_Node (M);
         end loop;
      else
         Write_Indentation;
         Write (Tok_Null);
         Write_Line (Tok_Semicolon);
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write_Name (Name (Defining_Identifier (P)));
   end Generate_Subprogram_Implementation;

   ---------------------------------------
   -- Generate_Subprogram_Specification --
   ---------------------------------------

   procedure Generate_Subprogram_Specification (N : Node_Id) is
      P : constant List_Id := Parameter_Profile (N);
      T : constant Node_Id := Return_Type (N);
      R : constant Node_Id := Renamed_Entity (N);
      G : constant Node_Id := Instantiated_Entity (N);
      W : Node_Id;
   begin
      --  If we deal with a main subprogram, then we generate its
      --  headers

      if not Is_Empty (Package_Headers (N)) then
         W := First_Node (Package_Headers (N));
         while Present (W) loop
            Generate (W);
            Generate_Statement_Delimiter (W);
            Write_Indentation;
            W := Next_Node (W);
         end loop;
         Write_Eol;
         Write_Indentation;
      end if;

      --  If we deal with a main subprogram, then we generate its
      --  withed packages

      if not Is_Empty (Withed_Packages (N)) then
         W := First_Node (Withed_Packages (N));
         while Present (W) loop
            Generate (W);
            Generate_Statement_Delimiter (W);
            Write_Indentation;
            W := Next_Node (W);
         end loop;
         Write_Eol;
         Write_Indentation;
      end if;

      if Present (T) then
         Write (Tok_Function);
      else
         Write (Tok_Procedure);
      end if;

      --  This work around is used to define access subprogram types

      if Present (Defining_Identifier (N)) then
         Write_Space;
         Write_Name (Name (Defining_Identifier (N)));
      end if;

      if not Is_Empty (P) then
         Write_Eol;
         Generate_Parameter_List (P);
      end if;

      if Present (T) then
         if not Is_Empty (P) then
            Write_Eol;
            Increment_Indentation;
            Write_Indentation (-1);
         else
            Write_Space;
         end if;

         Write (Tok_Return);
         Write_Space;
         Generate (T);

         if not Is_Empty (P) then
            Decrement_Indentation;
         end if;
      end if;

      if Present (R) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Renames);
         Write_Space;
         Generate (R);
         Decrement_Indentation;
      end if;

      if Present (G) then
         Write_Space;
         Write (Tok_Is);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_New);
         Write_Space;
         Generate (G);
         Decrement_Indentation;
      end if;

      Generate_Aspect (Aspect_Specification (N));
   end Generate_Subprogram_Specification;

   ---------------------
   -- Generate_Aspect --
   ---------------------

   procedure Generate_Aspect (N : Node_Id) is
      W : Node_Id;
   begin
      if Present (N) and then
        not Is_Empty (Aspect (N))
      then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         Write (Tok_With);
         Write_Space;

         W := First_Node (Aspect (N));
         while Present (W) loop
            Write_Name (Aspect_Mark (W));
            if Present (Aspect_Definition (W)) then
               if Kind (Aspect_Definition (W)) = K_Pre_Definition then
                  Write_Space;
                  Write (Tok_Arrow);
                  Write_Space;
                  Write (Tok_Left_Paren);
                  Generate (Subprogram_Call (Aspect_Definition (W)));
                  Write (Tok_Right_Paren);

               elsif Kind (Aspect_Definition (W)) = K_Global_Specification then
                  declare
                     X : Node_Id;
                  begin
                     X := First_Node (Moded_Global_List
                                        (Aspect_Definition (W)));
                     while (Present (X)) loop
                        Write_Space;
                        Write (Tok_Arrow);
                        Write_Space;
                        Write (Tok_Left_Paren);
                        if Mode_Selector (X) = Mode_In then
                           Write_Str ("Input => ");
                        else
                           raise Program_Error;
                        end if;
                        Write (Tok_Left_Paren);
                        Generate (Defining_Identifier (X));
                        Write (Tok_Right_Paren);
                        Write (Tok_Right_Paren);
                        X := Next_Node (X);
                     end loop;
                  end;

               elsif Kind (Aspect_Definition (W)) = K_Initialization_Spec then
                  declare
                     X : Node_Id;
                  begin
                     X := First_Node (Initialization_List
                                        (Aspect_Definition (W)));
                     while (Present (X)) loop
                        Write_Space;
                        Write (Tok_Arrow);
                        Write_Space;
                        Write (Tok_Left_Paren);
                        Generate (X);
                        Write (Tok_Right_Paren);
                        X := Next_Node (X);
                        exit when No (W);
                     end loop;
                  end;

               elsif Kind (Aspect_Definition (W)) = K_Abstract_State_List then
                  declare
                     X : Node_Id;
                  begin
                     X := First_Node (State_Name_With_Option
                                        (Aspect_Definition (W)));
                     while (Present (X)) loop
                        Write_Space;
                        Write (Tok_Arrow);
                        Write_Space;
                        Write (Tok_Left_Paren);
                        Generate (Defining_Identifier (X));

                        if Synchronous (X) or else External (X) then
                           Write_Space;
                           Write (Tok_With);
                           Write_Space;

                           if Synchronous (X) then
                              Write_Str ("Synchronous");
                           end if;
                           if Synchronous (X) and then External (X) then
                              Write_Str (", ");
                           end if;
                           if External (X) then
                              Write_Str ("External");
                           end if;
                        end if;

                        Write (Tok_Right_Paren);
                        X := Next_Node (X);
                        exit when No (W);
                     end loop;
                  end;

               elsif Kind (Aspect_Definition (W)) = K_Refinement_List then
                  declare
                     X : Node_Id;
                  begin
                     X := First_Node (Refinement_Clause
                                        (Aspect_Definition (W)));
                     while (Present (X)) loop
                        Write_Space;
                        Write (Tok_Arrow);
                        Write_Space;
                        Write (Tok_Left_Paren);
                        Generate (State_Name (X));

                        declare
                           Y : Node_Id;
                        begin
                           Y := First_Node (Constituent (X));
                           if Present (Y) then
                              Write_Space;
                              Write (Tok_Arrow);
                              Write_Eol;
                              Write_Indentation (9);
                              Write (Tok_Left_Paren);
                           end if;

                           while (Present (Y)) loop
                              Generate (Y);
                              Y := Next_Node (Y);
                              exit when No (Y);
                              Write (Tok_Comma);
                              Write_Eol;
                              Write_Indentation (10);
                           end loop;
                        end;

                        Write (Tok_Right_Paren);
                        X := Next_Node (X);
                        exit when No (W);
                     end loop;
                     Write (Tok_Right_Paren);
                  end;

               end if;
            end if;
            W := Next_Node (W);

            exit when No (W);
            Write (Tok_Comma);
            Write_Eol;
            Write_Indentation (5);
         end loop;
         Decrement_Indentation;
         Write_Space;
      end if;
   end Generate_Aspect;

   ------------------------------
   -- Generate_Type_Conversion --
   ------------------------------

   procedure Generate_Type_Conversion (N : Node_Id) is
   begin
      Generate (Subtype_Mark (N));
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_Left_Paren);
      Generate (Expression (N));
      Write (Tok_Right_Paren);
      Decrement_Indentation;
   end Generate_Type_Conversion;

   ------------------------
   -- Generate_Used_Type --
   ------------------------

   procedure Generate_Used_Type (N : Node_Id) is
   begin
      Write (Tok_Use);
      Write_Space;
      Write (Tok_Type);
      Write_Space;
      Generate (The_Used_Entity (N));
   end Generate_Used_Type;

   ---------------------------
   -- Generate_Used_Package --
   ---------------------------

   procedure Generate_Used_Package (N : Node_Id) is
   begin
      Write (Tok_Use);
      Write_Space;
      Generate (The_Used_Entity (N));
   end Generate_Used_Package;

   ---------------------------
   -- Generate_Variant_Part --
   ---------------------------

   procedure Generate_Variant_Part (N : Node_Id) is
      V : Node_Id;
      C : Node_Id;
      O : Node_Id := No_Node;
      R : Node_Id;

   begin
      Write (Tok_Case);
      Write_Space;
      Generate (Discriminant (N));
      Write_Space;
      Write (Tok_Is);
      Write_Eol;
      V := First_Node (Variants (N));
      Increment_Indentation;
      while Present (V) loop
         C := First_Node (Discrete_Choices (V));

         if No (C)
           or else (Kind (C) = K_Literal and then Value (C) = No_Value)
         then
            O := V;
         else
            Write_Indentation;
            Write (Tok_When);
            Write_Space;
            Increment_Indentation;
            loop
               Generate (C);
               C := Next_Node (C);

               if No (C) then
                  Write_Space;
                  Write (Tok_Arrow);
                  Write_Eol;
                  exit;
               end if;

               Write_Eol;
               Write_Indentation (-1);
               Write (Tok_Vertical_Bar);
               Write_Space;
            end loop;
            Write_Indentation;

            if not Is_Empty (Component_List (V)) then
               R := First_Node (Component_List (V));

               while Present (R) loop
                  Generate (R);
                  Generate_Statement_Delimiter (R);
                  R := Next_Node (R);
                  exit when No (R);
                  Write_Indentation;
               end loop;
            else
               Write (Tok_Null);
               Write_Line (Tok_Semicolon);
            end if;

            Decrement_Indentation;
         end if;

         V := Next_Node (V);
      end loop;

      --  Add a "when others" clause either based on the "default"
      --  label or a null one. In case of null statement, add two
      --  pragmas to disable warnings and enable them after the
      --  addition of the null statement

      if No (O) then
         Write_Indentation;
         Generate_Pragma_Warnings (W_Off);
         Write_Line (Tok_Semicolon);
      end if;

      Write_Indentation;
      Write (Tok_When);
      Write_Space;
      Write (Tok_Others);
      Write_Space;
      Write (Tok_Arrow);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;

      if Present (O) then
         if not Is_Empty (Component_List (O)) then
            R := First_Node (Component_List (O));

            while Present (R) loop
               Generate (R);
               Generate_Statement_Delimiter (R);
               R := Next_Node (R);
            end loop;
         else
            Write (Tok_Null);
            Write_Line (Tok_Semicolon);
         end if;
      else
         Write (Tok_Null);
         Generate_Statement_Delimiter (O);
      end if;

      Decrement_Indentation;

      if No (O) then
         Write_Indentation;
         Generate_Pragma_Warnings (W_On);
         Write_Line (Tok_Semicolon);
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_Case);
   end Generate_Variant_Part;

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

   -----------------------------
   -- Generate_Withed_Package --
   -----------------------------

   procedure Generate_Withed_Package (N : Node_Id) is
   begin
      Write (Tok_With);
      Write_Space;
      Generate (Defining_Identifier (N));

      if Used (N) then
         Write (Tok_Semicolon);
         Write_Eol;
         Write_Indentation;
         Write (Tok_Use);
         Write_Space;
         Generate (Defining_Identifier (N));
      end if;

      if Warnings_Off (N) then
         Write (Tok_Semicolon);
         Write_Eol;
         Write_Indentation;
         Write (Tok_Pragma);
         Write_Space;
         Write_Str ("Warnings");
         Write_Space;
         Write (Tok_Left_Paren);
         Write_Str ("Off");
         Write (Tok_Comma);
         Write_Space;
         Generate (Defining_Identifier (N));
         Write (Tok_Right_Paren);
      end if;

      if Elaborated (N) then
         Write (Tok_Semicolon);
         Write_Eol;
         Write_Indentation;
         Write (Tok_Pragma);
         Write_Space;
         Write_Str ("Elaborate_All");
         Write_Space;
         Write (Tok_Left_Paren);
         Generate (Defining_Identifier (N));
         Write (Tok_Right_Paren);
      end if;

   end Generate_Withed_Package;

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
      if No (N) or else Kind (N) /= K_Ada_Comment then
         Write_Line (Tok_Semicolon);
      else
         Write_Eol;
      end if;
   end Generate_Statement_Delimiter;

   --------------------------
   -- Generate_Comment_Box --
   --------------------------

   procedure Generate_Comment_Box (M : Name_Id) is
   begin
      Get_Name_String (M);

      for I in 1 .. Name_Len + 6 loop
         Write_Char ('-');
      end loop;
      Write_Eol;
      Write_Indentation;

      Write_Str ("-- ");
      Write_Name (M);
      Write_Str (" -- ");
      Write_Eol;
      Write_Indentation;

      for I in 1 .. Name_Len + 6 loop
         Write_Char ('-');
      end loop;
      Write_Eol;
   end Generate_Comment_Box;

end Ocarina.Backends.Ada_Tree.Generator;
