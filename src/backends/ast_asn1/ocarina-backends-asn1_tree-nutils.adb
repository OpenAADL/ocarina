------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . A S N 1 _ T R E E . N U T I L S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2018 ESA & ISAE.                    --
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

with GNAT.Table;

with Ocarina.Namet; use Ocarina.Namet;
with Charset;       use Charset;
with Locations;     use Locations;

with Ocarina.Backends.ASN1_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;

use Ocarina.Backends.ASN1_Tree.Nodes;

package body Ocarina.Backends.ASN1_Tree.Nutils is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   Initialized : Boolean := False;

   Keyword_Suffix : constant String := "%C";
   --  Used to mark C keywords and avoid collision with other languages

   type Entity_Stack_Entry is record
      Current_File   : Node_Id;
      Current_Entity : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is new GNAT.Table
     (Entity_Stack_Entry,
      Int,
      No_Depth + 1,
      10,
      10);

   procedure New_Operator (O : Operator_Type; I : String := "");

   ------------------------
   -- Add_Prefix_To_Name --
   ------------------------

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id
   is
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Get_Name_String_And_Append (Name);
      return Name_Find;
   end Add_Prefix_To_Name;

   ------------------------
   -- Add_Suffix_To_Name --
   ------------------------

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id
   is
   begin
      Get_Name_String (Name);
      Add_Str_To_Name_Buffer (Suffix);
      return Name_Find;
   end Add_Suffix_To_Name;

   -----------------------------
   -- Remove_Suffix_From_Name --
   -----------------------------

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id
   is
      Length   : Natural;
      Temp_Str : String (1 .. Suffix'Length);
   begin
      Set_Str_To_Name_Buffer (Suffix);
      Length := Name_Len;
      Get_Name_String (Name);
      if Name_Len > Length then
         Temp_Str := Name_Buffer (Name_Len - Length + 1 .. Name_Len);
         if Suffix = Temp_Str then
            Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len - Length));
            return Name_Find;
         end if;
      end if;
      return Name;
   end Remove_Suffix_From_Name;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : ASN1_Nodes.Node_Kind;
      From : Node_Id := No_Node) return List_Id
   is
      N : Node_Id;
   begin
      ASN1_Nodes.Entries.Increment_Last;
      N                            := ASN1_Nodes.Entries.Last;
      ASN1_Nodes.Entries.Table (N) := ASN1_Nodes.Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         ASN1_Nodes.Set_Loc (N, ASN1_Nodes.Loc (From));
      else
         ASN1_Nodes.Set_Loc (N, No_Location);
      end if;
      return List_Id (N);
   end New_List;

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;

   begin
      Last := ASN1_Nodes.Last_Node (L);
      if No (Last) then
         ASN1_Nodes.Set_First_Node (L, E);
      else
         ASN1_Nodes.Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         ASN1_Nodes.Set_Last_Node (L, Last);
         Last := ASN1_Nodes.Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   -----------------------
   -- Insert_After_Node --
   -----------------------

   procedure Insert_After_Node (E : Node_Id; N : Node_Id) is
      Next : constant Node_Id := ASN1_Nodes.Next_Node (N);
   begin
      ASN1_Nodes.Set_Next_Node (N, E);
      ASN1_Nodes.Set_Next_Node (E, Next);
   end Insert_After_Node;

   ------------------------
   -- Insert_Before_Node --
   ------------------------

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id) is
      Entity : Node_Id;
   begin
      Entity := ASN1_Nodes.First_Node (L);
      if Entity = N then
         ASN1_Nodes.Set_Next_Node (E, Entity);
         ASN1_Nodes.Set_First_Node (L, E);
      else
         while Present (Entity) loop
            exit when ASN1_Nodes.Next_Node (Entity) = N;
            Entity := ASN1_Nodes.Next_Node (Entity);
         end loop;

         Insert_After_Node (E, Entity);
      end if;
   end Insert_Before_Node;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (N : Node_Id) return Node_Id is
      C : Node_Id;
   begin
      case ASN1_Nodes.Kind (N) is
         when ASN1_Nodes.K_Defining_Identifier =>
            C := New_Node (ASN1_Nodes.K_Defining_Identifier);
            ASN1_Nodes.Set_Name (C, ASN1_Nodes.Name (N));
            ASN1_Nodes.Set_Corresponding_Node
              (C,
               ASN1_Nodes.Corresponding_Node (N));

         when others =>
            raise Program_Error;
      end case;
      return C;
   end Copy_Node;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
      S : String := Token_Type'Image (T);
   begin
      To_Lower (S);
      return S (5 .. S'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : Operator_Type) return String is
      S : String := Operator_Type'Image (O);
   begin
      To_Lower (S);
      for I in S'First .. S'Last loop
         if S (I) = '_' then
            S (I) := ' ';
         end if;
      end loop;
      return S (4 .. S'Last);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize Nutils only once

      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Keywords.
      for I in Keyword_Type loop
         New_Token (I);
      end loop;

      --  Graphic Characters
      New_Token (Tok_Module, "MODULE");
      New_Token (Tok_And, "&&");
      New_Token (Tok_Xor, "^");
      New_Token (Tok_Sharp, "#");
      New_Token (Tok_Or, "||");
      New_Token (Tok_Left_Brace, "{");
      New_Token (Tok_Right_Brace, "}");
      New_Token (Tok_Mod, "%");
      New_Token (Tok_Not, "!");
      New_Token (Tok_Ampersand, "&");
      New_Token (Tok_Minus, "-");
      New_Token (Tok_Underscore, "_");
      New_Token (Tok_Plus, "+");
      New_Token (Tok_Asterisk, "*");
      New_Token (Tok_Slash, "/");
      New_Token (Tok_Dot, ".");
      New_Token (Tok_Apostrophe, "'");
      New_Token (Tok_Left_Paren, "(");
      New_Token (Tok_Right_Paren, ")");
      New_Token (Tok_Left_Hook, "[");
      New_Token (Tok_Right_Hook, "]");
      New_Token (Tok_Comma, ",");
      New_Token (Tok_Less, "<");
      New_Token (Tok_Equal, "=");
      New_Token (Tok_Equal_Equal, "==");
      New_Token (Tok_Greater, ">");
      New_Token (Tok_Not_Equal, "!=");
      New_Token (Tok_Greater_Equal, ">=");
      New_Token (Tok_Less_Equal, "<=");
      New_Token (Tok_Colon, ":");
      New_Token (Tok_Greater_Greater, ">>");
      New_Token (Tok_Less_Less, "<<");
      New_Token (Tok_Quote, """");
      New_Token (Tok_Semicolon, ";");
      New_Token (Tok_Arrow, "->");
      New_Token (Tok_Vertical_Bar, "|");

      for O in Op_And .. Op_Or_Else loop
         New_Operator (O);
      end loop;
      New_Operator (Op_And_Symbol, "&");
      New_Operator (Op_Double_Asterisk, "**");
      New_Operator (Op_Asterisk, "**");
      New_Operator (Op_Minus, "-");
      New_Operator (Op_Plus, "+");
      New_Operator (Op_Asterisk, "*");
      New_Operator (Op_Slash, "/");
      New_Operator (Op_Less, "<");
      New_Operator (Op_Equal, "=");
      New_Operator (Op_Equal_Equal, "==");
      New_Operator (Op_Greater, ">");
      New_Operator (Op_Not_Equal, "!=");
      New_Operator (Op_Greater_Equal, ">=");
      New_Operator (Op_Less_Equal, "<=");
      New_Operator (Op_Greater_Greater, ">>");
      New_Operator (Op_Less_Less, "<<");
      New_Operator (Op_Semicolon, ";");
      New_Operator (Op_Arrow, "=>");
      New_Operator (Op_Vertical_Bar, "|");
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Entity_Stack.Init;

      Initialized := False;
   end Reset;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (ASN1_Nodes.First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List_Id) return Natural is
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := ASN1_Nodes.First_Node (L);

         while Present (N) loop
            C := C + 1;
            N := ASN1_Nodes.Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : ASN1_Nodes.Node_Kind;
      From : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      ASN1_Nodes.Entries.Increment_Last;
      N                            := ASN1_Nodes.Entries.Last;
      ASN1_Nodes.Entries.Table (N) := ASN1_Nodes.Default_Node;
      ASN1_Nodes.Set_Kind (N, Kind);

      if Present (From) then
         ASN1_Nodes.Set_Loc (N, AIN.Loc (From));
      else
         ASN1_Nodes.Set_Loc (N, No_Location);
      end if;

      return N;
   end New_Node;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token (T : Token_Type; I : String := "") is
      Name : Name_Id;
   begin
      if T in Keyword_Type then
         --  Marking the token image as a keyword for fas searching
         --  purpose, we add the prefix to avoir collision with other
         --  languages keywords

         Set_Str_To_Name_Buffer (Image (T));
         Name := Name_Find;
         Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
         Set_Name_Table_Byte
           (Name,
            Ocarina.Types.Byte (Token_Type'Pos (T) + 1));

         Set_Str_To_Name_Buffer (Image (T));
      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Token_Image (T) := Name_Find;
   end New_Token;

   ------------------
   -- New_Operator --
   ------------------

   procedure New_Operator (O : Operator_Type; I : String := "") is
   begin
      if O in Keyword_Operator then
         Set_Str_To_Name_Buffer (Image (O));
      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Operator_Image (Operator_Type'Pos (O)) := Name_Find;
   end New_Operator;

   --------------------
   -- Make_ASN1_File --
   --------------------

   function Make_ASN1_File (Identifier : Node_Id) return Node_Id is
      File : Node_Id;
   begin
      File := New_Node (K_ASN1_File);
      Set_Defining_Identifier (File, Identifier);
      Set_Corresponding_Node (Identifier, File);

      Set_Module_Node (File, New_Node (K_ASN1_Module));
      Set_Name (Module_Node (File), Get_String_Name ("unknownmodule"));
      Set_Definitions (Module_Node (File), New_List (K_List_Id));
      return File;
   end Make_ASN1_File;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Defining_Identifier);
      Set_Name (N, Name);
      return N;
   end Make_Defining_Identifier;

   ---------------------------
   -- Make_Enumerated_Value --
   ---------------------------

   function Make_Enumerated_Value (Name : Name_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Enumerated_Value);
      Set_Name (N, Name);
      Set_Value (N, No_Value);
      return N;
   end Make_Enumerated_Value;

   ---------------------------
   -- Make_Enumerated_Value --
   ---------------------------

   function Make_Enumerated_Value
     (Name : Name_Id;
      V    : Unsigned_Long_Long) return Node_Id
   is
      N : Node_Id;
   begin
      N := Make_Enumerated_Value (Name);
      Set_Value (N, ASN1_Values.New_Int_Value (V, 1, 10));
      return N;
   end Make_Enumerated_Value;

   --------------------------
   -- Make_Type_Definition --
   --------------------------

   function Make_Type_Definition
     (Name : Name_Id;
      Decl : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Type_Definition);
      Set_Name (N, Name);
      Set_Declaration (N, Decl);
      return N;
   end Make_Type_Definition;

   ---------------------
   -- Make_Enumerated --
   ---------------------

   function Make_Enumerated return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Enumerated);
      Set_Values (N, New_List (K_Enumerated_Value));
      return N;
   end Make_Enumerated;

   ---------------------
   -- Make_Enumerated --
   ---------------------

   function Make_Enumerated (L : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Enumerated);
      Set_Values (N, L);
      return N;
   end Make_Enumerated;

   -------------------
   -- Make_Sequence --
   -------------------

   function Make_Sequence (Sequence_Members : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Sequence);
      Set_Values (N, Sequence_Members);
      return N;
   end Make_Sequence;

   --------------------------
   -- Make_Sequence_Member --
   --------------------------

   function Make_Sequence_Member
     (Member_Name : Name_Id;
      Member_Type : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Sequence_Member);
      Set_Member_Name (N, Member_Name);
      Set_Member_Type (N, Member_Type);
      return N;
   end Make_Sequence_Member;

   -----------------
   -- Make_Choice --
   -----------------

   function Make_Choice (Choice_Members : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Choice);
      Set_Values (N, Choice_Members);
      return N;
   end Make_Choice;

   ------------------------
   -- Make_Choice_Member --
   ------------------------

   function Make_Choice_Member
     (Member_Name : Name_Id;
      Member_Type : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Choice_Member);
      Set_Member_Name (N, Member_Name);
      Set_Member_Type (N, Member_Type);
      return N;
   end Make_Choice_Member;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal (Value : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Literal);
      ASN1_Nodes.Set_Value (N, Value);
      return N;
   end Make_Literal;

   ---------------------------
   -- Make_Type_Constraints --
   ---------------------------

   function Make_Type_Constraints
     (Size_Up   : Value_Id := No_Value;
      Size_Down : Value_Id := No_Value) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Type_Constraints);
      ASN1_Nodes.Set_Size_Up (N, Size_Up);
      ASN1_Nodes.Set_Size_Down (N, Size_Down);
      return N;
   end Make_Type_Constraints;

   --------------------------
   -- Make_Type_Designator --
   --------------------------

   function Make_Type_Designator
     (Type_Name        : Node_Id;
      Type_Constraints : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Type_Designator);
      ASN1_Nodes.Set_Type_Name (N, Type_Name);
      ASN1_Nodes.Set_Constraints (N, Type_Constraints);
      return N;
   end Make_Type_Designator;

end Ocarina.Backends.ASN1_Tree.Nutils;
