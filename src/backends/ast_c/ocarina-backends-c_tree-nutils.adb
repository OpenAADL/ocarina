------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . C _ T R E E . N U T I L S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2020 ESA & ISAE.      --
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
with GNAT.Case_Util;

with Charset;       use Charset;
with Locations;     use Locations;
with Ocarina.Namet; use Ocarina.Namet;
with Utils;         use Utils;

with Ocarina.Backends;
with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.POK_C;
with Ocarina.Backends.POK_C.Runtime;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.Properties;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Instances.Queries;

use Ocarina.ME_AADL.AADL_Instances.Nodes;
use Ocarina.Backends;
use Ocarina.Backends.Utils;
use Ocarina.Backends.Messages;
use Ocarina.Backends.Properties;
use Ocarina.Backends.C_Common.Mapping;
use Ocarina.Backends.C_Values;

use Ocarina.Instances.Queries;

package body Ocarina.Backends.C_Tree.Nutils is

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package CV renames Ocarina.Backends.C_Values;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package PHCR renames Ocarina.Backends.PO_HI_C.Runtime;
   package PKR renames Ocarina.Backends.POK_C.Runtime;

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

   use Entity_Stack;

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

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;

   begin
      Last := CTN.Last_Node (L);
      if No (Last) then
         CTN.Set_First_Node (L, E);
      else
         CTN.Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         CTN.Set_Last_Node (L, Last);
         Last := CTN.Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   -----------------------
   -- Insert_After_Node --
   -----------------------

   procedure Insert_After_Node (E : Node_Id; N : Node_Id) is
      Next : constant Node_Id := CTN.Next_Node (N);
   begin
      CTN.Set_Next_Node (N, E);
      CTN.Set_Next_Node (E, Next);
   end Insert_After_Node;

   ------------------------
   -- Insert_Before_Node --
   ------------------------

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id) is
      Entity : Node_Id;
   begin
      Entity := CTN.First_Node (L);
      if Entity = N then
         CTN.Set_Next_Node (E, Entity);
         CTN.Set_First_Node (L, E);
      else
         while Present (Entity) loop
            exit when CTN.Next_Node (Entity) = N;
            Entity := CTN.Next_Node (Entity);
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
      case CTN.Kind (N) is
         when K_Defining_Identifier =>
            C := New_Node (K_Defining_Identifier);
            CTN.Set_Name (C, CTN.Name (N));
            CTN.Set_Corresponding_Node (C, CTN.Corresponding_Node (N));

         when K_Function_Specification =>
            C := New_Node (K_Function_Specification);
            CTN.Set_Defining_Identifier
              (C,
               CTU.Copy_Node (Defining_Identifier (N)));
            CTN.Set_Parameters (C, CTN.Parameters (N));
            CTN.Set_Return_Type (C, CTN.Return_Type (N));

         when K_Include_Clause =>
            C := New_Node (K_Include_Clause);
            CTN.Set_Header_Name (C, CTU.Copy_Node (Header_Name (N)));
            CTN.Set_Is_Local (C, CTN.Is_Local (N));

         when K_Literal =>
            C := New_Node (K_Literal);
            CTN.Set_Value (C, CTN.Value (N));

         when K_Ifdef_Clause =>
            C := New_Node (K_Ifdef_Clause);
            CTN.Set_Negation (C, Negation (N));
            CTN.Set_Then_Statements (C, Then_Statements (N));
            CTN.Set_Else_Statements (C, Else_Statements (N));
            CTN.Set_Clause (C, Copy_Node (Clause (N)));

         when others =>
            raise Program_Error;
      end case;
      return C;
   end Copy_Node;

   ---------------------
   -- Message_Comment --
   ---------------------

   function Message_Comment (M : Name_Id) return Node_Id is
      C : Node_Id;
   begin
      C := Make_C_Comment (M);
      return C;
   end Message_Comment;

   ---------------------
   -- Message_Comment --
   ---------------------

   function Message_Comment (M : String) return Node_Id is
      C : Node_Id;
   begin
      Set_Str_To_Name_Buffer (M);
      C := Make_C_Comment (Name_Find);
      return C;
   end Message_Comment;

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
      New_Token (Tok_Plus_Plus, "++");
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

      New_Operator (Op_Not, "!");
      New_Operator (Op_And, "&&");
      New_Operator (Op_Or, "||");
      New_Operator (Op_And_Symbol, "&");
      New_Operator (Op_Double_Asterisk, "**");
      New_Operator (Op_Asterisk, "**");
      New_Operator (Op_Minus, "-");
      New_Operator (Op_Plus, "+");
      New_Operator (Op_Plus_Plus, "++");
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
      New_Operator (Op_Modulo, "%");
      New_Operator (Op_Vertical_Bar, "|");

      for A in Attribute_Id loop
         Set_Str_To_Name_Buffer (Attribute_Id'Image (A));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         AN (A) := Name_Find;
      end loop;

      for C in Constant_Id loop
         Set_Str_To_Name_Buffer (Constant_Id'Image (C));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         CONST (C) := To_Upper (Name_Find);
      end loop;

      for P in Parameter_Id loop
         Set_Str_To_Name_Buffer (Parameter_Id'Image (P));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         PN (P) := Name_Find;
      end loop;

      for F in Function_Id loop
         Set_Str_To_Name_Buffer (Function_Id'Image (F));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         FN (F) := To_Lower (Name_Find);
      end loop;

      for T in Type_Id loop
         Set_Str_To_Name_Buffer (Type_Id'Image (T));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         TN (T) := To_Lower (Name_Find);
      end loop;

      for V in Variable_Id loop
         Set_Str_To_Name_Buffer (Variable_Id'Image (V));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         VN (V) := To_Lower (Name_Find);
      end loop;

      for V in Member_Id loop
         Set_Str_To_Name_Buffer (Member_Id'Image (V));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));

         MN (V) := To_Lower (Name_Find);
      end loop;
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
      return L = No_List or else No (CTN.First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List_Id) return Natural is
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := CTN.First_Node (L);

         while Present (N) loop
            C := C + 1;
            N := CTN.Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   --------------------
   -- Make_C_Comment --
   --------------------

   function Make_C_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True) return Node_Id
   is
      C : Node_Id;
   begin
      C := New_Node (K_C_Comment);
      Set_Defining_Identifier (C, New_Node (K_Defining_Identifier));
      CTN.Set_Name (Defining_Identifier (C), N);
      CTN.Set_Has_Header_Spaces (C, Has_Header_Spaces);
      return C;
   end Make_C_Comment;

   -------------------------------
   -- Make_Assignment_Statement --
   -------------------------------

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Assignment_Statement);
      Set_Defining_Identifier (N, Variable_Identifier);
      Set_Expression (N, Expression);
      return N;
   end Make_Assignment_Statement;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier
     (Name             : Name_Id;
      C_Conversion     : Boolean := True;
      Ada_Conversion   : Boolean := False;
      Pointer          : Boolean := False;
      Variable_Address : Boolean := False) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Defining_Identifier);
      if C_Conversion then
         CTN.Set_Name (N, To_C_Name (Name, Ada_Conversion));
      else
         CTN.Set_Name (N, Name);
      end if;

      if Pointer then
         CTN.Set_Is_Pointer (N, True);
      end if;

      if Variable_Address then
         CTN.Set_Is_Variable_Address (N, True);
      end if;

      return N;
   end Make_Defining_Identifier;

   ---------------------
   -- Make_Expression --
   ---------------------

   function Make_Expression
     (Left_Expr  : Node_Id;
      Operator   : Operator_Type := Op_None;
      Right_Expr : Node_Id       := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Expression);
      Set_Left_Expression (N, Left_Expr);
      Set_Operator (N, Operator_Type'Pos (Operator));
      Set_Right_Expression (N, Right_Expr);
      return N;
   end Make_Expression;

   ------------------------
   -- Make_For_Statement --
   ------------------------

   function Make_For_Statement
     (Pre_Cond            : Node_Id;
      Condition           : Node_Id;
      Post_Cond           : Node_Id;
      Statements          : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_For_Statement);
      Set_Pre_Cond (N, Pre_Cond);
      Set_Condition (N, Condition);
      Set_Post_Cond (N, Post_Cond);
      Set_Statements (N, Statements);
      return N;
   end Make_For_Statement;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal (Value : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Literal);
      CTN.Set_Value (N, Value);
      return N;
   end Make_Literal;

   -------------------------
   -- Make_Loop_Statement --
   -------------------------

   function Make_While_Statement
     (Condition  : Node_Id;
      Statements : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_While_Statement);
      Set_Condition (N, Condition);
      Set_Statements (N, Statements);
      return N;
   end Make_While_Statement;

   --------------------------------
   -- Make_Full_Type_Declaration --
   --------------------------------

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Full_Type_Declaration);
      Set_Type_Name (N, Defining_Identifier);
      Set_Type_Definition (N, Type_Definition);
      return N;
   end Make_Full_Type_Declaration;

   -----------------------
   -- Make_If_Statement --
   -----------------------

   function Make_If_Statement
     (Condition       : Node_Id;
      Statements      : List_Id;
      Else_Statements : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_If_Statement);
      Set_Condition (N, Condition);
      Set_Statements (N, Statements);
      Set_Else_Statements (N, Else_Statements);
      return N;
   end Make_If_Statement;

   ------------------
   -- Make_List_Id --
   ------------------

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node) return List_Id
   is
      L : List_Id;
   begin
      L := New_List (K_List_Id);
      Append_Node_To_List (N1, L);
      if Present (N2) then
         Append_Node_To_List (N2, L);

         if Present (N3) then
            Append_Node_To_List (N3, L);
         end if;
      end if;
      return L;
   end Make_List_Id;

   ----------------------------------
   -- Make_Parameter_Specification --
   ----------------------------------

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Type      : Node_Id := No_Node) return Node_Id
   is
      P : Node_Id;

   begin
      P := New_Node (K_Parameter_Specification);
      Set_Defining_Identifier (P, Defining_Identifier);
      Set_Parameter_Type (P, Parameter_Type);
      return P;
   end Make_Parameter_Specification;

   ---------------------------
   -- Make_Return_Statement --
   ---------------------------

   function Make_Return_Statement
     (Expression : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Return_Statement);
      if Expression /= No_Node then
         Set_Expression (N, Expression);
      end if;
      return N;
   end Make_Return_Statement;

   ---------------------------------
   -- Make_Function_Specification --
   ---------------------------------

   function Make_Function_Specification
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List;
      Return_Type         : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Function_Specification);
      Set_Parameters (N, Parameters);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Return_Type (N, Return_Type);
      return N;
   end Make_Function_Specification;

   ----------------------------------
   -- Make_Function_Implementation --
   ----------------------------------

   function Make_Function_Implementation
     (Specification : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Function_Implementation);
      Set_Specification (N, Specification);
      CTN.Set_Declarations (N, Declarations);
      Set_Statements (N, Statements);
      return N;
   end Make_Function_Implementation;

   -----------------------------
   -- Make_Member_Declaration --
   -----------------------------

   function Make_Member_Declaration
     (Defining_Identifier : Node_Id;
      Used_Type           : Node_Id) return Node_Id
   is
      P : Node_Id;
   begin
      P := New_Node (K_Member_Declaration);
      Set_Defining_Identifier (P, Defining_Identifier);
      Set_Used_Type (P, Used_Type);
      return P;
   end Make_Member_Declaration;

   -------------------------------
   -- Make_Variable_Declaration --
   -------------------------------

   function Make_Variable_Declaration
     (Defining_Identifier : Node_Id;
      Used_Type           : Node_Id;
      Is_Static           : Boolean := False;
      Value               : Node_Id := No_Node) return Node_Id
   is
      P : Node_Id;
   begin
      P := New_Node (K_Variable_Declaration);
      Set_Defining_Identifier (P, Defining_Identifier);
      Set_Used_Type (P, Used_Type);
      Set_Is_Static (P, Is_Static);
      Set_Initialization_Value (P, Value);
      return P;
   end Make_Variable_Declaration;

   ---------------------------
   -- Make_Variable_Address --
   ---------------------------

   function Make_Variable_Address (Expression : Node_Id) return Node_Id is
      P : Node_Id;
   begin
      P := New_Node (K_Variable_Address);
      Set_Expression (P, Expression);
      return P;
   end Make_Variable_Address;

   ------------------------------------
   -- Make_Extern_Entity_Declaration --
   ------------------------------------

   function Make_Extern_Entity_Declaration (Entity : Node_Id) return Node_Id is
      P : Node_Id;
   begin
      P := New_Node (K_Extern_Entity_Declaration);
      CTN.Set_Entity (P, Entity);
      return P;
   end Make_Extern_Entity_Declaration;

   ---------------------------
   -- Make_Struct_Aggregate --
   ---------------------------

   function Make_Struct_Aggregate
     (Defining_Identifier : Node_Id := No_Node;
      Members             : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Struct_Aggregate);
      if Defining_Identifier /= No_Node then
         Set_Defining_Identifier (N, Defining_Identifier);
      end if;
      Set_Struct_Members (N, Members);
      return N;
   end Make_Struct_Aggregate;

   --------------------------
   -- Make_Union_Aggregate --
   --------------------------

   function Make_Union_Aggregate
     (Defining_Identifier : Node_Id := No_Node;
      Members             : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Union_Aggregate);
      if Defining_Identifier /= No_Node then
         Set_Defining_Identifier (N, Defining_Identifier);
      end if;
      Set_Union_Members (N, Members);
      return N;
   end Make_Union_Aggregate;

   -------------------------
   -- Make_Enum_Aggregate --
   -------------------------

   function Make_Enum_Aggregate (Members : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Enum_Aggregate);
      Set_Enum_Members (N, Members);
      return N;
   end Make_Enum_Aggregate;

   -----------------------
   -- Make_Call_Profile --
   -----------------------

   function Make_Call_Profile
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Call_Profile);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Parameters (N, Parameters);
      return N;
   end Make_Call_Profile;

   ---------------------
   -- Make_Macro_Call --
   ---------------------

   function Make_Macro_Call
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Macro_Call);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Parameters (N, Parameters);
      return N;
   end Make_Macro_Call;

   -------------------------
   -- Make_Type_Attribute --
   -------------------------

   function Make_Type_Attribute
     (Designator : Node_Id;
      Attribute  : Attribute_Id) return Node_Id
   is
      procedure Get_Scoped_Name_String (S : Node_Id);

      ----------------------------
      -- Get_Scoped_Name_String --
      ----------------------------

      procedure Get_Scoped_Name_String (S : Node_Id) is
      begin
         Get_Name_String_And_Append (CTN.Name (Defining_Identifier (S)));
      end Get_Scoped_Name_String;

   begin
      Name_Len := 0;
      Get_Scoped_Name_String (Designator);
      Add_Char_To_Name_Buffer (''');
      Get_Name_String_And_Append (AN (Attribute));
      return Make_Defining_Identifier (Name_Find);
   end Make_Type_Attribute;

   --------------------------
   -- Make_Type_Conversion --
   --------------------------

   function Make_Type_Conversion
     (Subtype_Mark : Node_Id;
      Expression   : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Type_Conversion);
      Set_Subtype_Mark (N, Subtype_Mark);
      Set_Expression (N, Expression);
      return N;
   end Make_Type_Conversion;

   -------------------------
   -- Make_Comment_Header --
   -------------------------

   procedure Make_Comment_Header (Header : List_Id) is
      N : Node_Id;
   begin
      --  Appending the comment header lines to the file header

      Set_Str_To_Name_Buffer
        ("***************************************************");
      N := Make_C_Comment (Name_Find, False);
      Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("This file was automatically generated by Ocarina ");
      N := Make_C_Comment (Name_Find);
      Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        (SCM_Version.all);
      N := Make_C_Comment (Name_Find);
      Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("Do NOT hand-modify this file, as your            ");
      N := Make_C_Comment (Name_Find);
      Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("changes will be lost when you re-run Ocarina     ");
      N := Make_C_Comment (Name_Find);
      Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("***************************************************");
      N := Make_C_Comment (Name_Find, False);
      Append_Node_To_List (N, Header);

   end Make_Comment_Header;

   -----------------
   -- Next_N_Node --
   -----------------

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id is
      Result : Node_Id := N;
   begin
      for I in 1 .. Num loop
         Result := CTN.Next_Node (Result);
      end loop;

      return Result;
   end Next_N_Node;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : CTN.Node_Kind;
      From : Node_Id := No_Node) return List_Id
   is
      N : Node_Id;

   begin
      CTN.Entries.Increment_Last;
      N                     := CTN.Entries.Last;
      CTN.Entries.Table (N) := CTN.Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         CTN.Set_Loc (N, CTN.Loc (From));
      else
         CTN.Set_Loc (N, No_Location);
      end if;
      return List_Id (N);
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : CTN.Node_Kind;
      From : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      CTN.Entries.Increment_Last;
      N                     := CTN.Entries.Last;
      CTN.Entries.Table (N) := CTN.Default_Node;
      CTN.Set_Kind (N, Kind);

      if Present (From) then
         CTN.Set_Loc (N, AIN.Loc (From));
      else
         CTN.Set_Loc (N, No_Location);
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

      Set_Str_To_Name_Buffer (I);
      Operator_Image (Operator_Type'Pos (O)) := Name_Find;
   end New_Operator;

   ----------------
   -- Pop_Entity --
   ----------------

   procedure Pop_Entity is
   begin
      if Last > No_Depth then
         Decrement_Last;
      end if;
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   procedure Push_Entity (E : Node_Id) is
   begin
      Increment_Last;
      Table (Last).Current_Entity := E;
   end Push_Entity;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;

   begin
      C := CTN.First_Node (L);
      if C = E then
         CTN.Set_First_Node (L, CTN.Next_Node (E));
         if CTN.Last_Node (L) = E then
            CTN.Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if CTN.Next_Node (C) = E then
               CTN.Set_Next_Node (C, CTN.Next_Node (E));
               if CTN.Last_Node (L) = E then
                  CTN.Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := CTN.Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   ---------------------
   -- Set_Main_Source --
   ---------------------

   procedure Set_Main_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Main_Source (X);
   end Set_Main_Source;

   ---------------------
   -- Set_Main_Header --
   ---------------------

   procedure Set_Main_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Main_Header (X);
   end Set_Main_Header;

   ---------------
   -- To_C_Name --
   ---------------

   function To_C_Name
     (N             : Name_Id;
      Ada_Style     : Boolean := False;
      Keyword_Check : Boolean := True) return Name_Id
   is
      Name      : Name_Id;
      Test_Name : Name_Id;
      V         : Ocarina.Types.Byte;
   begin
      Get_Name_String (Normalize_Name (N, Ada_Style));
      Name := Name_Find;

      if Keyword_Check then

         --  If the identifier collides with a C reserved word insert
         --  "AADL_" string before the identifier.

         Test_Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
         V         := Get_Name_Table_Byte (Test_Name);
         if V > 0 then
            Set_Str_To_Name_Buffer ("AADL_");
            Get_Name_String_And_Append (Name);
            Name := Name_Find;
         end if;
      end if;
      return To_Lower (Name);
   end To_C_Name;

   ----------------------------
   -- Conventional_Base_Name --
   ----------------------------

   function Conventional_Base_Name (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);

      for Index in 1 .. Name_Len loop
         Name_Buffer (Index) := To_Lower (Name_Buffer (Index));
      end loop;

      return Name_Find;
   end Conventional_Base_Name;

   -------------------------
   -- Set_Activity_Source --
   -------------------------

   procedure Set_Activity_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Activity_Source (X);
   end Set_Activity_Source;

   ---------------------------
   -- Set_Deployment_Header --
   ---------------------------

   procedure Set_Deployment_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Deployment_Header (X);
   end Set_Deployment_Header;

   ---------------------------
   -- Set_Deployment_Source --
   ---------------------------

   procedure Set_Deployment_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Deployment_Source (X);
   end Set_Deployment_Source;

   -------------------------
   -- Set_Activity_Header --
   -------------------------

   procedure Set_Activity_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Activity_Header (X);
   end Set_Activity_Header;

   ------------------------
   -- Set_Request_Header --
   ------------------------

   procedure Set_Request_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Request_Header (X);
   end Set_Request_Header;

   ------------------------
   -- Set_Request_Source --
   ------------------------

   procedure Set_Request_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Request_Source (X);
   end Set_Request_Source;

   ----------------------------
   -- Set_Marshallers_Source --
   ----------------------------
   procedure Set_Marshallers_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Marshallers_Source (X);
   end Set_Marshallers_Source;

   ----------------------
   -- Set_Types_Header --
   ----------------------

   procedure Set_Types_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Types_Header (X);
   end Set_Types_Header;

   ----------------------------
   -- Set_Marshallers_Header --
   ----------------------------

   procedure Set_Marshallers_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Marshallers_Header (X);
   end Set_Marshallers_Header;

   ----------------------------
   -- Set_Subprograms_Header --
   ----------------------------

   procedure Set_Subprograms_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Subprograms_Header (X);
   end Set_Subprograms_Header;

   -----------------------
   -- Set_Naming_Header --
   -----------------------

   procedure Set_Naming_Header (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Naming_Header (X);
   end Set_Naming_Header;

   -----------------------
   -- Set_Naming_Source --
   -----------------------

   procedure Set_Naming_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Naming_Source (X);
   end Set_Naming_Source;

   ----------------------------
   -- Set_Subprograms_Source --
   ----------------------------

   procedure Set_Subprograms_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Subprograms_Source (X);
   end Set_Subprograms_Source;

   ----------------------
   -- Set_Types_Source --
   ----------------------

   procedure Set_Types_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Types_Source (X);
   end Set_Types_Source;

   --------------------
   -- Current_Entity --
   --------------------

   function Current_Entity return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_Entity;
      end if;
   end Current_Entity;

   ------------------
   -- Current_File --
   ------------------

   function Current_File return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_File;
      end if;
   end Current_File;

   ----------------------
   -- Make_Source_File --
   ----------------------

   function Make_Source_File (Identifier : Node_Id) return Node_Id is
      File : Node_Id;
   begin
      File := New_Node (K_Source_File);
      Set_Defining_Identifier (File, Identifier);
      Set_Corresponding_Node (Identifier, File);

      CTN.Set_Included_Headers (File, New_List (K_Header_List));
      CTN.Set_Declarations (File, New_List (CTN.K_Declaration_List));
      Make_Comment_Header (CTN.Declarations (File));

      return File;
   end Make_Source_File;

   ----------------------
   -- Make_Header_File --
   ----------------------

   function Make_Header_File (Identifier : Node_Id) return Node_Id is
      File : Node_Id;
   begin
      File := New_Node (K_Header_File);
      Set_Defining_Identifier (File, Identifier);
      Set_Corresponding_Node (Identifier, File);

      CTN.Set_Included_Headers (File, New_List (K_Header_List));
      CTN.Set_Declarations (File, New_List (CTN.K_Declaration_List));
      Make_Comment_Header (CTN.Declarations (File));

      return File;
   end Make_Header_File;

   -----------------
   -- Add_Include --
   -----------------

   procedure Add_Include (E : Node_Id; Preserve_Case : Boolean := False) is
      W                : Node_Id;
      N                : Name_Id;
      M                : Name_Id;
      Existing_Include : Node_Id;
   begin
      --  Get the info associated to the obtained name in the hash
      --  table and check whether it is already set to a value
      --  different from 0 (No_Node) which means that the withed
      --  entity is already in the withed package list. In this case
      --  try to enrich the exisiting with clause with eventual 'use',
      --  'elaborate' or warning disabling clauses.
      Get_Name_String (CTN.Name (Defining_Identifier (Current_File)));

      if Kind (Current_File) = K_Header_File then
         --  If the included file is the file in which we add the
         --  include, we return immediatly, because a file don't
         --  include itself
         if To_Lower (CTN.Name (CTN.Header_Name (E))) =
           To_Lower (CTN.Name (Defining_Identifier (Current_File)))
         then
            return;
         end if;

         Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len) & ".h");
      else
         Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len) & ".c");
      end if;

      Get_Name_String_And_Append (CTN.Name (CTN.Header_Name (E)));
      Get_Name_String_And_Append
        (CTN.Name (CTN.Entity (Distributed_Application_Unit (Current_File))));

      if Distributed_Application
          (Entity (Distributed_Application_Unit (Current_File))) /=
        No_Node
      then
         Get_Name_String_And_Append
           (CTN.Name
              (Distributed_Application
                 (Entity (Distributed_Application_Unit (Current_File)))));
      end if;

      if Preserve_Case then
         N := Name_Find;
      else
         N := To_Lower (Name_Find);
      end if;

      Existing_Include := Node_Id (Get_Name_Table_Info (N));

      --  If the file was already included, we return immediatly
      if Present (Existing_Include) then
         return;
      end if;

      --  Else, we add the corresponding header file to included files
      Get_Name_String (CTN.Name (Header_Name ((E))));

      M := Name_Find;
      W :=
        Make_Include_Clause
          (Make_Defining_Identifier (M, not Preserve_Case),
           Is_Local (E));
      Set_Name_Table_Info (N, Int (W));

      Append_Node_To_List (W, Included_Headers (Current_File));
   end Add_Include;

   ---------------------------
   -- Make_Define_Statement --
   ---------------------------

   function Make_Define_Statement
     (Defining_Identifier : Node_Id;
      Value               : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Define_Statement);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Defined_Value (N, Value);
      return N;
   end Make_Define_Statement;

   -----------------------
   -- Make_Pointer_Type --
   -----------------------

   function Make_Pointer_Type (Used_Type : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Pointer_Type);
      Set_Used_Type (N, Used_Type);
      return (N);
   end Make_Pointer_Type;

   ------------------------
   -- Make_Constant_Type --
   ------------------------

   function Make_Constant_Type (Used_Type : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Constant_Type);
      Set_Used_Type (N, Used_Type);
      return (N);
   end Make_Constant_Type;

   ----------------------------
   -- Make_Member_Designator --
   ----------------------------

   function Make_Member_Designator
     (Defining_Identifier : Node_Id;
      Aggregate_Name      : Node_Id;
      Is_Pointer          : Boolean := False) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Member_Designator);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Is_Pointer (N, Is_Pointer);
      Set_Aggregate_Name (N, Aggregate_Name);
      return (N);
   end Make_Member_Designator;

   ----------------------------
   -- Make_Array_Declaration --
   ----------------------------

   function Make_Array_Declaration
     (Defining_Identifier : Node_Id;
      Array_Size          : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Array_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Array_Size (N, Array_Size);
      return (N);
   end Make_Array_Declaration;

   -----------------------
   -- Make_Array_Values --
   -----------------------

   function Make_Array_Values (Values : List_Id := No_List) return Node_Id is
      L : List_Id;
      N : Node_Id;
   begin
      N := New_Node (K_Array_Values);
      if not Present (Values) then
         L := New_List (CTN.K_Enumeration_Literals);
         Set_Values (N, L);
      else
         Set_Values (N, Values);
      end if;
      return (N);
   end Make_Array_Values;

   ----------------------
   -- Make_Array_Value --
   ----------------------

   function Make_Array_Value
     (Array_Name : Node_Id;
      Array_Item : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Array_Value);
      Set_Defining_Identifier (N, Array_Name);
      Set_Array_Item (N, Array_Item);
      return (N);
   end Make_Array_Value;

   ---------------------------
   -- Make_Switch_Statement --
   ---------------------------

   function Make_Switch_Statement
     (Expression   : Node_Id;
      Alternatives : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Switch_Statement);
      Set_Expression (N, Expression);
      Set_Alternatives (N, Alternatives);
      return (N);
   end Make_Switch_Statement;

   -----------------------------
   -- Make_Switch_Alternative --
   -----------------------------

   function Make_Switch_Alternative
     (Labels     : List_Id;
      Statements : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Switch_Alternative);
      Set_Labels (N, Labels);
      Set_Statements (N, Statements);
      return (N);
   end Make_Switch_Alternative;

   --------------------------
   -- Handle_Call_Sequence --
   --------------------------

   procedure Handle_Call_Sequence
     (Caller            : Node_Id;
      Call_Seq          : Node_Id;
      Declarations      : List_Id;
      Statements        : List_Id;
      Containing_Device : Node_Id := No_Node)
   is
      Destination_F : Node_Id;
      Source_F      : Node_Id;
      Source_Parent : Node_Id;
      Param_Value   : Node_Id;
      Call_Profile  : List_Id;
      Spg           : Node_Id;
      Spg_Call      : Node_Id;
      N             : Node_Id;
      F             : Node_Id;
      M             : Node_Id;
      Declaration   : Node_Id;
      Data_Accessed : Node_Id;
      Hybrid        : constant Boolean :=
        AINU.Is_Subprogram (Caller)
        and then
          Properties.Get_Subprogram_Kind (Caller) =
          Properties.Subprogram_Hybrid_Ada_95; --  XXX why Ada ?
   begin
      --  The lists have to be created

      if Declarations = No_List or else Statements = No_List then
         raise Program_Error
           with "Lists have to be created before any call " &
           "to Handle_Call_Sequence";
      end if;

      --  The call sequence generally contains at least one call to a
      --  subprogram.

      if AINU.Is_Empty (AIN.Subprogram_Calls (Call_Seq)) then
         Display_Located_Error
           (AIN.Loc (Call_Seq),
            "Empty call sequence",
            Fatal   => False,
            Warning => True);
         return;
      end if;
      Spg_Call := AIN.First_Node (AIN.Subprogram_Calls (Call_Seq));

      while Present (Spg_Call) loop
         Spg          := AIN.Corresponding_Instance (Spg_Call);
         Call_Profile := New_List (CTN.K_List_Id);

         if not AINU.Is_Empty (AIN.Features (Spg)) then
            F := AIN.First_Node (AIN.Features (Spg));
            while Present (F) loop
               if Kind (F) = K_Subcomponent_Access_Instance then
                  --  This case is specific to POK since we don't
                  --  handle the shared data with the same patterns as
                  --  in PolyORB-HI-C. This could be updated later.
                  Data_Accessed := Get_Accessed_Data (F);

                  if Data_Accessed = No_Node then
                     Display_Located_Error
                       (AIN.Loc (F),
                        "is not properly conected to" & " any source",
                        Fatal => True);
                  end if;

                  Param_Value :=
                    Make_Variable_Address
                      (Map_C_Defining_Identifier (Data_Accessed));

                  Append_Node_To_List (Param_Value, Call_Profile);

               elsif AIN.Kind (F) = AIN.K_Parameter_Instance
                 and then AIN.Is_Out (F)
               then
                  --  Raise an error if the parameter is not connected
                  --  to any source.

                  if AINU.Length (AIN.Destinations (F)) = 0 then
                     Display_Located_Error
                       (AIN.Loc (F),
                        "This OUT parameter is not connected to" &
                        " any destination",
                        Fatal => True);
                  elsif AINU.Length (AIN.Destinations (F)) > 1 then
                     Display_Located_Error
                       (AIN.Loc (F),
                        "This OUT parameter has too many destinations",
                        Fatal => True);
                  end if;

                  --  At this point, we have a subprogram call
                  --  parameter that has exactly one destination.

                  Destination_F :=
                    AIN.Item (AIN.First_Node (AIN.Destinations (F)));

                  --  For each OUT parameter, we declare a local
                  --  variable if the OUT parameter is connected to
                  --  another subprogram call or if the caller is a
                  --  thread. Otherwise, we use the corresponding
                  --  caller subprogram parameter.

                  --  The parameter association value takes 3 possible
                  --  values (see the (1), (2) and (3) comments below.

                  if AINU.Is_Thread (Caller)
                    or else AIN.Parent_Component (Destination_F) /= Caller
                  then
                     --  (1) Here, we map the variable name from the
                     --  subprogram *call* name and the feature
                     --  name. This avoids name clashing when a thread
                     --  calls twice the same subprogram.

                     if Get_Current_Backend_Kind = PolyORB_HI_C then
                        M :=
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (F));
                        Declaration :=
                          Make_Variable_Declaration
                            (Defining_Identifier =>
                               Make_Defining_Identifier
                                 (Map_C_Variable_Name
                                    (F,
                                     Request_Variable => True)),
                             Used_Type => M,
                             Is_Static => True);

                        Append_Node_To_List (Declaration, Declarations);

                        M :=
                          Make_Defining_Identifier
                            (Map_C_Variable_Name
                               (F,
                                Request_Variable => True));

                     elsif Get_Current_Backend_Kind = PolyORB_Kernel_C then
                        M :=
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (F));

                        Declaration :=
                          Make_Variable_Declaration
                            (Defining_Identifier =>
                               Make_Defining_Identifier
                                 (Map_Port_Data (Destination_F)),
                             Used_Type => M);

                        Append_Node_To_List (Declaration, Declarations);

                        M :=
                          Make_Defining_Identifier
                            (Map_Port_Data (Destination_F));
                     end if;

                     Param_Value := Make_Variable_Address (M);

                  elsif Hybrid then
                     --  (2) If the calleD parameter is connected to
                     --      the calleR parameter and then the calleR
                     --      IS hybrid, then we use the 'Status'
                     --      record field corresponding to the calleR
                     --      parameter.

                     Param_Value :=
                       Make_Member_Designator
                         (Make_Defining_Identifier
                            (To_C_Name
                               (AIN.Display_Name (AIN.Identifier (F)))),
                          Make_Defining_Identifier (PN (P_Status)));

                  else
                     --  (3) If the calleD parameter is connected to
                     --      the calleR parameter and then then calleR
                     --      is NOT hybrid, then we use simply the
                     --      corresponding parameter of the calleR.

                     Param_Value :=
                       Make_Defining_Identifier
                         (To_C_Name
                            (AIN.Display_Name
                               (AIN.Identifier (Destination_F))));
                  end if;

                  --  For each OUT parameter we build a parameter
                  --  association of the actual profile of the
                  --  implmentaion subprogram call <Param> =>
                  --  <Param_Value>.

                  CTU.Append_Node_To_List (Param_Value, Call_Profile);

               elsif AIN.Kind (F) = AIN.K_Parameter_Instance
                 and then AIN.Is_In (F)
               then
                  --  Raise an error if the parameter is not connected
                  --  to any source.

                  if AINU.Length (AIN.Sources (F)) = 0 then
                     Display_Located_Error
                       (AIN.Loc (F),
                        "This IN parameter is not connected to" &
                        " any source",
                        Fatal => True);
                  elsif AINU.Length (AIN.Sources (F)) > 1 then
                     Display_Located_Error
                       (AIN.Loc (F),
                        "This IN parameter has too many sources",
                        Fatal => True);
                  end if;

                  --  Here we have an IN parameter with exactly one
                  --  source.

                  Source_F := AIN.Item (AIN.First_Node (AIN.Sources (F)));

                  --  Get the source feature parent

                  Source_Parent := AIN.Parent_Component (Source_F);

                  --  The parameter value of the built parameter
                  --  association can take 4 different values. (see
                  --  comments (1), (2), (3) and (4) above).

                  if AINU.Is_Thread (Source_Parent) then
                     --  (1) If the Parent of 'Source_F' is a thread,
                     --  then we use the '<Thread>_Job_Req' record
                     --  field corresponding to F.

                     if Get_Current_Backend_Kind = PolyORB_HI_C then
                        Param_Value :=
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Member_Designator
                                      (Defining_Identifier =>
                                         Make_Defining_Identifier
                                           (Map_C_Enumerator_Name (Source_F)),
                                       Aggregate_Name =>
                                         Make_Defining_Identifier
                                           (Map_C_Enumerator_Name (Source_F))),
                                  Aggregate_Name =>
                                    Make_Defining_Identifier (MN (M_Vars))),
                             Aggregate_Name =>
                               Make_Defining_Identifier
                                 (Map_C_Variable_Name
                                    (Source_F,
                                     Port_Request => True)));
                     else
                        M :=
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (F));

                        Declaration :=
                          Make_Variable_Declaration
                            (Defining_Identifier =>
                               Make_Defining_Identifier
                                 (Map_Port_Data (Source_F)),
                             Used_Type => M);

                        Append_Node_To_List (Declaration, Declarations);

                        Param_Value :=
                          Make_Defining_Identifier (Map_Port_Data (Source_F));
                     end if;

                  elsif Source_Parent /= Caller then
                     --  (2) If the the source call is different from
                     --      the englobing subprogram, we use the
                     --      formerly declared variable.

                     Param_Value :=
                       Make_Defining_Identifier
                         (Map_C_Variable_Name
                            (Source_F,
                             Request_Variable => True));

                  elsif Hybrid then
                     --  (3) If the calleD parameter is connected to
                     --      the calleR parameter then calleR IS
                     --      hybrid, then we use the 'Status' record field
                     --      corresponding to the calleR parameter.

                     Param_Value :=
                       Make_Member_Designator
                         (Make_Defining_Identifier
                            (To_C_Name
                               (AIN.Display_Name (AIN.Identifier (Source_F)))),
                          Make_Defining_Identifier (PN (P_Status)));
                  else
                     --  (4) If the calleD parameter is connected to
                     --      the calleR parameter and then then calleR
                     --      is NOT hybrid, then we use simply the
                     --      corresponding paremeter of the calleR.

                     Param_Value :=
                       Make_Defining_Identifier
                         (To_C_Name
                            (AIN.Display_Name (AIN.Identifier (Source_F))));
                  end if;

                  --  For each IN parameter we build a parameter
                  --  association association of the actual profile of
                  --  the implmentaion subprogram call <Param> =>
                  --  <Param_Value>.

                  CTU.Append_Node_To_List (Param_Value, Call_Profile);
               end if;

               F := AIN.Next_Node (F);
            end loop;

         end if;

         if not AINU.Is_Empty (Path (Spg_Call)) then
            --  If this is a feature subprogram call, generate a call
            --  to the corresponding method.  For this moment, we
            --  simply handle protected objects

            N := Message_Comment ("Invoking method");
            CTU.Append_Node_To_List (N, Statements);
            --  The name of the called subprogram is deduced from the
            --  corresponding subprogram spec instance (last element
            --  of the 'Path' list) and from the actual data component
            --  instance the call is connected to.

            N :=
              Map_C_Feature_Subprogram
                (Item (AIN.Last_Node (Path (Spg_Call))),
                 Corresponding_Instance (Get_Actual_Owner (Spg_Call)));

            N := Make_Call_Profile (N, Call_Profile);
            CTU.Append_Node_To_List (N, Statements);

         else
            --  If this is a classic subprogram, call its
            --  implementation.

            if Get_Current_Backend_Kind = PolyORB_HI_C then
               Add_Include (PHCR.RH (PHCR.RH_Subprograms));
            elsif Get_Current_Backend_Kind = PolyORB_Kernel_C then
               Add_Include (PKR.RH (PKR.RH_Subprograms));
            end if;

            N := Message_Comment ("Call implementation");
            CTU.Append_Node_To_List (N, Statements);

            if Get_Current_Backend_Kind = PolyORB_HI_C then
               N := Map_C_Defining_Identifier (Spg);
               if Containing_Device /= No_Node then
                  CTU.Append_Node_To_List
                    (Make_Defining_Identifier
                       (Map_C_Enumerator_Name (Containing_Device)),
                     Call_Profile);
               end if;
            elsif Get_Current_Backend_Kind = PolyORB_Kernel_C then
               N := Map_C_Defining_Identifier (Spg);
            end if;
            N := Make_Call_Profile (N, Call_Profile);
            CTU.Append_Node_To_List (N, Statements);
         end if;

         Spg_Call := AIN.Next_Node (Spg_Call);
      end loop;
   end Handle_Call_Sequence;

   -------------------------
   -- Get_C_Default_Value --
   -------------------------

   function Get_C_Default_Value (D : Node_Id) return Node_Id is
      Data_Representation : Supported_Data_Representation;
      Result              : Node_Id;
   begin
      pragma Assert (AINU.Is_Data (D));

      Data_Representation := Get_Data_Representation (D);

      case Data_Representation is
         when Data_Integer =>
            --  For integers, default value is 0

            Result := CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));

         when Data_Float | Data_Fixed =>
            --  For reals, the default value is 0.0

            Result := CTU.Make_Literal (CV.New_Floating_Point_Value (0.0));

         when Data_Boolean =>
            --  For booleans, the default value is FALSE

            Result := CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));

         when Data_Character =>
            --  For characters, the default value is the space ' '

            Result :=
              CTU.Make_Literal (CV.New_Char_Value (Character'Pos (' ')));

         when Data_Wide_Character =>
            --  For wide characters, the default value is the wide
            --  space ' '.

            Result :=
              CTU.Make_Literal (CV.New_Char_Value (Character'Pos (' ')));

         when Data_String =>
            Display_Located_Error
              (AIN.Loc (D),
               "Bounded strings default values not supported yet!",
               Fatal => True);

         when Data_Wide_String =>
            Display_Located_Error
              (AIN.Loc (D),
               "Bounded wide strings default values not supported yet!",
               Fatal => True);

         when Data_Array =>
            Display_Located_Error
              (AIN.Loc (D),
               "Bounded arrays default values not supported yet!",
               Fatal => True);

         when Data_With_Accessors =>
            --  This is definitely a code generation error

            raise Program_Error
              with "Data types with accessors should" &
              " not have default values";

         when others =>
            raise Program_Error with "Unsupported data type default value!";

      end case;

      return Result;
   end Get_C_Default_Value;

   -------------------------
   -- Make_Include_Clause --
   -------------------------

   function Make_Include_Clause
     (Header_Name : Node_Id;
      Local       : Boolean := False) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Include_Clause);
      Set_Header_Name (N, Header_Name);
      Set_Is_Local (N, Local);

      return N;
   end Make_Include_Clause;

   ---------------------------
   -- Add_Define_Deployment --
   ---------------------------

   procedure Add_Define_Deployment (E : Node_Id) is
      W            : Node_Id;
      N            : Name_Id;
      F            : Node_Id;
      Existing_Def : Node_Id;
   begin
      Set_Str_To_Name_Buffer ("deployment");
      Get_Name_String_And_Append (CTN.Name (E));

      Get_Name_String_And_Append
        (CTN.Name (CTN.Entity (Table (Last).Current_Entity)));
      N := Name_Find;

      Existing_Def := Node_Id (Get_Name_Table_Info (N));

      --  If the file was already included, we return immediatly
      if Present (Existing_Def) then
         return;
      end if;

      --  Else, we add the corresponding header file to included files
      W :=
        CTU.Make_Define_Statement
          (Defining_Identifier => Copy_Node (E),
           Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

      Set_Name_Table_Info (N, Int (W));

      F                         := Table (Last).Current_File;
      Table (Last).Current_File :=
        Deployment_Header (Table (Last).Current_Entity);
      Append_Node_To_List (W, CTN.Declarations (Current_File));
      Table (Last).Current_File := F;
   end Add_Define_Deployment;

   --------------------------
   -- Add_Return_Assertion --
   --------------------------

   procedure POK_Add_Return_Assertion
     (Statements      : List_Id;
      Exception_Error : Node_Id := No_Node)
   is
   begin
      if not POK_C.Add_Assertions then
         return;
      end if;

      if Exception_Error = No_Node then
         Append_Node_To_List
           (Make_Macro_Call
              (PKR.RE (PKR.RE_Assert_Ret),
               Make_List_Id (Make_Defining_Identifier (VN (V_Ret)))),
            Statements);
      else
         Append_Node_To_List
           (Make_Macro_Call
              (PKR.RE (PKR.RE_Assert_Ret_With_Exception),
               Make_List_Id
                 (Make_Defining_Identifier (VN (V_Ret)),
                  Exception_Error)),
            Statements);
      end if;
   end POK_Add_Return_Assertion;

   ----------------------------------------
   -- POK_Make_Function_Call_With_Assert --
   ----------------------------------------

   function POK_Make_Function_Call_With_Assert
     (Function_Name : Node_Id;
      Parameters    : List_Id) return Node_Id
   is
      use Ocarina.Backends.POK_C;
      Function_Call : Node_Id;
   begin
      Function_Call := Make_Call_Profile (Function_Name, Parameters);
      if POK_C.Add_Assertions and then POK_Flavor = POK then
         return Make_Expression
             (Make_Defining_Identifier (VN (V_Ret)),
              Op_Equal,
              Function_Call);
      else
         return Function_Call;
      end if;
   end POK_Make_Function_Call_With_Assert;

   -------------------
   -- Get_Data_Size --
   -------------------

   function Get_Data_Size (Data : Node_Id;
                           Is_Pointer : Boolean := False;
                           Maximum_Size : Boolean := False)
                          return Node_Id
   is
      Data_Representation : Supported_Data_Representation;
      Value_UUL           : Unsigned_Long_Long;
      Value_Node          : Node_Id            := No_Node;
      Dimension           : constant ULL_Array := Get_Dimension (Data);
      Type_Size           : Size_Type;
   begin
      pragma Assert (AINU.Is_Data (Data));

      Data_Representation := Get_Data_Representation (Data);
      Type_Size           := Get_Data_Size (Data);

      if Get_Data_Size (Data) /= Null_Size then
         Value_UUL := To_Bytes (Type_Size);
         return (Make_Literal (New_Int_Value (Value_UUL, 1, 10)));
      end if;

      if Is_Defined_Property (Data, "type_source_name") then
         return Make_Call_Profile
             (Make_Defining_Identifier (FN (F_Sizeof)),
              Make_List_Id
                (Make_Defining_Identifier
                   (To_C_Name
                      (Get_String_Property (Data, "type_source_name")))));
      end if;

      case Data_Representation is
         when Data_Integer | Data_Boolean =>
            Value_Node :=
              Make_Call_Profile
                (Make_Defining_Identifier (FN (F_Sizeof)),
                 Make_List_Id (Make_Defining_Identifier (TN (T_Int))));

         when Data_Float =>
            Value_Node :=
              Make_Call_Profile
                (Make_Defining_Identifier (FN (F_Sizeof)),
                 Make_List_Id (Make_Defining_Identifier (TN (T_Float))));

         when Data_String | Data_Wide_String =>
            Value_UUL := Dimension (1);

         when Data_Array =>
            Value_Node :=
              Make_Expression
                (Left_Expr =>
                   Make_Literal (New_Int_Value (Dimension (1), 1, 10)),
                 Operator   => Op_Asterisk,
                 Right_Expr =>
                   Get_Data_Size
                     (ATN.Entity (ATN.First_Node (Get_Base_Type (Data)))));

         when Data_Bounded_Array =>
            if Maximum_Size then
               Value_Node :=
                 Make_Expression
                 (Left_Expr =>
                    Make_Literal (New_Int_Value (Dimension (1), 1, 10)),
                  Operator   => Op_Asterisk,
                  Right_Expr =>
                    Get_Data_Size
                    (ATN.Entity (ATN.First_Node (Get_Base_Type (Data)))));

            else
               Value_Node :=
                 Make_Expression
                 (Left_Expr =>
                    Make_Member_Designator
                    (Defining_Identifier =>
                       Make_Defining_Identifier (MN (M_length)),
                     Aggregate_Name =>
                       Make_Defining_Identifier (PN (P_Value)),
                     Is_Pointer => Is_Pointer),
                  Operator   => Op_Asterisk,
                  Right_Expr =>
                    Get_Data_Size
                    (ATN.Entity (ATN.First_Node (Get_Base_Type (Data)))));
            end if;

         when Data_None =>
            Value_Node :=
              Make_Call_Profile
                (Make_Defining_Identifier (FN (F_Sizeof)),
                 Make_List_Id (Map_C_Defining_Identifier (Data)));

         when others =>
            Value_Node :=
              Make_Call_Profile
                (Make_Defining_Identifier (FN (F_Sizeof)),
                 Make_List_Id (Map_C_Defining_Identifier (Data)));
      end case;

      if Value_Node /= No_Node then
         return Value_Node;
      else
         raise Program_Error
           with "Impossible to get the data size of this data";
      end if;
   end Get_Data_Size;

   ---------------------------------
   -- Add_Return_Variable_In_List --
   ---------------------------------

   procedure Add_Return_Variable_In_Parameters (Parameters : List_Id) is
   begin
      Append_Node_To_List
        (Make_Variable_Address (Make_Defining_Identifier (VN (V_Ret))),
         Parameters);
   end Add_Return_Variable_In_Parameters;

   -----------------------------------------------------
   -- Declare_Return_Variable_In_Function_Declaration --
   -----------------------------------------------------

   procedure POK_Declare_Return_Variable (Declarations : List_Id) is
      use Ocarina.Backends.POK_C;
      use Ocarina.Backends.POK_C.Runtime;
      N : Node_Id;
   begin
      if POK_C.Add_Assertions and then POK_Flavor = POK then
         N :=
           Make_Variable_Declaration
             (Defining_Identifier => Make_Defining_Identifier (VN (V_Ret)),
              Used_Type           => RE (RE_Pok_Ret_T));
         Append_Node_To_List (N, Declarations);
      elsif Use_ARINC653_API then
         N :=
           Make_Variable_Declaration
             (Defining_Identifier => Make_Defining_Identifier (VN (V_Ret)),
              Used_Type           => RE (RE_Return_Code_Type));
         Append_Node_To_List (N, Declarations);
      end if;
   end POK_Declare_Return_Variable;

   -------------------------
   --  Make_Ifdef_Clause  --
   -------------------------

   function Make_Ifdef_Clause
     (Clause          : Node_Id;
      Negation        : Boolean := False;
      Then_Statements : List_Id;
      Else_Statements : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Ifdef_Clause);
      CTN.Set_Negation (N, Negation);
      CTN.Set_Clause (N, Clause);
      CTN.Set_Then_Statements (N, Then_Statements);
      CTN.Set_Else_Statements (N, Else_Statements);
      return N;
   end Make_Ifdef_Clause;

   -------------------------------------
   --  Get_Inter_Partition_Port_Size  --
   -------------------------------------

   function Get_Inter_Partition_Port_Size (Port : Node_Id) return Node_Id is

      Type_Found : Node_Id;

      function Get_Inter_Partition_Port_Size_Rec
        (Port   : Node_Id;
         Method : Browsing_Kind) return Node_Id
      is
         Source_Port      : Node_Id;
         Destination_Port : Node_Id;
         Tmp              : Node_Id;
         Associated_Type  : Node_Id;
      begin

         Associated_Type :=
           Get_Instance_Type_Associated_With_Virtual_Bus (Port);

         if Associated_Type /= No_Node then
            return CTU.Get_Data_Size (Associated_Type);
         end if;

         if AINU.Is_Thread (Parent_Component (Port)) then
            return Get_Data_Size (Corresponding_Instance (Port));
         end if;

         --  We are at the thread level so we are in the applicative domain.
         --  no virtual bus was found so we fallback to the thread application
         --  data.

         if Method = By_Source
           and then not AINU.Is_Empty (AIN.Sources (Port))
         then
            Tmp := AIN.First_Node (AIN.Sources (Port));

            while Present (Tmp) loop
               Source_Port := AIN.Item (Tmp);

               Associated_Type :=
                 Get_Inter_Partition_Port_Size_Rec (Source_Port, Method);

               if Associated_Type /= No_Node then
                  return Associated_Type;
               end if;

               Tmp := AIN.Next_Node (Tmp);

            end loop;
         end if;

         if Method = By_Destination
           and then not AINU.Is_Empty (AIN.Destinations (Port))
         then
            Tmp := AIN.First_Node (AIN.Destinations (Port));

            while Present (Tmp) loop
               Destination_Port := AIN.Item (Tmp);

               Associated_Type :=
                 Get_Inter_Partition_Port_Size_Rec (Destination_Port, Method);

               if Associated_Type /= No_Node then
                  return Associated_Type;
               end if;

               Tmp := AIN.Next_Node (Tmp);

            end loop;
         end if;

         return No_Node;

      end Get_Inter_Partition_Port_Size_Rec;

   begin
      if not AIN.Is_Data (Port) then
         raise Program_Error
           with "Call to Get_Inter_Partition_Port_Size with non DATA port";
      end if;

      if AIN.Is_In (Port) then
         Type_Found := Get_Inter_Partition_Port_Size_Rec (Port, By_Source);
      else
         Type_Found :=
           Get_Inter_Partition_Port_Size_Rec (Port, By_Destination);
      end if;

      if Type_Found = No_Node then
         return Get_Data_Size (Corresponding_Instance (Port));
      end if;

      return Type_Found;
   end Get_Inter_Partition_Port_Size;

   -------------------------------------
   --  Get_Inter_Partition_Port_Type  --
   -------------------------------------

   function Get_Inter_Partition_Port_Type (Port : Node_Id) return Node_Id is

      Type_Found : Node_Id;

      function Get_Inter_Partition_Port_Type_Rec
        (Port   : Node_Id;
         Method : Browsing_Kind) return Node_Id
      is
         Source_Port      : Node_Id;
         Destination_Port : Node_Id;
         Tmp              : Node_Id;
         Associated_Type  : Node_Id;
      begin

         Associated_Type :=
           Get_Instance_Type_Associated_With_Virtual_Bus (Port);

         if Associated_Type /= No_Node then
            if Is_Defined_Property (Associated_Type, "type_source_name") then
               return Make_Defining_Identifier
                   (To_C_Name
                      (Get_String_Property
                         (Associated_Type,
                          "type_source_name")));
            else
               return Map_C_Data_Type_Designator (Associated_Type);
            end if;
         end if;

         if AINU.Is_Thread (Parent_Component (Port)) then
            return No_Node;
         end if;

         --  We are at the thread level so we are in the applicative domain.
         --  no virtual bus was found so we fallback to the thread application
         --  data.

         if Method = By_Source
           and then not AINU.Is_Empty (AIN.Sources (Port))
         then
            Tmp := AIN.First_Node (AIN.Sources (Port));

            while Present (Tmp) loop
               Source_Port := AIN.Item (Tmp);

               Associated_Type :=
                 Get_Inter_Partition_Port_Type_Rec (Source_Port, Method);

               if Associated_Type /= No_Node then
                  return Associated_Type;
               end if;

               Tmp := AIN.Next_Node (Tmp);

            end loop;
         end if;

         if Method = By_Destination
           and then not AINU.Is_Empty (AIN.Destinations (Port))
         then
            Tmp := AIN.First_Node (AIN.Destinations (Port));

            while Present (Tmp) loop
               Destination_Port := AIN.Item (Tmp);

               Associated_Type :=
                 Get_Inter_Partition_Port_Type_Rec (Destination_Port, Method);

               if Associated_Type /= No_Node then
                  return Associated_Type;
               end if;

               Tmp := AIN.Next_Node (Tmp);

            end loop;
         end if;

         return No_Node;

      end Get_Inter_Partition_Port_Type_Rec;

   begin
      if not AIN.Is_Data (Port) then
         raise Program_Error
           with "Call to Get_Inter_Partition_Port_Type with non DATA port";
      end if;

      Type_Found := Get_Inter_Partition_Port_Type_Rec (Port, By_Source);

      if Type_Found /= No_Node then
         return Type_Found;
      end if;

      Type_Found := Get_Inter_Partition_Port_Type_Rec (Port, By_Destination);

      if Type_Found /= No_Node then
         return Type_Found;
      end if;

      return Map_C_Data_Type_Designator (Corresponding_Instance (Port));
   end Get_Inter_Partition_Port_Type;

   ----------------------------
   -- Make_Doxygen_C_Comment --
   ----------------------------

   function Make_Doxygen_C_Comment
     (Desc              : String;
      Brief             : String  := "";
      Element_Name      : String  := "";
      Is_Struct         : Boolean := False;
      Is_Union          : Boolean := False;
      Is_Enum           : Boolean := False;
      Is_Function       : Boolean := False;
      Is_Variable       : Boolean := False;
      Is_Define         : Boolean := False;
      Is_Typedef        : Boolean := False;
      Is_File           : Boolean := False;
      Is_Namespace      : Boolean := False;
      Is_Package        : Boolean := False;
      Is_Interface      : Boolean := False;
      Has_Header_Spaces : Boolean := True) return Node_Id
   is
      C : Node_Id;
   begin
      C := New_Node (K_Doxygen_C_Comment);
      CTN.Set_Summary (C, No_Node);
      CTN.Set_Element (C, No_Node);
      CTN.Set_Description (C, No_Node);

      CTN.Set_For_Struct (C, False);
      CTN.Set_For_Union (C, False);
      CTN.Set_For_Enum (C, False);
      CTN.Set_For_Function (C, False);
      CTN.Set_For_Variable (C, False);
      CTN.Set_For_Define (C, False);
      CTN.Set_For_Typedef (C, False);
      CTN.Set_For_File (C, False);
      CTN.Set_For_Namespace (C, False);
      CTN.Set_For_Package (C, False);
      CTN.Set_For_Interface (C, False);
      CTN.Set_Has_Header_Spaces (C, Has_Header_Spaces);

      if Desc /= "" then
         Set_Description (C, New_Node (K_Defining_Identifier));
         CTN.Set_Name (Description (C), Get_String_Name (Desc));
      end if;

      if Element_Name /= "" then
         Set_Element (C, New_Node (K_Defining_Identifier));
         CTN.Set_Name (Element (C), Get_String_Name (Element_Name));
      end if;

      if Brief /= "" then
         Set_Summary (C, New_Node (K_Defining_Identifier));
         CTN.Set_Name (Summary (C), Get_String_Name (Brief));
      end if;

      CTN.Set_For_Struct (C, Is_Struct);
      CTN.Set_For_Union (C, Is_Union);
      CTN.Set_For_Enum (C, Is_Enum);
      CTN.Set_For_Function (C, Is_Function);
      CTN.Set_For_Variable (C, Is_Variable);
      CTN.Set_For_Define (C, Is_Define);
      CTN.Set_For_Typedef (C, Is_Typedef);
      CTN.Set_For_File (C, Is_File);
      CTN.Set_For_Namespace (C, Is_Namespace);
      CTN.Set_For_Package (C, Is_Package);
      CTN.Set_For_Interface (C, Is_Interface);

      return C;
   end Make_Doxygen_C_Comment;

end Ocarina.Backends.C_Tree.Nutils;
