------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . R T S J _ T R E E . N U T I L S     --
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

with GNAT.Table;

with Charset;   use Charset;
with Locations; use Locations;
with Ocarina.Namet;     use Ocarina.Namet;
with Utils;     use Utils;

with Ocarina.Backends.Utils; use Ocarina.Backends.Utils;

with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Backends.RTSJ_Tree.Nutils is

   package AIN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;

   Keyword_Suffix : constant String := "%RTSJ";
   --  Used to mark Java keywords and avoid collision with other languages

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

      --  Keywords
      for I in Keyword_Type loop
         New_Token (I);
      end loop;

      --  Graphic Characters
      New_Token (Tok_And, "&&");
      New_Token (Tok_Xor, "^");
      New_Token (Tok_Or, "||");
      New_Token (Tok_Left_Brace, "{");
      New_Token (Tok_Right_Brace, "}");
      New_Token (Tok_Mod, "%");
      New_Token (Tok_Not, "!");
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
      New_Token (Tok_Greater, ">");
      New_Token (Tok_Not_Equal, "/=");
      New_Token (Tok_Greater_Equal, ">=");
      New_Token (Tok_Less_Equal, "<=");
      New_Token (Tok_Colon, ":");
      New_Token (Tok_Greater_Greater, ">>");
      New_Token (Tok_Less_Less, "<<");
      New_Token (Tok_Quote, """");
      New_Token (Tok_Semicolon, ";");
      New_Token (Tok_Vertical_Bar, "|");

      for O in Op_And .. Op_Or loop
         New_Operator (O);
      end loop;

      New_Operator (Op_Minus, "-");
      New_Operator (Op_Plus, "+");
      New_Operator (Op_Plus_Plus, "++");
      New_Operator (Op_Mult, "*");
      New_Operator (Op_Slash, "/");
      New_Operator (Op_Less, "<");
      New_Operator (Op_Equal, "=");
      New_Operator (Op_Equal_Equal, "==");
      New_Operator (Op_Greater, ">");
      New_Operator (Op_Not_Equal, "/=");
      New_Operator (Op_Greater_Equal, ">=");
      New_Operator (Op_Less_Equal, "<=");
      New_Operator (Op_Greater_Greater, ">>");
      New_Operator (Op_Less_Less, "<<");
      New_Operator (Op_Semicolon, ";");
      New_Operator (Op_Vertical_Bar, "|");

      --  Format of a variable : myVar
      for V in Variable_Id loop
         Set_Str_To_Name_Buffer (Variable_Id'Image (V));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         VN (V) := To_RTSJ_Conventional_Name (Name_Find, False);
      end loop;

      --  Format of a method : myMethod
      for M in Method_Id loop
         Set_Str_To_Name_Buffer (Method_Id'Image (M));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         MN (M) := To_RTSJ_Conventional_Name (Name_Find, False);
      end loop;

      --  Format of an object : MyObject
      for O in Object_Id loop
         Set_Str_To_Name_Buffer (Object_Id'Image (O));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         ON (O) := To_RTSJ_Conventional_Name (Name_Find, True);
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
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------
   function Length (L : List_Id) return Natural is
      N   : Node_Id;
      Cpt : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := First_Node (L);

         while Present (N) loop
            Cpt := Cpt + 1;
            N   := Next_Node (N);
         end loop;
      end if;

      return Cpt;
   end Length;

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

   -----------------
   --  Add_Import --
   -----------------
   procedure Add_Import (E : Node_Id) is
      M                : Name_Id;
      N                : Name_Id;
      W                : Node_Id;
      Existing_Include : Node_Id;
   begin
      Get_Name_String (RTN.Name (Defining_Identifier (Current_File)));

      Get_Name_String_And_Append (RTN.Name (Import_Name (E)));
      Get_Name_String_And_Append
        (RTN.Name (RTN.Entity (Distributed_Application_Unit (Current_File))));

      if Distributed_Application
          (Entity (Distributed_Application_Unit (Current_File))) /=
        No_Node
      then
         Get_Name_String_And_Append
           (RTN.Name
              (Distributed_Application
                 (Entity (Distributed_Application_Unit (Current_File)))));
      end if;

      N := Name_Find;

      Existing_Include := Node_Id (Get_Name_Table_Info (N));

      --  If the file was already included, we return immediatly
      if Present (Existing_Include) then
         return;
      end if;

      --  Else, we add the corresponding header file to included files
      Get_Name_String (RTN.Name (Import_Name ((E))));

      M := Name_Find;
      W := Make_Import_Statement (Make_Defining_Identifier (M));
      Set_Name_Table_Info (N, Int (W));

      RTU.Append_Node_To_List (W, Imported_Headers (Current_File));
   end Add_Import;

   -------------------------------
   -- To_RTSJ_Conventional_Name --
   -------------------------------
   function To_RTSJ_Conventional_Name
     (Name   : Name_Id;
      Is_Obj : Boolean) return Name_Id
   is
   begin
      Get_Name_String (Name);

      for I in 1 .. Name_Len loop
         --  Put the first letter upper for an object
         --  or lower for variables and methods

         if Is_Obj = False and then I = 1 then
            Name_Buffer (I) := To_Lower (Name_Buffer (I));
         elsif Is_Obj /= False and then I = 1 then
            Name_Buffer (I) := To_Upper (Name_Buffer (I));
         end if;

         if Name_Buffer (I) = '_' then
            Name_Buffer (I + 1)             := To_Upper (Name_Buffer (I + 1));
            Name_Buffer (I .. Name_Len - 1) := Name_Buffer (I + 1 .. Name_Len);
            Name_Len                        := Name_Len - 1;
         elsif I /= 1 then
            Name_Buffer (I) := To_Lower (Name_Buffer (I));
         end if;
      end loop;

      return Name_Find;
   end To_RTSJ_Conventional_Name;

   ----------------------------
   -- Conventional_Base_Name --
   ----------------------------
   function Conventional_Base_Name (M : Name_Id) return Name_Id is
   begin
      Get_Name_String (M);

      for I in 1 .. Name_Len loop
         Name_Buffer (I) := To_Lower (Name_Buffer (I));
      end loop;

      return Name_Find;
   end Conventional_Base_Name;

   -----------
   -- Image --
   -----------
   function Image (T : Token_Type) return String is
      S : String := Token_Type'Image (T);
   begin
      To_Lower (S);
      return S (5 .. S'Last);
   end Image;

   ---------------
   -- New Token --
   ---------------
   procedure New_Token (T : Token_Type; I : String := "") is
      Name : Name_Id;
   begin
      if T in Keyword_Type then
         Set_Str_To_Name_Buffer (Image (T));
         Name := Name_Find;
         Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
         Set_Name_Table_Byte
           (Name, Ocarina.Types.Byte (Token_Type'Pos (T) + 1));

         Set_Str_To_Name_Buffer (Image (T));
      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Token_Image (T) := Name_Find;
   end New_Token;

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

   ------------------
   -- New Operator --
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

   -------------------------
   -- Append_Node_To_List --
   -------------------------
   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;
   begin
      Last := RTN.Last_Node (L);
      if No (Last) then
         RTN.Set_First_Node (L, E);
      else
         RTN.Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         RTN.Set_Last_Node (L, Last);
         Last := RTN.Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------
   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;
   begin
      C := RTN.First_Node (L);
      if C = E then
         RTN.Set_First_Node (L, RTN.Next_Node (E));
         if RTN.Last_Node (L) = E then
            RTN.Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if RTN.Next_Node (C) = E then
               RTN.Set_Next_Node (C, RTN.Next_Node (E));
               if RTN.Last_Node (L) = E then
                  RTN.Set_Last_Node (L, No_Node);
               end if;
               exit;
            end if;
            C := RTN.Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   ---------------
   -- Copy_Node --
   ---------------
   function Copy_Node (N : Node_Id) return Node_Id is
      Res : Node_Id;
   begin
      case RTN.Kind (N) is

         when K_Defining_Identifier =>
            Res := New_Node (K_Defining_Identifier);
            RTN.Set_Name (Res, RTN.Name (N));
            RTN.Set_Corresponding_Node (Res, RTN.Corresponding_Node (N));

         when K_Literal =>
            Res := New_Node (K_Literal);
            RTN.Set_Value (Res, RTN.Value (N));

         when K_Import_Statement =>
            Res := New_Node (K_Import_Statement);
            RTN.Set_Import_Name (Res, RTU.Copy_Node (Import_Name (N)));

         when others =>
            raise Program_Error;

      end case;

      return Res;
   end Copy_Node;

   -----------------
   -- Push_Entity --
   -----------------
   procedure Push_Entity (E : Node_Id) is
   begin
      --  Add a new entity in the table
      Increment_Last;
      Table (Last).Current_Entity := E;
   end Push_Entity;

   ----------------
   -- Pop_Entity --
   ----------------
   procedure Pop_Entity is
   begin
      --  Decrements the indice of the table
      if Last > No_Depth then
         Decrement_Last;
      end if;
   end Pop_Entity;

   --------------
   -- New_Node --
   --------------
   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      --  Increments indice of the table
      Entries.Increment_Last;

      --  Last entry of the table
      Result_Node := Entries.Last;

      --  Add a new entry with kind Default_Node
      Entries.Table (Result_Node) := Default_Node;

      --  Change the kind of the node added
      Set_Kind (Result_Node, Kind);

      if Present (From) then
         Set_Loc (Result_Node, AIN.Loc (From));
      else
         Set_Loc (Result_Node, No_Location);
      end if;

      return Result_Node;
   end New_Node;

   --------------
   -- New_List --
   --------------
   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return List_Id
   is
      N : Node_Id;
   begin
      --  Increments indice of the table
      Entries.Increment_Last;

      --  Last entry of the table
      N := Entries.Last;

      --  Add a new entry with kind default_Node
      Entries.Table (N) := Default_Node;

      --  Change the kind of the node added
      Set_Kind (N, Kind);

      if Present (From) then
         Set_Loc (N, Loc (From));
      else
         Set_Loc (N, No_Location);
      end if;

      return List_Id (N);
   end New_List;

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

   --------------------------------
   -- Set_Generated_Types_Source --
   --------------------------------
   procedure Set_Generated_Types_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Generated_Types_Source (X);
   end Set_Generated_Types_Source;

   -------------------------------------
   -- Set_Transport_High_Level_Source --
   -------------------------------------
   procedure Set_Transport_High_Level_Source (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_File := Transport_High_Level_Source (X);
   end Set_Transport_High_Level_Source;

   ---------------------
   -- Message_Comment --
   ---------------------
   function Message_Comment (M : String) return Node_Id is
      Result_Node : Node_Id;
   begin
      Set_Str_To_Name_Buffer (M);
      Result_Node := Make_Java_Comment (Name_Find);
      return Result_Node;
   end Message_Comment;

   ------------------
   -- To_RTSJ_Name --
   ------------------
   function To_RTSJ_Name (N : Name_Id) return Name_Id is
      Name      : Name_Id;
      Test_Name : Name_Id;
      V         : Ocarina.Types.Byte;
   begin
      Get_Name_String (Normalize_Name (N));
      Name := Name_Find;

      --  If the identifier collides with a Java reserved word,
      --  insert "AADL_" string beofre the identifier.
      Test_Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
      V         := Get_Name_Table_Byte (Test_Name);
      if V > 0 then
         Set_Str_To_Name_Buffer ("AADL_");
         Get_Name_String_And_Append (Name);
         Name := Name_Find;
      end if;

      return To_Lower (Name);
   end To_RTSJ_Name;

   -------------------------------
   -- Make_Assignment_Statement --
   -------------------------------
   function Make_Assignment_Statement
     (Defining_Identifier : Node_Id;
      Expression          : Node_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Assignment_Statement);

      Set_Defining_Identifier (Result_Node, Defining_Identifier);

      --  Expression assigned to the variable
      Set_Expression (Result_Node, Expression);

      return Result_Node;
   end Make_Assignment_Statement;

   -------------------------------
   -- Make_Variable_Declaration --
   -------------------------------
   function Make_Variable_Declaration
     (Visibility          : List_Id := No_List;
      Used_Type           : Node_Id;
      Defining_Identifier : Node_Id;
      Value               : Node_Id := No_Node) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Variable_Declaration);
      Set_Visibility (Result_Node, Visibility);
      Set_Used_Type (Result_Node, Used_Type);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);
      Set_Value (Result_Node, Value);

      return Result_Node;
   end Make_Variable_Declaration;

   ---------------------------
   -- Make_Return_Statement --
   ---------------------------
   function Make_Return_Statement (Expression : Node_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Return_Statement);

      --  Add expression for the return statement
      Set_Expression (Result_Node, Expression);

      return Result_Node;
   end Make_Return_Statement;

   ---------------------------
   -- Make_Null_Statement --
   ---------------------------
   function Make_Null_Statement return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Null_Statement);

      return Result_Node;
   end Make_Null_Statement;

   ------------------------
   -- Make_Try_Statement --
   ------------------------
   function Make_Try_Statement
     (Statements       : List_Id;
      Catch_Statements : List_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Try_Statement);

      --  Add statements
      Set_Statements (Result_Node, Statements);

      --  Add catch statements
      Set_Catch_Statements (Result_Node, Catch_Statements);

      return Result_Node;
   end Make_Try_Statement;

   --------------------------
   -- Make_Catch_Statement --
   --------------------------
   function Make_Catch_Statement
     (Exception_Caught : Node_Id;
      Statements       : List_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Catch_Statement);

      --  Add exception caught
      Set_Exception_Caught (Result_Node, Exception_Caught);

      --  Add statements
      Set_Statements (Result_Node, Statements);

      return Result_Node;
   end Make_Catch_Statement;

   ----------------------------
   -- Make_Package_Statement --
   ----------------------------
   function Make_Package_Statement
     (Defining_Identifier : Node_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Package_Statement);

      --  Identifier of the package
      Set_Defining_Identifier (Result_Node, Defining_Identifier);

      return Result_Node;
   end Make_Package_Statement;

   ---------------------------
   -- Make_Import_Statement --
   --------------------------
   function Make_Import_Statement (Import_Name : Node_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Import_Statement);

      --  Name of the package imported
      Set_Import_Name (Result_Node, Import_Name);

      return Result_Node;
   end Make_Import_Statement;

   --------------------------
   -- Make_Class_Statement --
   --------------------------
   function Make_Class_Statement
     (Visibility          : List_Id := No_List;
      Defining_Identifier : Node_Id;
      Extends             : Node_Id := No_Node;
      Implements          : List_Id := No_List;
      Throws              : List_Id := No_List;
      Attributes          : List_Id := No_List;
      Methods             : List_Id := No_List;
      Classes             : List_Id := No_List) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Class_Statement);

      --  Visibility (public, private,...)
      Set_Visibility (Result_Node, Visibility);

      --  Identifier of the Java class
      Set_Defining_Identifier (Result_Node, Defining_Identifier);

      --  Classes inherited
      Set_Extends_Statement (Result_Node, Extends);

      --  Interfaces implemented
      Set_Implements_Statement (Result_Node, Implements);

      --  Throws statement for exceptions caught by the class
      Set_Throws_Statement (Result_Node, Throws);

      --  Body of the class : attributes
      Set_Attributes (Result_Node, Attributes);

      --  Body of the class : methods
      Set_Methods (Result_Node, Methods);

      --  Classes encapsulated
      Set_Classes (Result_Node, Classes);

      return Result_Node;
   end Make_Class_Statement;

   -----------------------
   -- Make_Java_Comment --
   -----------------------
   function Make_Java_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Java_Comment);
      Set_Defining_Identifier (Result_Node, New_Node (K_Defining_Identifier));
      Set_Name (Defining_Identifier (Result_Node), N);
      Set_Has_Header_Spaces (Result_Node, Has_Header_Spaces);

      return Result_Node;
   end Make_Java_Comment;

   ------------------------
   -- Make_New_Statement --
   ------------------------
   function Make_New_Statement
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List;
      Is_Array            : Boolean := False) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_New_Statement);

      --  Name of the class instanciated
      Set_Defining_Identifier (Result_Node, Defining_Identifier);

      --  Parameters used for the constructor
      Set_Parameters (Result_Node, Parameters);

      Set_Is_Array (Result_Node, Is_Array);

      return Result_Node;
   end Make_New_Statement;

   ---------------------------
   -- Make_Public_Statement --
   ---------------------------
   function Make_Public_Statement return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Public);

      return Result_Node;
   end Make_Public_Statement;

   ------------------------------
   -- Make_Protected_Statement --
   ------------------------------
   function Make_Protected_Statement return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Protected);

      return Result_Node;
   end Make_Protected_Statement;

   ----------------------------
   -- Make_Private_Statement --
   ----------------------------
   function Make_Private_Statement return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Private);

      return Result_Node;
   end Make_Private_Statement;

   -----------------------------
   -- Make_Abstract_Statement --
   -----------------------------
   function Make_Abstract_Statement return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Abstract);

      return Result_Node;
   end Make_Abstract_Statement;

   ---------------------------
   -- Make_Static_Statement --
   ---------------------------
   function Make_Static_Statement return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Static);

      return Result_Node;
   end Make_Static_Statement;

   --------------------------
   -- Make_Final_Statement --
   --------------------------
   function Make_Final_Statement return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Final);

      return Result_Node;
   end Make_Final_Statement;

   ---------------------
   -- Make_Enumerator --
   ---------------------
   function Make_Enumerator (Enum_Members : List_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Enumerator);

      --  Literals defined
      Set_Enum_Members (Result_Node, Enum_Members);

      return Result_Node;
   end Make_Enumerator;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------
   function Make_Defining_Identifier (Name : Name_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Defining_Identifier);
      Set_Name (Result_Node, Name);

      return Result_Node;
   end Make_Defining_Identifier;

   ---------------------------
   -- Make_Pointed_Notation --
   ---------------------------
   function Make_Pointed_Notation
     (Left_Member  : Node_Id;
      Right_Member : Node_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Pointed_Notation);
      Set_Defining_Identifier (Result_Node, Left_Member);
      Set_Right_Member (Result_Node, Right_Member);

      return Result_Node;
   end Make_Pointed_Notation;

   ---------------------
   -- Make_Expression --
   ---------------------
   function Make_Expression
     (Left_Expression     : Node_Id;
      Operator_Expression : Operator_Type := Op_None;
      Right_Expression    : Node_Id       := No_Node) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Expression);
      Set_Left_Expression (Result_Node, Left_Expression);
      Set_Operator_Expression
        (Result_Node,
         Operator_Type'Pos (Operator_Expression));
      Set_Right_Expression (Result_Node, Right_Expression);

      return Result_Node;
   end Make_Expression;

   ---------------------------------
   -- Make_Function_Specification --
   ---------------------------------
   function Make_Function_Specification
     (Visibility          : List_Id;
      Return_Type         : Node_Id := No_Node;
      Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List;
      Throws              : List_Id := No_List) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Function_Specification);
      Set_Visibility (Result_Node, Visibility);
      Set_Return_Type (Result_Node, Return_Type);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);
      Set_Parameters (Result_Node, Parameters);
      Set_Throws (Result_Node, Throws);

      return Result_Node;
   end Make_Function_Specification;

   ----------------------------------
   -- Make_Function_Implementation --
   ----------------------------------
   function Make_Function_Implementation
     (Specification : Node_Id;
      Declarations  : List_Id := No_List;
      Statements    : List_Id := No_List) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Function_Implementation);
      Set_Specification (Result_Node, Specification);
      Set_Declarations (Result_Node, Declarations);
      Set_Statements (Result_Node, Statements);

      return Result_Node;
   end Make_Function_Implementation;

   ------------------------
   -- Make_Call_Function --
   ------------------------
   function Make_Call_Function
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Call_Function);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);
      Set_Parameters (Result_Node, Parameters);

      return Result_Node;
   end Make_Call_Function;

   ----------------------------------
   -- Make_Parameter_Specification --
   ----------------------------------
   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Type      : Node_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Parameter_Specification);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);
      Set_Parameter_Type (Result_Node, Parameter_Type);

      return Result_Node;
   end Make_Parameter_Specification;

   ----------------------------
   -- Make_Array_Declaration --
   ----------------------------
   function Make_Array_Declaration
     (Defining_Identifier : Node_Id;
      Array_Size          : Node_Id := No_Node) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Array_Declaration);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);
      Set_Array_Size (Result_Node, Array_Size);

      return Result_Node;
   end Make_Array_Declaration;

   ----------------------
   -- Make_Array_Value --
   ----------------------
   function Make_Array_Value
     (Defining_Identifier : Node_Id;
      Array_Item          : Node_Id := No_Node) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Array_Value);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);
      Set_Array_Item (Result_Node, Array_Item);

      return Result_Node;
   end Make_Array_Value;

   ---------------------------------
   -- Make_Full_Array_Declaration --
   ---------------------------------
   function Make_Full_Array_Declaration
     (Array_Declaration : Node_Id;
      Array_Assignments : List_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Full_Array_Declaration);
      Set_Array_Declaration (Result_Node, Array_Declaration);
      Set_Array_Assignments (Result_Node, Array_Assignments);

      return Result_Node;
   end Make_Full_Array_Declaration;

   -------------------------
   -- Make_Cast_Statement --
   -------------------------
   function Make_Cast_Statement
     (Cast_Type           : Node_Id;
      Defining_Identifier : Node_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Cast_Statement);
      Set_Cast_Type (Result_Node, Cast_Type);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);

      return Result_Node;
   end Make_Cast_Statement;

   ---------------------------
   -- Make_Switch_Statement --
   ---------------------------
   function Make_Switch_Statement
     (Expr            : Node_Id;
      Case_Statements : List_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Switch_Statement);
      Set_Expr (Result_Node, Expr);
      Set_Case_Statements (Result_Node, Case_Statements);

      return Result_Node;
   end Make_Switch_Statement;

   -------------------------
   -- Make_Case_Statement --
   -------------------------
   function Make_Case_Statement
     (Labels     : List_Id;
      Statements : List_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Case_Statement);
      Set_Labels (Result_Node, Labels);
      Set_Statements (Result_Node, Statements);

      return Result_Node;
   end Make_Case_Statement;

   --------------------------
   -- Make_Throw_Statement --
   --------------------------
   function Make_Throw_Statement
     (Defining_Identifier : Node_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Throw_Statement);
      Set_Defining_Identifier (Result_Node, Defining_Identifier);

      return Result_Node;
   end Make_Throw_Statement;

   ------------------------
   -- Make_For_Statement --
   ------------------------
   function Make_For_Statement
     (Init_Statement      : Node_Id := No_Node;
      Iteration_Condition : Node_Id := No_Node;
      Step_Expression     : Node_Id := No_Node;
      Statements          : List_Id) return Node_Id
   is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_For_Statement);
      Set_Init_Statement (Result_Node, Init_Statement);
      Set_Iteration_Condition (Result_Node, Iteration_Condition);
      Set_Step_Expression (Result_Node, Step_Expression);
      Set_Statements (Result_Node, Statements);

      return Result_Node;
   end Make_For_Statement;

   ------------------
   -- Make_Literal --
   ------------------
   function Make_Literal (Value_Literal : Value_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := New_Node (K_Literal);
      Set_Value_Literal (Result_Node, Value_Literal);

      return Result_Node;
   end Make_Literal;

   ----------------------
   -- Make_Source_File --
   ----------------------
   function Make_Source_File (Identifier : Node_Id) return Node_Id is
      File : Node_Id;
   begin
      File := New_Node (K_Source_File);
      Set_Defining_Identifier (File, Identifier);
      Set_Corresponding_Node (Identifier, File);
      Set_Imported_Headers (File, New_List (K_Header_List));
      Set_Declarations (File, New_List (K_Declaration_List));
      Make_Comment_Header (Declarations (File));
      Set_Statements (File, New_List (K_Statement_List));

      return File;
   end Make_Source_File;

   -------------------------
   -- Make_Comment_Header --
   -------------------------
   procedure Make_Comment_Header (Header : List_Id) is
      N : Node_Id;
   begin
      --  Appending the comment header lines to the file header

      Set_Str_To_Name_Buffer
        ("***************************************************");
      N := Make_Java_Comment (Name_Find, False);
      Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("This file was automatically generated by Ocarina ");
      N := Make_Java_Comment (Name_Find);
      RTU.Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("Do NOT hand-modify this file, as your            ");
      N := Make_Java_Comment (Name_Find);
      RTU.Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("changes will be lost when you re-run Ocarina     ");
      N := Make_Java_Comment (Name_Find);
      RTU.Append_Node_To_List (N, Header);

      Set_Str_To_Name_Buffer
        ("***************************************************");
      N := Make_Java_Comment (Name_Find, False);
      RTU.Append_Node_To_List (N, Header);
   end Make_Comment_Header;

end Ocarina.Backends.RTSJ_Tree.Nutils;
