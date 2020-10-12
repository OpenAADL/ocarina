------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . A D A _ T R E E . N U T I L S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2020 ESA & ISAE.      --
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

with Charset;        use Charset;
with Locations;      use Locations;
with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;
with Ocarina.Types;  use Ocarina.Types;
with Utils;          use Utils;

with Ocarina.Backends.Ada_Values; use Ocarina.Backends.Ada_Values;
with Ocarina.Backends.Utils;      use Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;   use Ocarina.Backends.Messages;

with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Backends.Ada_Tree.Nutils is

   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;
   package AAN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   Var_Suffix  : constant String := "_Ü";
   Initialized : Boolean         := False;

   Keyword_Suffix : constant String := "%Ada";
   --  Used to mark Ada keywords and avoid collision with other
   --  languages

   type Entity_Stack_Entry is record
      Current_Package : Node_Id;
      Current_Entity  : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is new GNAT.Table
     (Entity_Stack_Entry,
      Int,
      No_Depth + 1,
      10,
      10);

   use Entity_Stack;

   function Create_Unique_Identifier
     (Name   : Name_Id;
      Suffix : String := "") return Name_Id;
   --  This function returns a unique identifier for Name with a UT_ prefix,
   --  followed by the name of the node, name of the package, Name
   --  and Suffix if exists.

   function Get_Style_State return Value_Id;
   --  This function returns a string literal which is the value given
   --  to the pragma style_checks. The 'Off' value is does not ignore
   --  line length.

   procedure New_Operator (O : Operator_Type; I : String := "");

   ----------------------
   -- Add_With_Package --
   ----------------------

   procedure Add_With_Package
     (E            : Node_Id;
      Used         : Boolean := False;
      Warnings_Off : Boolean := False;
      Elaborated   : Boolean := False)
   is

      function To_Library_Unit (E : Node_Id) return Node_Id;
      --  Return the library unit which E belongs to in order to with
      --  it. As a special rule, package Standard returns No_Node.

      ---------------------
      -- To_Library_Unit --
      ---------------------

      function To_Library_Unit (E : Node_Id) return Node_Id is
         U : Node_Id;

      begin
         pragma Assert (Kind (E) = K_Designator);
         U := Corresponding_Node (Defining_Identifier (E));

         --  This node is not properly built as the corresponding node
         --  is not set.

         if No (U) then
            if Output_Tree_Warnings then
               Write_Str ("WARNING: node ");
               Write_Name (Name (Defining_Identifier (E)));
               Write_Line (" has a null corresponding node");
            end if;
            return E;
         end if;

         if ADN.Kind (U) = K_Package_Declaration then
            U := Package_Specification (U);
         end if;

         pragma Assert
           (Kind (U) = K_Package_Specification
            or else Kind (U) = K_Package_Instantiation);

         --  This is a subunit and we do not need to add a with for
         --  this unit but for one of its parents.  If the kind of the
         --  parent unit name is a K_Package_Instantiation, we
         --  consider it as a subunit.

         if Kind (U) = K_Package_Instantiation
           or else Is_Subunit_Package (U)
         then
            U := Parent_Unit_Name (E);

            --  This is a special case to handle package Standard

            if No (U) then
               return No_Node;
            end if;

            return To_Library_Unit (U);
         end if;

         return E;
      end To_Library_Unit;

      P             : constant Node_Id := To_Library_Unit (E);
      W             : Node_Id;
      N             : Name_Id;
      I             : Node_Id;
      Existing_With : Node_Id;

   begin
      if No (P) then
         return;
      end if;

      --  Build a string "<current_entity>%[s,b] <withed_entity>" that
      --  is the current entity name, a character 's' (resp 'b') to
      --  indicate whether we consider the spec (resp. body) of the
      --  current entity and the withed entity name.

      --  To avoid that a package "with"es itself

      if Kind (Current_Package) /= K_Subprogram_Implementation
        and then Kind (Current_Package) /= K_Subprogram_Specification
      then
         --  and then Corresponding_Node (Defining_Identifier (P))
         --  = Package_Declaration (Current_Package)

         if To_Lower (Fully_Qualified_Name (P)) =
           To_Lower
             (Fully_Qualified_Name
                (Defining_Identifier (Package_Declaration (Current_Package))))
         then
            return;
         end if;
      end if;

      --  Routine that check wether the package P has already been
      --  added to the withed packages of the current package. When we
      --  add a 'with' clause to a package specification, we check
      --  only if this clause has been added to the current
      --  spec. However, when we add a 'with' clause to a package
      --  body, we check that the clause has been added in both the
      --  spec and the body.

      --  IMPORTANT: Provided that all specs are generated before all
      --  bodies, this behaviour is automatically applied. We just
      --  need to encode the package name *without* precising whether
      --  it is a spec or a body

      --  Encoding the withed package and the current entity

      N := Fully_Qualified_Name (P);

      if Kind (Current_Package) /= K_Subprogram_Implementation
        and then Kind (Current_Package) /= K_Subprogram_Specification
      then
         I := Defining_Identifier (Package_Declaration (Current_Package));

         Get_Name_String (Fully_Qualified_Name (I));

         --  In both the PolyORB-HI and PolyORB-QoS generators some
         --  packages that are generated for different nodes have
         --  exactly the same name. We must encode the node name to
         --  differenciate them. This happens only when we deal with a
         --  package generated for a root node

         if Present
             (Main_Subprogram
                (Distributed_Application_Unit
                   (Package_Declaration (Current_Package))))
         then
            Add_Char_To_Name_Buffer (' ');
            Get_Name_String_And_Append
              (ADN.Name
                 (Defining_Identifier
                    (Main_Subprogram
                       (Distributed_Application_Unit
                          (Package_Declaration (Current_Package))))));
         end if;

      elsif Kind (Current_Package) /= K_Subprogram_Specification then
         I := Defining_Identifier (Specification (Current_Package));
         Get_Name_String (Fully_Qualified_Name (I));
      else
         I := Defining_Identifier (Current_Package);
         Get_Name_String (Fully_Qualified_Name (I));
      end if;

      Add_Char_To_Name_Buffer (' ');
      Get_Name_String_And_Append (N);
      N := To_Lower (Name_Find);

      --  Get the info associated to the obtained name in the hash
      --  table and check whether it is already set to a value
      --  different from 0 (No_Node) which means that the withed
      --  entity is already in the withed package list. In this case
      --  try to enrich the exisiting with clause with eventual 'use',
      --  'elaborate' or warning disabling clauses.

      Existing_With := Node_Id (Get_Name_Table_Info (N));

      if Present (Existing_With) then
         Set_Used (Existing_With, ADN.Used (Existing_With) or else Used);
         Set_Warnings_Off
           (Existing_With,
            ADN.Warnings_Off (Existing_With) or else Warnings_Off);
         Set_Elaborated
           (Existing_With,
            ADN.Elaborated (Existing_With) or else Elaborated);
         return;
      end if;

      --  Debug message (if wanted by the user)

      if Output_Unit_Withing then
         Write_Name (N);
         Write_Eol;
      end if;

      --  Add entity to the withed packages list of the current
      --  package

      W := Make_Withed_Package (P, Used, Warnings_Off, Elaborated);

      --  Mark the 'with' clause as being added to the current package

      Set_Name_Table_Info (N, Int (W));

      Append_Node_To_List (W, Withed_Packages (Current_Package));
   end Add_With_Package;

   ------------------------------------
   -- Append_Node_To_Current_Package --
   ------------------------------------

   procedure Append_Node_To_Current_Package (N : Node_Id) is
   begin
      case Kind (Current_Package) is
         when K_Package_Specification =>
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         when K_Package_Implementation =>
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         when others =>
            raise Program_Error;
      end case;
   end Append_Node_To_Current_Package;

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;

   begin
      Last := Last_Node (L);
      if No (Last) then
         Set_First_Node (L, E);
      else
         Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         Set_Last_Node (L, Last);
         Last := Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   -----------------------
   -- Insert_After_Node --
   -----------------------

   procedure Insert_After_Node (E : Node_Id; N : Node_Id) is
      Next : constant Node_Id := Next_Node (N);
   begin
      Set_Next_Node (N, E);
      Set_Next_Node (E, Next);
   end Insert_After_Node;

   ------------------------
   -- Insert_Before_Node --
   ------------------------

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id) is
      Entity : Node_Id;
   begin
      Entity := First_Node (L);
      if Entity = N then
         Set_Next_Node (E, Entity);
         Set_First_Node (L, E);
      else
         while Present (Entity) loop
            exit when Next_Node (Entity) = N;
            Entity := Next_Node (Entity);
         end loop;

         Insert_After_Node (E, Entity);
      end if;
   end Insert_Before_Node;

   ---------------------
   -- Copy_Designator --
   ---------------------

   function Copy_Designator
     (Designator : Node_Id;
      Withed     : Boolean := True) return Node_Id
   is
      D : Node_Id;
      P : Node_Id := Parent_Unit_Name (Designator);

   begin
      D := Copy_Node (Designator);
      if Kind (Designator) = K_Designator
        or else Kind (Designator) = K_Defining_Identifier
      then
         P := Parent_Unit_Name (Designator);
      elsif Kind (Designator) = K_Attribute_Designator then
         P := Parent_Unit_Name (Prefix (Designator));
      end if;

      if Present (P) then
         P := Copy_Designator (P, False);
         if Withed then
            Add_With_Package (P);
         end if;
      end if;
      return D;
   end Copy_Designator;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (N : Node_Id) return Node_Id is
      C : Node_Id;

   begin
      case Kind (N) is
         when K_Designator =>
            C := New_Node (K_Designator);
            Set_Defining_Identifier (C, Defining_Identifier (N));
            Set_Frontend_Node (C, Frontend_Node (N));
            Set_Homogeneous_Parent_Unit_Name (C, Parent_Unit_Name (N));

         when K_Defining_Identifier =>
            C := New_Node (K_Defining_Identifier);
            Set_Name (C, Name (N));
            Set_Homogeneous_Parent_Unit_Name (C, Parent_Unit_Name (N));
            Set_Corresponding_Node (C, Corresponding_Node (N));

         when K_Attribute_Designator =>
            C := New_Node (K_Attribute_Designator);
            Set_Name (C, Name (N));
            Set_Prefix (C, Copy_Node (Prefix (N)));

         when others =>
            raise Program_Error;
      end case;
      return C;
   end Copy_Node;

   ------------------------------------------
   -- Create_Subtype_From_Range_Constraint --
   ------------------------------------------

   function Create_Subtype_From_Range_Constraint
     (R : Node_Id) return Node_Id
   is
      N         : Node_Id := No_Node;
      C_First   : Node_Id := No_Node;
      C_Last    : Node_Id := No_Node;
      C_Index   : Node_Id := No_Node;
      Ident     : Node_Id := No_Node;
      Sub_Ident : Node_Id := No_Node;
   begin
      pragma Assert (Kind (R) = K_Range_Constraint);

      --  Stock identifier of the node in the variable Ident.
      --  If the node is not a literal, only its identifier is necessary.
      --  Variables C_first, C_Last and C_Index keep informations to
      --  construct the type replacing the range constraint.
      --  C_First and C_Last stock identifier of the node except for
      --  a literal node.

      if Present (Nodes.First (R)) then
         case Kind (Nodes.First (R)) is
            when K_Attribute_Designator =>
               C_First := Defining_Identifier (Nodes.Prefix (Nodes.First (R)));
               Ident   := C_First;

            when K_Designator =>
               C_First := Defining_Identifier (Nodes.First (R));
               Ident   := C_First;

            when K_Literal =>
               C_First := Nodes.First (R);
               Ident   :=
                 Make_Defining_Identifier
                   (Get_String_Name
                      (Ada_Values.Image (Nodes.Value (C_First))));

            when K_Defining_Identifier =>
               C_First := Nodes.First (R);
               Ident   := C_First;
            when others =>
               null;
         end case;
      end if;

      if Present (Nodes.Last (R)) then
         case Kind (Nodes.Last (R)) is
            when K_Attribute_Designator =>
               C_Last := Defining_Identifier (Nodes.Prefix (Nodes.Last (R)));

            when K_Designator =>
               C_Last := Defining_Identifier (Nodes.Last (R));

            when K_Literal =>
               C_Last :=
                 Make_Defining_Identifier
                   (Get_String_Name
                      (Ada_Values.Image (Nodes.Value (Nodes.Last (R)))));

            when K_Defining_Identifier =>
               C_Last := Nodes.Last (R);

            when others =>
               null;
         end case;

         --  Construct identifier of the type : First_range_Last_Range
         Get_Name_String (Name (Ident));
         Add_Char_To_Name_Buffer ('_');
         Get_Name_String_And_Append (Name (C_Last));
         Sub_Ident := Make_Defining_Identifier (Name_Find);
      end if;

      if Present (Index_Type (R)) then
         case Kind (Index_Type (R)) is
            when K_Attribute_Designator =>
               Ident := Defining_Identifier (Nodes.Prefix (Index_Type (R)));

               if C_First = No_Node and then C_Last = No_Node then

                  --  Consider only Range attribute. Can be problematic
                  --  with a size attribute for instance.
                  C_Index :=
                    Make_Range_Constraint
                      (Make_Attribute_Designator (Ident, A_First),
                       Make_Attribute_Designator (Ident, A_Last),
                       Ident);

                  Sub_Ident :=
                    Make_Defining_Identifier (Name (Index_Type (R)));
               end if;
            when K_Designator =>
               Ident   := Defining_Identifier (Index_Type (R));
               C_Index := Index_Type (R);

            when others =>
               null;
         end case;
      end if;

      --  Case of unconstraint array (range <>)
      --  or a range attribute (Index'Range).
      if (C_First = No_Node)
        and then (C_Last = No_Node)
        and then (C_Index /= No_Node)
        and then Ident /= No_Node
      then

         --  if C_Index is an unconstraint array (range <>)
         --  return a range constraint, else return created type.
         if Kind (C_Index) = K_Designator then
            N := Make_Range_Constraint (No_Node, No_Node, Ident);
         else
            Sub_Ident :=
              Make_Defining_Identifier
                (Create_Unique_Identifier
                   (Name (Ident),
                    Get_Name_String (Name (Sub_Ident))));
            if Get_Name_Table_Info (Name (Sub_Ident)) = Int (No_Node) then
               N :=
                 Make_Full_Type_Declaration
                   (Defining_Identifier => Sub_Ident,
                    Type_Definition     => C_Index,
                    Is_Subtype          => True);
               Set_Name_Table_Info (Name (Sub_Ident), Int (Sub_Ident));
               Append_Node_To_Current_Package (N);
            else
               N :=
                 Corresponding_Node
                   (Node_Id (Get_Name_Table_Info (Name (Sub_Ident))));
            end if;
         end if;

      --  Case range constraint is of the form :
      --  My_Type range Range_First .. Range_Last
      --  create a type : subtype UT_Type is My_Type Range_First ..Range_Last
      elsif (C_First /= No_Node)
        and then (C_Last /= No_Node)
        and then (C_Index /= No_Node)
      then

         Sub_Ident :=
           Make_Defining_Identifier
             (Create_Unique_Identifier (Name (Sub_Ident)));

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier => Sub_Ident,
              Type_Definition     =>
                Make_Range_Constraint (C_First, C_Last, Ident),
              Is_Subtype => True);

         if Get_Name_Table_Info (Name (Sub_Ident)) = Int (No_Node) then
            Set_Name_Table_Info (Name (Sub_Ident), Int (Sub_Ident));
            Append_Node_To_Current_Package (N);
         else
            N :=
              Corresponding_Node
                (Node_Id (Get_Name_Table_Info (Name (Sub_Ident))));
         end if;

      --  Case range constraint is of the form : 1 .. Max_Size,
      --  create a type : type UT_Type is Integer range 1 .. Max_Size
      elsif (C_First /= No_Node)
        and then (C_Last /= No_Node)
        and then (C_Index = No_Node)
      then

         Sub_Ident :=
           Make_Defining_Identifier
             (Create_Unique_Identifier (Name (Sub_Ident)));

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier => Sub_Ident,
              Type_Definition     =>
                Make_Range_Constraint
                  (C_First,
                   C_Last,
                   Make_Defining_Identifier (TN (T_Integer))),
              Is_Subtype => True);

         if Get_Name_Table_Info (Name (Sub_Ident)) = Int (No_Node) then
            Set_Name_Table_Info (Name (Sub_Ident), Int (Sub_Ident));
            Append_Node_To_Current_Package (N);
         else
            N :=
              Corresponding_Node
                (Node_Id (Get_Name_Table_Info (Name (Sub_Ident))));
         end if;
      end if;

      return N;
   end Create_Subtype_From_Range_Constraint;

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

   ---------------------
   -- Current_Package --
   ---------------------

   function Current_Package return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_Package;
      end if;
   end Current_Package;

   ---------------------------------------
   -- Defining_Identifier_To_Designator --
   ---------------------------------------

   function Defining_Identifier_To_Designator
     (N                       : Node_Id;
      Copy                    : Boolean := False;
      Keep_Parent             : Boolean := True;
      Keep_Corresponding_Node : Boolean := True) return Node_Id
   is
      P      : Node_Id;
      Def_Id : Node_Id := N;
   begin
      pragma Assert (ADN.Kind (N) = K_Defining_Identifier);

      if Copy then
         Def_Id := Copy_Node (N);
      end if;

      if not Keep_Parent then
         Def_Id := Make_Defining_Identifier (ADN.Name (N));
      end if;

      if Keep_Corresponding_Node then
         Set_Corresponding_Node (Def_Id, Corresponding_Node (N));
      end if;

      P := New_Node (K_Designator);
      Set_Defining_Identifier (P, Def_Id);

      if Keep_Parent then
         Set_Homogeneous_Parent_Unit_Name (P, Parent_Unit_Name (N));
      end if;

      return P;
   end Defining_Identifier_To_Designator;

   ---------------------
   -- Message_Comment --
   ---------------------

   function Message_Comment (M : Name_Id) return Node_Id is
      C : Node_Id;
   begin
      C := Make_Ada_Comment (M);
      return C;
   end Message_Comment;

   ---------------------
   -- Message_Comment --
   ---------------------

   function Message_Comment (M : String) return Node_Id is
      C : Node_Id;
   begin
      Set_Str_To_Name_Buffer (M);
      C := Make_Ada_Comment (Name_Find);
      return C;
   end Message_Comment;

   --------------------------
   -- Fully_Qualified_Name --
   --------------------------

   function Fully_Qualified_Name (N : Node_Id) return Name_Id is
      Parent_Node : Node_Id := No_Node;
      Parent_Name : Name_Id := No_Name;

   begin
      case Kind (N) is
         when K_Designator =>
            Parent_Node := Parent_Unit_Name (N);

            if not Present (Parent_Node) then
               Parent_Node := Parent_Unit_Name (Defining_Identifier (N));
            end if;

            if Present (Parent_Node) then
               Parent_Name := Fully_Qualified_Name (Parent_Node);
            end if;

            Name_Len := 0;
            if Present (Parent_Node) then
               Get_Name_String (Parent_Name);
               Add_Char_To_Name_Buffer ('.');
            end if;
            Get_Name_String_And_Append (Name (Defining_Identifier (N)));
            return Name_Find;

         when K_Defining_Identifier =>
            Parent_Node := Parent_Unit_Name (N);
            if Present (Parent_Node) then
               Parent_Name := Fully_Qualified_Name (Parent_Node);
            end if;

            Name_Len := 0;
            if Present (Parent_Node) then
               Get_Name_String (Parent_Name);
               Add_Char_To_Name_Buffer ('.');
            end if;
            Get_Name_String_And_Append (Name (N));
            return Name_Find;

         when K_Attribute_Designator =>
            Get_Name_String (Fully_Qualified_Name (Prefix (N)));
            Add_Char_To_Name_Buffer (''');
            Get_Name_String_And_Append (Name (N));
            return Name_Find;

         when others =>
            raise Program_Error;
      end case;
   end Fully_Qualified_Name;

   ---------------------
   -- Get_Style_State --
   ---------------------

   function Get_Style_State return Value_Id is

      --  The maximum line length allowed by GNAT is 32766

      Max_Line_Length : constant Int := 32766;
      Result          : Value_Id;
   begin
      Set_Str_To_Name_Buffer ("NM");
      Add_Nat_To_Name_Buffer (Max_Line_Length);
      Result := New_String_Value (Name_Find);
      return Result;
   end Get_Style_State;

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
      New_Token (Tok_Double_Asterisk, "**");
      New_Token (Tok_Ampersand, "&");
      New_Token (Tok_Minus, "-");
      New_Token (Tok_Plus, "+");
      New_Token (Tok_Asterisk, "*");
      New_Token (Tok_Slash, "/");
      New_Token (Tok_Dot, ".");
      New_Token (Tok_Apostrophe, "'");
      New_Token (Tok_Left_Paren, "(");
      New_Token (Tok_Right_Paren, ")");
      New_Token (Tok_Comma, ",");
      New_Token (Tok_Less, "<");
      New_Token (Tok_Equal, "=");
      New_Token (Tok_Greater, ">");
      New_Token (Tok_Not_Equal, "/=");
      New_Token (Tok_Greater_Equal, ">=");
      New_Token (Tok_Less_Equal, "<=");
      New_Token (Tok_Box, "<>");
      New_Token (Tok_Colon_Equal, ":=");
      New_Token (Tok_Colon, ":");
      New_Token (Tok_Greater_Greater, ">>");
      New_Token (Tok_Less_Less, "<<");
      New_Token (Tok_Semicolon, ";");
      New_Token (Tok_Arrow, "=>");
      New_Token (Tok_Vertical_Bar, "|");
      New_Token (Tok_Dot_Dot, "..");
      New_Token (Tok_Minus_Minus, "--");

      for O in Op_And .. Op_Or_Else loop
         New_Operator (O);
      end loop;
      New_Operator (Op_And_Symbol, "&");
      New_Operator (Op_Double_Asterisk, "**");
      New_Operator (Op_Minus, "-");
      New_Operator (Op_Plus, "+");
      New_Operator (Op_Asterisk, "*");
      New_Operator (Op_Slash, "/");
      New_Operator (Op_Less, "<");
      New_Operator (Op_Equal, "=");
      New_Operator (Op_Greater, ">");
      New_Operator (Op_Not_Equal, "/=");
      New_Operator (Op_Greater_Equal, ">=");
      New_Operator (Op_Less_Equal, "<=");
      New_Operator (Op_Box, "<>");
      New_Operator (Op_Colon_Equal, ":=");
      New_Operator (Op_Colon, "--");
      New_Operator (Op_Greater_Greater, ">>");
      New_Operator (Op_Less_Less, "<<");
      New_Operator (Op_Semicolon, ";");
      New_Operator (Op_Arrow, "=>");
      New_Operator (Op_Vertical_Bar, "|");

      for A in Attribute_Id loop
         Set_Str_To_Name_Buffer (Attribute_Id'Image (A));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         AN (A) := Name_Find;
      end loop;

      for C in Component_Id loop
         Set_Str_To_Name_Buffer (Component_Id'Image (C));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         CN (C) := Name_Find;
      end loop;

      for P in Parameter_Id loop
         Set_Str_To_Name_Buffer (Parameter_Id'Image (P));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         PN (P) := Name_Find;
      end loop;

      for S in Subprogram_Id loop
         Set_Str_To_Name_Buffer (Subprogram_Id'Image (S));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         SN (S) := Name_Find;
      end loop;

      for T in Type_Id loop
         Set_Str_To_Name_Buffer (Type_Id'Image (T));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         TN (T) := Name_Find;
      end loop;

      for V in Variable_Id loop
         Set_Str_To_Name_Buffer (Variable_Id'Image (V));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Add_Str_To_Name_Buffer (Var_Suffix);
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         VN (V) := Name_Find;
      end loop;

      for G in Pragma_Id loop
         Set_Str_To_Name_Buffer (Pragma_Id'Image (G));
         Set_Str_To_Name_Buffer (Name_Buffer (8 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         GN (G) := Name_Find;
      end loop;

      for E in Error_Id loop
         Set_Str_To_Name_Buffer (Error_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         EN (E) := Name_Find;
      end loop;

      for A in Aspect_Id loop
         Set_Str_To_Name_Buffer (Aspect_Id'Image (A));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
         ASN (A) := Name_Find;
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
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := First_Node (L);

         while Present (N) loop
            C := C + 1;
            N := Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   ---------------------------------
   -- Make_Access_Type_Definition --
   ---------------------------------

   function Make_Access_Type_Definition
     (Subtype_Indication : Node_Id;
      Is_All             : Boolean := False;
      Is_Constant        : Boolean := False;
      Is_Not_Null        : Boolean := False) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Access_Type_Definition);
      Set_Subtype_Indication (N, Subtype_Indication);

      Set_Is_All (N, Is_All);
      Set_Is_Constant (N, Is_Constant);
      Set_Is_Not_Null (N, Is_Not_Null);
      return N;
   end Make_Access_Type_Definition;

   ----------------------
   -- Make_Ada_Comment --
   ----------------------

   function Make_Ada_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True) return Node_Id
   is
      C : Node_Id;
   begin
      C := New_Node (K_Ada_Comment);
      Set_Defining_Identifier (C, New_Node (K_Defining_Identifier));
      Set_Name (Defining_Identifier (C), N);
      Set_Has_Header_Spaces (C, Has_Header_Spaces);
      return C;
   end Make_Ada_Comment;

   --------------------------
   -- Make_Array_Aggregate --
   --------------------------

   function Make_Array_Aggregate (Elements : List_Id) return Node_Id is
      pragma Assert (not Is_Empty (Elements));
      N : Node_Id;
   begin
      N := New_Node (K_Array_Aggregate);
      Set_Elements (N, Elements);
      return N;
   end Make_Array_Aggregate;

   ------------------------------
   -- Create_Unique_Identifier --
   ------------------------------

   function Create_Unique_Identifier
     (Name   : Name_Id;
      Suffix : String := "") return Name_Id
   is
      Name_Returned : Name_Id;
      Pack          : constant Name_Id :=
        Nodes.Name
          (Defining_Identifier (Package_Declaration (Current_Package)));
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Pack);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append
        (ADN.Name
           (Defining_Identifier
              (Main_Subprogram
                 (Distributed_Application_Unit
                    (Package_Declaration (Current_Package))))));
      GNAT.Case_Util.To_Mixed (Name_Buffer (1 .. Name_Len));
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Name);
      if Suffix /= "" then
         Name_Returned :=
           Add_Prefix_To_Name
             ("UT_",
              Add_Suffix_To_Name ("_" & Suffix, Name_Find));
      else
         Name_Returned := Add_Prefix_To_Name ("UT_", Name_Find);
      end if;
      return Name_Returned;
   end Create_Unique_Identifier;

   --------------------------------
   -- Make_Array_Type_Definition --
   --------------------------------

   function Make_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id;
      Aliased_Present      : Boolean := False) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (ADN.K_Array_Type_Definition);
      Set_Range_Constraints (N, Range_Constraints);
      Set_Component_Definition (N, Component_Definition);
      Set_Aliased_Present (N, Aliased_Present);
      return N;
   end Make_Array_Type_Definition;

   -------------------------------
   -- Make_Aspect_Specification --
   -------------------------------

   function Make_Aspect_Specification (Aspects : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_Aspect_Specification);
      Set_Aspect (N, Aspects);
      return N;
   end Make_Aspect_Specification;

   --------------
   -- Make_Pre --
   --------------

   function Make_Pre (Subprogram_Call : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_Pre_Definition);
      Set_Subprogram_Call (N, Subprogram_Call);
      return N;
   end Make_Pre;

   -------------------------------
   -- Make_Global_Specification --
   -------------------------------

   function Make_Global_Specification
     (Moded_Global_List : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_Global_Specification);
      Set_Moded_Global_List (N, Moded_Global_List);
      return N;
   end Make_Global_Specification;

   function Make_Moded_Global_List
   (Mode : Mode_Id; Identifier : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_Moded_Global_List);
      Set_Mode_Selector (N, Mode);
      Set_Defining_Identifier (N, Identifier);
      return N;
   end Make_Moded_Global_List;

   -----------------
   -- Make_Aspect --
   -----------------

   function Make_Aspect
     (Aspect_Mark : Name_Id;
      Aspect_Definition : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_Aspect);
      Set_Aspect_Mark (N, Aspect_Mark);
      Set_Aspect_Definition (N, Aspect_Definition);
      return N;
   end Make_Aspect;

   ------------------------------
   -- Make_Initialization_Spec --
   ------------------------------

   function Make_Initialization_Spec
     (Initialization_List : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_Initialization_Spec);
      Set_Initialization_List (N, Initialization_List);
      return N;
   end Make_Initialization_Spec;

   ------------------------------
   -- Make_Abstract_State_List --
   ------------------------------

   function Make_Abstract_State_List
     (State_Name_With_Option : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_Abstract_State_List);
      Set_State_Name_With_Option (N, State_Name_With_Option);
      return N;
   end Make_Abstract_State_List;

   ---------------------------------
   -- Make_State_Name_With_Option --
   ---------------------------------

   function Make_State_Name_With_Option
     (Defining_Identifier : Node_Id;
      Synchronous : Boolean;
      External : Boolean) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (ADN.K_State_Name_With_Option);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Synchronous (N, Synchronous);
      Set_External (N, External);
      return N;
   end Make_State_Name_With_Option;

   --------------------------
   -- Make_Refinement_List --
   --------------------------

   function Make_Refinement_List
     (Refinement_Clause : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (Adn.K_Refinement_List);
      Set_Refinement_Clause (N, Refinement_Clause);
      return N;
   end Make_Refinement_List;

   ----------------------------
   -- Make_Refinement_Clause --
   ----------------------------

   function Make_Refinement_Clause
     (State_Name : Node_Id;
      Constituent : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (Adn.K_Refinement_Clause);
      Set_State_Name (N, State_Name);
      Set_Constituent (N, Constituent);
      return N;
   end Make_Refinement_Clause;

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

   --------------------------------------
   -- Make_Attribute_Definition_Clause --
   --------------------------------------

   function Make_Attribute_Definition_Clause
     (Defining_Identifier  : Node_Id;
      Attribute_Designator : Attribute_Id;
      Expression           : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Attribute_Definition_Clause);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Attribute_Designator (N, AN (Attribute_Designator));
      Set_Expression (N, Expression);

      return N;
   end Make_Attribute_Definition_Clause;

   -------------------------------
   -- Make_Attribute_Designator --
   -------------------------------

   function Make_Attribute_Designator
     (Prefix    : Node_Id;
      Attribute : Attribute_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Attribute_Designator);
      Set_Prefix (N, Prefix);
      Set_Name (N, AN (Attribute));
      return N;
   end Make_Attribute_Designator;

   --------------------------
   -- Make_Block_Statement --
   --------------------------

   function Make_Block_Statement
     (Statement_Identifier : Node_Id := No_Node;
      Declarative_Part     : List_Id;
      Statements           : List_Id;
      Exception_Handler    : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Block_Statement);
      Set_Defining_Identifier (N, Statement_Identifier);
      if Present (Statement_Identifier) then
         Set_Corresponding_Node (Statement_Identifier, N);
      end if;
      Set_Declarative_Part (N, Declarative_Part);
      Set_Statements (N, Statements);
      if not Is_Empty (Exception_Handler) then
         Set_Exception_Handler (N, Exception_Handler);
      end if;
      return N;
   end Make_Block_Statement;

   ---------------------
   -- Make_Case_Label --
   ---------------------

   function Make_Case_Label (Value : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Case_Label);
      Set_Value (N, Value);
      return N;
   end Make_Case_Label;

   -------------------------
   -- Make_Case_Statement --
   -------------------------

   function Make_Case_Statement
     (Expression                  : Node_Id;
      Case_Statement_Alternatives : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Case_Statement);
      Set_Expression (N, Expression);
      Set_Case_Statement_Alternatives (N, Case_Statement_Alternatives);
      return N;
   end Make_Case_Statement;

   -------------------------------------
   -- Make_Case_Statement_Alternative --
   -------------------------------------

   function Make_Case_Statement_Alternative
     (Discret_Choice_List : List_Id;
      Statements          : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Case_Statement_Alternative);
      Set_Discret_Choice_List (N, Discret_Choice_List);
      Set_Statements (N, Statements);
      return N;
   end Make_Case_Statement_Alternative;

   --------------------------------
   -- Make_Component_Association --
   --------------------------------

   function Make_Component_Association
     (Selector_Name : Node_Id;
      Expression    : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Component_Association);
      Set_Defining_Identifier (N, Selector_Name);
      Set_Expression (N, Expression);
      return N;
   end Make_Component_Association;

   --------------------------------
   -- Make_Component_Declaration --
   --------------------------------

   function Make_Component_Declaration
     (Defining_Identifier : Node_Id;
      Subtype_Indication  : Node_Id;
      Expression          : Node_Id := No_Node;
      Aliased_Present     : Boolean := False) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Component_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Subtype_Indication (N, Subtype_Indication);
      Set_Expression (N, Expression);
      Set_Aliased_Present (N, Aliased_Present);
      return N;
   end Make_Component_Declaration;

   ----------------------------------
   -- Make_Decimal_Type_Definition --
   ----------------------------------

   function Make_Decimal_Type_Definition
     (D_Digits : Unsigned_Long_Long;
      D_Scale  : Unsigned_Long_Long) return Node_Id
   is
      N : Node_Id;
      V : Value_Id;
   begin
      N := New_Node (K_Decimal_Type_Definition);

      V :=
        New_Floating_Point_Value
          (Long_Double (1.0 / (10**(Integer (D_Scale)))));

      Set_Scale (N, Make_Literal (V));

      V := New_Integer_Value (D_Digits, 1, 10);
      Set_Total (N, V);

      return N;
   end Make_Decimal_Type_Definition;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier
     (Name : Name_Id; Normalize : Boolean := True) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Defining_Identifier);
      if Normalize then
         Set_Name (N, To_Ada_Name (Name));
      else
         Set_Name (N, Name);
      end if;

      return N;
   end Make_Defining_Identifier;

   --------------------------
   -- Make_Delay_Statement --
   --------------------------

   function Make_Delay_Statement
     (Expression : Node_Id;
      Is_Until   : Boolean := False) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Delay_Statement);
      Set_Expression (N, Expression);
      Set_Is_Until (N, Is_Until);
      return N;
   end Make_Delay_Statement;

   ----------------------------------
   -- Make_Derived_Type_Definition --
   ----------------------------------

   function Make_Derived_Type_Definition
     (Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id := No_Node;
      Is_Abstract_Type      : Boolean := False;
      Is_Private_Extention  : Boolean := False;
      Is_Subtype            : Boolean := False) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Derived_Type_Definition);
      Set_Is_Abstract_Type (N, Is_Abstract_Type);
      Set_Is_Private_Extention (N, Is_Private_Extention);
      Set_Subtype_Indication (N, Subtype_Indication);
      Set_Record_Extension_Part (N, Record_Extension_Part);
      Set_Is_Subtype (N, Is_Subtype);
      return N;
   end Make_Derived_Type_Definition;

   ---------------------
   -- Make_Designator --
   ---------------------

   function Make_Designator
     (Designator : Name_Id;
      Parent     : Name_Id := No_Name;
      Is_All     : Boolean := False) return Node_Id
   is
      N : Node_Id;
      P : Node_Id;
   begin
      N := New_Node (K_Designator);
      Set_Defining_Identifier (N, Make_Defining_Identifier (Designator));
      Set_Is_All (N, Is_All);

      if Parent /= No_Name then
         P := New_Node (K_Designator);
         Set_Defining_Identifier (P, Make_Defining_Identifier (Parent));
         Set_Homogeneous_Parent_Unit_Name (N, P);
      end if;

      return N;
   end Make_Designator;

   ------------------------------
   -- Make_Element_Association --
   ------------------------------

   function Make_Element_Association
     (Index      : Node_Id;
      Expression : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Element_Association);
      Set_Index (N, Index);
      Set_Expression (N, Expression);
      return N;
   end Make_Element_Association;

   --------------------------
   -- Make_Elsif_Statement --
   --------------------------

   function Make_Elsif_Statement
     (Condition       : Node_Id;
      Then_Statements : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Elsif_Statement);
      Set_Condition (N, Condition);
      Set_Then_Statements (N, Then_Statements);
      return N;
   end Make_Elsif_Statement;

   --------------------------------------
   -- Make_Enumeration_Type_Definition --
   --------------------------------------

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Enumeration_Type_Definition);
      Set_Enumeration_Literals (N, Enumeration_Literals);
      return N;
   end Make_Enumeration_Type_Definition;

   --------------------------------------------
   -- Make_Enumeration_Representation_Clause --
   --------------------------------------------

   function Make_Enumeration_Representation_Clause
     (Defining_Identifier : Node_Id;
      Array_Aggregate     : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Enumeration_Representation_Clause);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Array_Aggregate (N, Array_Aggregate);
      return N;
   end Make_Enumeration_Representation_Clause;

   -------------------------------
   -- Make_Explicit_Dereference --
   -------------------------------

   function Make_Explicit_Dereference (Prefix : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Explicit_Dereference);
      Set_Prefix (N, Prefix);
      return N;
   end Make_Explicit_Dereference;

   --------------------------------
   -- Make_Exception_Declaration --
   --------------------------------

   function Make_Exception_Declaration
     (Defining_Identifier : Node_Id;
      Renamed_Exception   : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Exception_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Renamed_Entity (N, Renamed_Exception);
      Set_Corresponding_Node (Defining_Identifier, N);
      Set_Parent (N, Current_Package);
      return N;
   end Make_Exception_Declaration;

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
      Set_Left_Expr (N, Left_Expr);
      Set_Operator (N, Operator_Type'Pos (Operator));
      Set_Right_Expr (N, Right_Expr);
      return N;
   end Make_Expression;

   ------------------------
   -- Make_For_Statement --
   ------------------------

   function Make_For_Statement
     (Defining_Identifier : Node_Id;
      Range_Constraint    : Node_Id;
      Statements          : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_For_Statement);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Range_Constraint (N, Range_Constraint);
      Set_Statements (N, Statements);
      return N;
   end Make_For_Statement;

   -------------------------
   -- Make_Loop_Statement --
   -------------------------

   function Make_Loop_Statement (Statements : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Loop_Statement);
      Set_Statements (N, Statements);
      return N;
   end Make_Loop_Statement;

   --------------------------------
   -- Make_Full_Type_Declaration --
   --------------------------------

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id;
      Discriminant_Spec   : Node_Id := No_Node;
      Parent              : Node_Id := No_Node;
      Is_Subtype          : Boolean := False) return Node_Id
   is
      N            : Node_Id;
      T_Definition : Node_Id := Type_Definition;
   begin
      --  Remove anonymous type if necessary.
      if Kind (Type_Definition) = K_Array_Type_Definition then
         T_Definition :=
           Remove_Anonymous_Array_Type_Definition
             (Range_Constraints (Type_Definition),
              Component_Definition (Type_Definition),
              Nodes.Aliased_Present (Type_Definition),
              Defining_Identifier,
              True);
      end if;

      N := New_Node (K_Full_Type_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Corresponding_Node (Defining_Identifier, N);
      Set_Type_Definition (N, T_Definition);
      Set_Discriminant_Spec (N, Discriminant_Spec);
      if Present (Parent) then
         Set_Parent (N, Parent);
      else
         Set_Parent (N, Current_Package);
      end if;
      Set_Is_Subtype (N, Is_Subtype);
      return N;
   end Make_Full_Type_Declaration;

   ------------------------------
   -- Make_Exit_When_Statement --
   ------------------------------

   function Make_Exit_When_Statement (Condition : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Exit_When_Statement);
      Set_Condition (N, Condition);
      return N;
   end Make_Exit_When_Statement;

   -----------------------
   -- Make_If_Statement --
   -----------------------

   function Make_If_Statement
     (Condition        : Node_Id;
      Then_Statements  : List_Id;
      Elsif_Statements : List_Id := No_List;
      Else_Statements  : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_If_Statement);
      Set_Condition (N, Condition);
      Set_Then_Statements (N, Then_Statements);
      Set_Elsif_Statements (N, Elsif_Statements);
      Set_Else_Statements (N, Else_Statements);
      return N;
   end Make_If_Statement;

   ----------------------------
   -- Make_Indexed_Component --
   ----------------------------

   function Make_Indexed_Component
     (Prefix      : Node_Id;
      Expressions : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Indexed_Component);
      Set_Prefix (N, Prefix);
      Set_Expressions (N, Expressions);
      return N;
   end Make_Indexed_Component;

   ------------------
   -- Make_List_Id --
   ------------------

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node;
      N4 : Node_Id := No_Node) return List_Id
   is
      L : List_Id;
   begin
      L := New_List (K_List_Id);
      Append_Node_To_List (N1, L);
      if Present (N2) then
         Append_Node_To_List (N2, L);

         if Present (N3) then
            Append_Node_To_List (N3, L);

            if Present (N4) then
               Append_Node_To_List (N4, L);
            end if;
         end if;
      end if;
      return L;
   end Make_List_Id;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal
     (Value             : Value_Id;
      Parent_Designator : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Literal);
      Set_Value (N, Value);
      Set_Parent_Designator (N, Parent_Designator);
      return N;
   end Make_Literal;

   -----------------------------------------
   -- Make_Main_Subprogram_Implementation --
   -----------------------------------------

   function Make_Main_Subprogram_Implementation
     (Identifier : Node_Id;
      Build_Spec : Boolean := False;
      Build_Body : Boolean := True) return Node_Id
   is
      Unit        : Node_Id;
      Spg         : Node_Id;
      N           : Node_Id;
      Style_State : constant Value_Id := Get_Style_State;
   begin
      Unit := New_Node (K_Main_Subprogram_Implementation);
      Set_Defining_Identifier (Unit, Identifier);
      Set_Corresponding_Node (Identifier, Unit);

      ----------
      -- Spec --
      ----------

      Spg :=
        Make_Subprogram_Specification
          (Defining_Identifier => Copy_Node (Identifier),
           Parameter_Profile   => No_List,
           Return_Type         => No_Node,
           Parent              => No_Node,
           Renamed_Subprogram  => No_Node);

      if Build_Spec then
         Set_Withed_Packages (Spg, New_List (K_Withed_Packages));
         Set_Package_Headers (Spg, New_List (K_Package_Headers));

         --  Adding a comment header

         Make_Comment_Header (Package_Headers (Spg));

         --  Disabling style checks

         N :=
           Make_Pragma_Statement
             (Pragma_Style_Checks,
              Make_List_Id (Make_Literal (Style_State)));
         Append_Node_To_List (N, Package_Headers (Spg));

         --  Binding

         Set_Main_Subprogram_Unit (Spg, Unit);
         Set_Subprogram_Specification (Unit, Spg);
      end if;

      if Build_Body then

         ----------
         -- Body --
         ----------

         Spg :=
           Make_Subprogram_Implementation
             (Specification => Spg,
              Declarations  => New_List (K_Declaration_List),
              Statements    => New_List (K_Statement_List));
         Set_Withed_Packages (Spg, New_List (K_Withed_Packages));
         Set_Package_Headers (Spg, New_List (K_Package_Headers));

         --  Adding a comment header

         Make_Comment_Header (Package_Headers (Spg));

         --  Disabling style checks

         N :=
           Make_Pragma_Statement
             (Pragma_Style_Checks,
              Make_List_Id (Make_Literal (Style_State)));
         Append_Node_To_List (N, Package_Headers (Spg));

         --  Binding

         Set_Main_Subprogram_Unit (Spg, Unit);
         Set_Subprogram_Implementation (Unit, Spg);
      end if;

      return Unit;
   end Make_Main_Subprogram_Implementation;

   -------------------------
   -- Make_Null_Statement --
   -------------------------

   function Make_Null_Statement return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Null_Statement);
      return N;
   end Make_Null_Statement;

   -----------------------------
   -- Make_Object_Declaration --
   -----------------------------

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean := False;
      Object_Definition   : Node_Id;
      Expression          : Node_Id := No_Node;
      Parent              : Node_Id := No_Node;
      Renamed_Object      : Node_Id := No_Node;
      Aliased_Present     : Boolean := False;
      Discriminant_Spec   : Node_Id := No_Node) return Node_Id
   is
      N              : Node_Id;
      Obj_Definition : Node_Id := Object_Definition;
      Exp            : Node_Id := Expression;
   begin
      --  Remove anonymous type if necessary.
      if Kind (Obj_Definition) = K_Array_Type_Definition then
         Obj_Definition :=
           Remove_Anonymous_Array_Type_Definition
             (Range_Constraints (Object_Definition),
              Component_Definition (Object_Definition),
              Nodes.Aliased_Present (Object_Definition),
              Defining_Identifier);

         --  Fully qualify aggregates
         if Kind (Exp) = K_Array_Aggregate then
            Exp := Make_Qualified_Expression (Obj_Definition, Expression);
         end if;
      end if;
      N := New_Node (K_Object_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Corresponding_Node (Defining_Identifier, N);
      Set_Constant_Present (N, Constant_Present);
      Set_Aliased_Present (N, Aliased_Present);
      Set_Object_Definition (N, Obj_Definition);
      Set_Expression (N, Exp);
      Set_Renamed_Entity (N, Renamed_Object);
      Set_Discriminant_Spec (N, Discriminant_Spec);

      if No (Parent) then
         Set_Parent (N, Current_Package);
      else
         Set_Parent (N, Parent);
      end if;

      return N;
   end Make_Object_Declaration;

   -------------------------------
   -- Make_Object_Instantiation --
   -------------------------------

   function Make_Object_Instantiation
     (Qualified_Expression : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Object_Instantiation);
      Set_Qualified_Expression (N, Qualified_Expression);
      return N;
   end Make_Object_Instantiation;

   ------------------------------
   -- Make_Package_Declaration --
   ------------------------------

   function Make_Package_Declaration (Identifier : Node_Id) return Node_Id is
      Pkg         : Node_Id;
      Unit        : Node_Id;
      N           : Node_Id;
      Style_State : constant Value_Id := Get_Style_State;
   begin
      Unit := New_Node (K_Package_Declaration);
      Set_Defining_Identifier (Unit, Identifier);
      Set_Corresponding_Node (Identifier, Unit);

      --  FIXME : Set the correct parent!

      ----------
      -- Spec --
      ----------

      Pkg := New_Node (K_Package_Specification);
      Set_Withed_Packages (Pkg, New_List (K_Withed_Packages));
      Set_Package_Headers (Pkg, New_List (K_Package_Headers));

      --  Adding a comment header

      Make_Comment_Header (Package_Headers (Pkg));

      --  Disabling style checks

      N :=
        Make_Pragma_Statement
          (Pragma_Style_Checks,
           Make_List_Id (Make_Literal (Style_State)));
      Append_Node_To_List (N, Package_Headers (Pkg));

      Set_Visible_Part (Pkg, New_List (K_Declaration_List));
      Set_Private_Part (Pkg, New_List (K_Declaration_List));
      Set_Package_Declaration (Pkg, Unit);
      Set_Package_Specification (Unit, Pkg);

      ----------
      -- Body --
      ----------

      Pkg := New_Node (K_Package_Implementation);
      Set_Withed_Packages (Pkg, New_List (K_Withed_Packages));
      Set_Package_Headers (Pkg, New_List (K_Package_Headers));

      --  Adding a comment header

      Make_Comment_Header (Package_Headers (Pkg));

      --  Disabling style checks

      N :=
        Make_Pragma_Statement
          (Pragma_Style_Checks,
           Make_List_Id (Make_Literal (Style_State)));
      Append_Node_To_List (N, Package_Headers (Pkg));

      Set_Declarations (Pkg, New_List (K_Declaration_List));
      Set_Statements (Pkg, New_List (K_Statement_List));
      Set_Package_Declaration (Pkg, Unit);
      Set_Package_Implementation (Unit, Pkg);

      return Unit;
   end Make_Package_Declaration;

   --------------------------------
   -- Make_Package_Instantiation --
   --------------------------------

   function Make_Package_Instantiation
     (Defining_Identifier : Node_Id;
      Generic_Package     : Node_Id;
      Parameter_List      : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Package_Instantiation);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Corresponding_Node (Defining_Identifier, N);
      Set_Generic_Package (N, Generic_Package);
      Set_Parameter_List (N, Parameter_List);
      return N;
   end Make_Package_Instantiation;

   ----------------------------------
   -- Make_Private_Type_Definition --
   ----------------------------------

   function Make_Private_Type_Definition return Node_Id is
   begin
      return New_Node (K_Private_Type_Definition);
   end Make_Private_Type_Definition;

   --------------------------------
   -- Make_Parameter_Association --
   --------------------------------

   function Make_Parameter_Association
     (Selector_Name    : Node_Id;
      Actual_Parameter : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Parameter_Association);
      Set_Selector_Name (N, Selector_Name);
      Set_Actual_Parameter (N, Actual_Parameter);
      return N;
   end Make_Parameter_Association;

   ----------------------------------
   -- Make_Parameter_Specification --
   ----------------------------------

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := Mode_In;
      Expression          : Node_Id := No_Node) return Node_Id
   is
      P : Node_Id;

   begin
      P := New_Node (K_Parameter_Specification);
      Set_Defining_Identifier (P, Defining_Identifier);
      Set_Parameter_Type (P, Subtype_Mark);
      Set_Parameter_Mode (P, Parameter_Mode);
      Set_Expression (P, Expression);
      return P;
   end Make_Parameter_Specification;

   ---------------------------
   -- Make_Pragma_Statement --
   ---------------------------

   function Make_Pragma_Statement
     (The_Pragma    : Pragma_Id;
      Argument_List : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Pragma_Statement);

      Set_Defining_Identifier (N, Make_Defining_Identifier (GN (The_Pragma)));
      Set_Argument_List (N, Argument_List);
      return N;
   end Make_Pragma_Statement;

   --------------------------------
   -- Make_Protected_Object_Spec --
   --------------------------------

   function Make_Protected_Object_Spec
     (Defining_Identifier : Node_Id;
      Visible_Part        : List_Id;
      Private_Part        : List_Id;
      Parent              : Node_Id := Current_Package;
      Is_Type             : Boolean := False) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Protected_Object_Spec);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Visible_Part (N, Visible_Part);
      Set_Private_Part (N, Private_Part);
      Set_Parent (N, Parent);
      Set_Is_Type (N, Is_Type);
      return N;
   end Make_Protected_Object_Spec;

   --------------------------------
   -- Make_Protected_Object_Body --
   --------------------------------

   function Make_Protected_Object_Body
     (Defining_Identifier : Node_Id;
      Statements          : List_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Protected_Object_Body);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Statements (N, Statements);
      return N;
   end Make_Protected_Object_Body;

   -------------------------------
   -- Make_Qualified_Expression --
   -------------------------------

   function Make_Qualified_Expression
     (Subtype_Mark : Node_Id;
      Aggregate    : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Qualified_Expression);
      Set_Subtype_Mark (N, Subtype_Mark);
      Set_Aggregate (N, Aggregate);
      return N;
   end Make_Qualified_Expression;

   --------------------------
   -- Make_Raise_Statement --
   --------------------------

   function Make_Raise_Statement
     (Raised_Error : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Raise_Statement);
      Set_Raised_Error (N, Raised_Error);
      return N;
   end Make_Raise_Statement;

   ---------------------------
   -- Make_Range_Constraint --
   ---------------------------

   function Make_Range_Constraint
     (First      : Node_Id;
      Last       : Node_Id;
      Index_Type : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Range_Constraint);
      Set_First (N, First);
      Set_Last (N, Last);
      Set_Index_Type (N, Index_Type);
      return N;
   end Make_Range_Constraint;

   ---------------------------
   -- Make_Record_Aggregate --
   ---------------------------

   function Make_Record_Aggregate (L : List_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Record_Aggregate);
      Set_Component_Association_List (N, L);
      return N;
   end Make_Record_Aggregate;

   ----------------------------
   -- Make_Record_Definition --
   ----------------------------

   function Make_Record_Definition (Component_List : List_Id) return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Record_Definition);
      Set_Component_List (N, Component_List);
      return N;
   end Make_Record_Definition;

   ---------------------------------
   -- Make_Record_Type_Definition --
   ---------------------------------

   function Make_Record_Type_Definition
     (Record_Definition : Node_Id;
      Is_Abstract_Type  : Boolean := False;
      Is_Tagged_Type    : Boolean := False;
      Is_Limited_Type   : Boolean := False) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Record_Type_Definition);
      Set_Is_Abstract_Type (N, Is_Abstract_Type);
      Set_Is_Tagged_Type (N, Is_Tagged_Type);
      Set_Is_Limited_Type (N, Is_Limited_Type);
      Set_Record_Definition (N, Record_Definition);
      return N;
   end Make_Record_Type_Definition;

   ---------------------------
   -- Make_Return_Statement --
   ---------------------------

   function Make_Return_Statement (Expression : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Return_Statement);
      Set_Expression (N, Expression);
      return N;
   end Make_Return_Statement;

   -----------------------------
   -- Make_Selected_Component --
   -----------------------------

   function Make_Selected_Component
     (Prefix        : Node_Id;
      Selector_Name : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Selected_Component);
      Set_Prefix (N, Prefix);
      Set_Selector_Name (N, Selector_Name);
      return N;
   end Make_Selected_Component;

   --------------------------
   -- Make_Subprogram_Call --
   --------------------------

   function Make_Subprogram_Call
     (Defining_Identifier   : Node_Id;
      Actual_Parameter_Part : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Subprogram_Call);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Actual_Parameter_Part (N, Actual_Parameter_Part);
      return N;
   end Make_Subprogram_Call;

   ------------------------------------
   -- Make_Subprogram_Implementation --
   ------------------------------------

   function Make_Subprogram_Implementation
     (Specification        : Node_Id;
      Declarations         : List_Id;
      Statements           : List_Id;
      Aspect_Specification : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Subprogram_Implementation);
      Set_Specification (N, Specification);
      if Present (Aspect_Specification) then
         Set_Aspect_Specification (ADN.Specification (N),
                                   Aspect_Specification);
      end if;

      Set_Declarations (N, Declarations);
      Set_Statements (N, Statements);
      return N;
   end Make_Subprogram_Implementation;

   -----------------------------------
   -- Make_Subprogram_Specification --
   -----------------------------------

   function Make_Subprogram_Specification
     (Defining_Identifier     : Node_Id;
      Parameter_Profile       : List_Id;
      Return_Type             : Node_Id := No_Node;
      Aspect_Specification    : Node_Id := No_Node;
      Parent                  : Node_Id := Current_Package;
      Renamed_Subprogram      : Node_Id := No_Node;
      Instantiated_Subprogram : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Subprogram_Specification);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Parameter_Profile (N, Parameter_Profile);
      Set_Return_Type (N, Return_Type);
      Set_Aspect_Specification (N, Aspect_Specification);
      Set_Parent (N, Parent);
      Set_Renamed_Entity (N, Renamed_Subprogram);
      Set_Instantiated_Entity (N, Instantiated_Subprogram);
      return N;
   end Make_Subprogram_Specification;

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
         P : Node_Id;

      begin
         P := Parent_Unit_Name (S);
         if Present (P) then
            Get_Scoped_Name_String (P);
            Add_Char_To_Name_Buffer ('.');
         end if;
         Get_Name_String_And_Append (Name (Defining_Identifier (S)));
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

   --------------------
   -- Make_Used_Type --
   --------------------

   function Make_Used_Type (The_Used_Type : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Used_Type);

      Set_The_Used_Entity (N, The_Used_Type);
      return N;
   end Make_Used_Type;

   -------------------------
   -- Make_Withed_Package --
   -------------------------

   function Make_Withed_Package
     (Defining_Identifier : Node_Id;
      Used                : Boolean := False;
      Warnings_Off        : Boolean := False;
      Elaborated          : Boolean := False) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Withed_Package);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Used (N, Used);
      Set_Warnings_Off (N, Warnings_Off);
      Set_Elaborated (N, Elaborated);
      return N;
   end Make_Withed_Package;

   -----------------------
   -- Make_Used_Package --
   -----------------------

   function Make_Used_Package (The_Used_Package : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Used_Package);

      Set_The_Used_Entity (N, The_Used_Package);
      return N;
   end Make_Used_Package;

   -----------------------
   -- Make_Variant_Part --
   -----------------------

   function Make_Variant_Part
     (Discriminant : Node_Id;
      Variant_List : List_Id) return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Variant_Part);
      Set_Variants (N, Variant_List);
      Set_Discriminant (N, Discriminant);
      return N;
   end Make_Variant_Part;

   -------------------------
   -- Make_Comment_Header --
   -------------------------

   procedure Make_Comment_Header (Package_Header : List_Id) is
      N : Node_Id;
   begin
      --  Appending the comment header lines to the package header

      Set_Str_To_Name_Buffer
        ("------------------------------------------------------");
      N := Make_Ada_Comment (Name_Find, False);
      Append_Node_To_List (N, Package_Header);

      Set_Str_To_Name_Buffer
        ("This file was automatically generated by Ocarina  --");
      N := Make_Ada_Comment (Name_Find);
      Append_Node_To_List (N, Package_Header);

      Set_Str_To_Name_Buffer
        (SCM_Version.all);
      N := Make_Ada_Comment (Name_Find);
      Append_Node_To_List (N, Package_Header);

      Set_Str_To_Name_Buffer
        ("Do NOT hand-modify this file, as your             --");
      N := Make_Ada_Comment (Name_Find);
      Append_Node_To_List (N, Package_Header);

      Set_Str_To_Name_Buffer
        ("changes will be lost when you re-run Ocarina      --");
      N := Make_Ada_Comment (Name_Find);
      Append_Node_To_List (N, Package_Header);

      Set_Str_To_Name_Buffer
        ("------------------------------------------------------");
      N := Make_Ada_Comment (Name_Find, False);
      Append_Node_To_List (N, Package_Header);

   end Make_Comment_Header;

   -----------------
   -- Next_N_Node --
   -----------------

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id is
      Result : Node_Id := N;
   begin
      for I in 1 .. Num loop
         Result := Next_Node (Result);
      end loop;

      return Result;
   end Next_N_Node;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return List_Id
   is
      N : Node_Id;

   begin
      Entries.Increment_Last;
      N                 := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         Set_Loc (N, Loc (From));
      else
         Set_Loc (N, No_Location);
      end if;
      return List_Id (N);
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N                 := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);

      if Present (From) then
         Set_Loc (N, AAN.Loc (From));
      else
         Set_Loc (N, No_Location);
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
         --  Marking the token image as a keyword for fast searching
         --  purpose, we add the prefix to avoir collision with other
         --  languages keywords

         Set_Str_To_Name_Buffer (Image (T));
         Name := Name_Find;
         Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
         Set_Name_Table_Byte (Name, Byte (Token_Type'Pos (T) + 1));

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

   --------------------------
   -- Qualified_Designator --
   --------------------------

   function Qualified_Designator (P : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Designator);
      Set_Defining_Identifier (N, Make_Defining_Identifier (Name (P)));
      if Present (Parent_Unit_Name (P)) then
         Set_Homogeneous_Parent_Unit_Name
           (N,
            Qualified_Designator (Parent_Unit_Name (P)));
      else
         Set_Homogeneous_Parent_Unit_Name (N, No_Node);
      end if;

      return N;
   end Qualified_Designator;

   --------------------------------------------
   -- Remove_Anonymous_Array_Type_Definition --
   --------------------------------------------

   function Remove_Anonymous_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id;
      Aliased_Present      : Boolean := False;
      Variable_Name        : Node_Id;
      Is_Full_Type         : Boolean := False) return Node_Id
   is
      N                : Node_Id;
      R                : Node_Id;
      Comp             : Node_Id          := No_Node;
      T_Def            : Node_Id;
      Tmp_Id           : Node_Id;
      List_Constraints : constant List_Id := New_List (K_List_Id);
      List_Comp        : constant List_Id := New_List (K_List_Id);

   begin
      R := First_Node (Range_Constraints);

      loop
         case Kind (R) is
            when K_Defining_Identifier =>
               Append_Node_To_List (R, List_Constraints);

            when K_Range_Constraint =>
               N := Create_Subtype_From_Range_Constraint (R);

               --  if N is not a full type then it's an
               --  unconstraint array type. In this case,
               --  we need to add the the type to the list,
               --  not only its identifier.

               if Kind (N) = K_Full_Type_Declaration then
                  Append_Node_To_List
                    (Defining_Identifier (N),
                     List_Constraints);
               else
                  Append_Node_To_List (N, List_Constraints);
               end if;

            when others =>
               raise Program_Error;
         end case;

         R := Next_Node (R);
         exit when No (R);
      end loop;

      case Kind (Component_Definition) is
         when K_Defining_Identifier =>
            Comp := Component_Definition;

         when K_Indexed_Component =>
            R := First_Node (Expressions (Component_Definition));
            loop
               N := Create_Subtype_From_Range_Constraint (R);

               --  if N is not a full type then it's an
               --  unconstraint array type. In this case,
               --  we need to add the the type to the list,
               --  not only its identifier.

               if Kind (N) = K_Full_Type_Declaration then
                  Append_Node_To_List (Defining_Identifier (N), List_Comp);
               else
                  Append_Node_To_List (N, List_Comp);
               end if;

               R := Next_Node (R);
               exit when No (R);
            end loop;

            Comp :=
              Make_Indexed_Component
                (Prefix      => Nodes.Prefix (Component_Definition),
                 Expressions => List_Comp);

            --  Create a unique name for component type

            Tmp_Id :=
              Make_Defining_Identifier
                (Create_Unique_Identifier (Name (Variable_Name), "Component"));

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => Tmp_Id,
                 Type_Definition     => Comp,
                 Is_Subtype          => True);

            Comp := Defining_Identifier (N);

            if Get_Name_Table_Info (Name (Tmp_Id)) = Int (No_Node) then
               Set_Name_Table_Info (Name (Tmp_Id), Int (Tmp_Id));
               Append_Node_To_Current_Package (N);
            end if;

         when others =>
            Comp := Component_Definition;
      end case;

      N :=
        Make_Array_Type_Definition (List_Constraints, Comp, Aliased_Present);

      --  Add a full type node, only if the caller of
      --  Remove_Anonymous_Array_Type_Definition is Make_Object_Declaration.

      if not Is_Full_Type then

         Tmp_Id :=
           Make_Defining_Identifier
             (Create_Unique_Identifier (Name (Variable_Name), "Array"));

         --  We don't call Make_Full_Type_Declaration in order to
         --  avoid recursive calls.

         T_Def := New_Node (K_Full_Type_Declaration);
         Set_Defining_Identifier (T_Def, Tmp_Id);
         Set_Corresponding_Node (Tmp_Id, T_Def);
         Set_Type_Definition (T_Def, N);
         Set_Discriminant_Spec (T_Def, No_Node);
         Set_Parent (T_Def, Current_Package);
         Set_Is_Subtype (T_Def, False);
         Set_Name_Table_Info (Name (Tmp_Id), Int (No_Node));

         if Get_Name_Table_Info (Name (Tmp_Id)) = Int (No_Node) then
            Set_Name_Table_Info (Name (Tmp_Id), Int (Tmp_Id));
            Append_Node_To_Current_Package (T_Def);
         end if;
         N := Defining_Identifier (T_Def);
      end if;

      return N;
   end Remove_Anonymous_Array_Type_Definition;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;

   begin
      C := First_Node (L);
      if C = E then
         Set_First_Node (L, Next_Node (E));
         if Last_Node (L) = E then
            Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if Next_Node (C) = E then
               Set_Next_Node (C, Next_Node (E));
               if Last_Node (L) = E then
                  Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   --------------------------------------
   -- Set_Homogeneous_Parent_Unit_Name --
   --------------------------------------

   procedure Set_Homogeneous_Parent_Unit_Name
     (Child  : Node_Id;
      Parent : Node_Id)
   is
   begin
      pragma Assert
        (ADN.Kind (Child) = K_Defining_Identifier
         or else ADN.Kind (Child) = K_Designator);

      pragma Assert
        (Parent = No_Node
         or else ADN.Kind (Parent) = K_Defining_Identifier
         or else ADN.Kind (Parent) = K_Designator);

      case ADN.Kind (Child) is

         when K_Defining_Identifier =>
            if Parent = No_Node then
               Set_Parent_Unit_Name (Child, Parent);
            elsif ADN.Kind (Parent) = K_Defining_Identifier then
               Set_Parent_Unit_Name (Child, Parent);
            elsif ADN.Kind (Parent) = K_Designator then
               Set_Parent_Unit_Name (Child, Defining_Identifier (Parent));
            else
               raise Program_Error;
            end if;

         when K_Designator =>
            if Parent = No_Node then
               Set_Parent_Unit_Name (Child, Parent);
               if Present (Defining_Identifier (Child)) then
                  Set_Parent_Unit_Name (Defining_Identifier (Child), Parent);
               end if;
            elsif ADN.Kind (Parent) = K_Defining_Identifier then
               Set_Parent_Unit_Name
                 (Child,
                  Defining_Identifier_To_Designator (Parent));
               if Present (Defining_Identifier (Child)) then
                  Set_Parent_Unit_Name (Defining_Identifier (Child), Parent);
               end if;
            elsif ADN.Kind (Parent) = K_Designator then
               Set_Parent_Unit_Name (Child, Parent);
               if Present (Defining_Identifier (Child)) then
                  Set_Parent_Unit_Name
                    (Defining_Identifier (Child),
                     Defining_Identifier (Parent));
               end if;
            else
               raise Program_Error;
            end if;

         when others =>
            raise Program_Error;

      end case;
   end Set_Homogeneous_Parent_Unit_Name;

   -------------------
   -- Set_Main_Body --
   -------------------

   procedure Set_Main_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Subprogram_Implementation (Main_Subprogram (X));
   end Set_Main_Body;

   -------------------
   -- Set_Main_Spec --
   -------------------

   procedure Set_Main_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Subprogram_Specification (Main_Subprogram (X));
   end Set_Main_Spec;

   --------------------------
   -- Set_Marshallers_Spec --
   --------------------------

   procedure Set_Marshallers_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Marshallers_Package (X));
   end Set_Marshallers_Spec;

   --------------------------
   -- Set_Marshallers_Body --
   --------------------------

   procedure Set_Marshallers_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Marshallers_Package (X));
   end Set_Marshallers_Body;

   -----------------------
   -- Set_Activity_Body --
   -----------------------

   procedure Set_Activity_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Activity_Package (X));
   end Set_Activity_Body;

   -----------------------
   -- Set_Activity_Spec --
   -----------------------

   procedure Set_Activity_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Activity_Package (X));
   end Set_Activity_Spec;

   ------------------
   -- Set_Job_Body --
   ------------------

   procedure Set_Job_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Job_Package (X));
   end Set_Job_Body;

   ------------------
   -- Set_Job_Spec --
   ------------------

   procedure Set_Job_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Job_Package (X));
   end Set_Job_Spec;

   ------------------------
   -- Set_Transport_Body --
   ------------------------

   procedure Set_Transport_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Transport_Package (X));
   end Set_Transport_Body;

   ------------------------
   -- Set_Transport_Spec --
   ------------------------

   procedure Set_Transport_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Transport_Package (X));
   end Set_Transport_Spec;

   --------------------
   -- Set_Types_Body --
   --------------------

   procedure Set_Types_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Types_Package (X));
   end Set_Types_Body;

   --------------------
   -- Set_Types_Spec --
   --------------------

   procedure Set_Types_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Types_Package (X));
   end Set_Types_Spec;

   --------------------------
   -- Set_Subprograms_Body --
   --------------------------

   procedure Set_Subprograms_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Subprograms_Package (X));
   end Set_Subprograms_Body;

   --------------------------
   -- Set_Subprograms_Spec --
   --------------------------

   procedure Set_Subprograms_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Subprograms_Package (X));
   end Set_Subprograms_Spec;

   -------------------------
   -- Set_Deployment_Spec --
   -------------------------

   procedure Set_Deployment_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Deployment_Package (X));
   end Set_Deployment_Spec;

   ---------------------
   -- Set_Naming_Spec --
   ---------------------

   procedure Set_Naming_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Naming_Package (X));
   end Set_Naming_Spec;

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name (N : Name_Id) return Name_Id is
      First     : Natural := 1;
      Name      : Name_Id;
      Test_Name : Name_Id;
      V         : Byte;
   begin
      Get_Name_String (Normalize_Name (N));
      while First <= Name_Len and then Name_Buffer (First) = '_' loop
         First := First + 1;
      end loop;

      for I in First .. Name_Len loop
         if Name_Buffer (I) = '_'
           and then I < Name_Len
           and then Name_Buffer (I + 1) = '_'
         then
            Name_Buffer (I + 1) := 'U';
         end if;
      end loop;

      if Name_Buffer (Name_Len) = '_' then
         Add_Char_To_Name_Buffer ('U');
      end if;
      Name := Name_Find;

      --  If the identifier collides with an Ada reserved word insert
      --  "AADL_" string before the identifier.

      Test_Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
      V         := Get_Name_Table_Byte (Test_Name);
      if V > 0 then
         Set_Str_To_Name_Buffer ("AADL_");
         Get_Name_String_And_Append (Name);
         Name := Name_Find;
      end if;

      return Name;
   end To_Ada_Name;

   ------------------------
   -- Extract_Designator --
   ------------------------

   function Extract_Designator
     (N               : Node_Id;
      Add_With_Clause : Boolean := True) return Node_Id
   is
      P  : Node_Id;
      D  : Node_Id := No_Node;
      X  : Node_Id := N;
      FE : Node_Id;

   begin
      case Kind (N) is
         when K_Full_Type_Declaration | K_Subprogram_Specification =>
            P  := Parent (X);
            FE := Frontend_Node (X);

         when K_Object_Declaration | K_Exception_Declaration =>
            P  := Parent (X);
            FE := Frontend_Node (X);

         when K_Package_Specification =>
            X  := Package_Declaration (N);
            P  := Parent (X);
            FE := Frontend_Node (Distributed_Application_Unit (X));

         when K_Package_Declaration =>
            P  := Parent (N);
            FE := Frontend_Node (Distributed_Application_Unit (X));

         when K_Designator =>
            return Copy_Designator (N);

         when K_Protected_Object_Spec =>
            P := Parent (N);

         when K_Package_Instantiation =>
            P := Parent (X);

         when others =>
            raise Program_Error;
      end case;

      D :=
        Defining_Identifier_To_Designator
          (N           => Defining_Identifier (X),
           Keep_Parent => False);
      Set_Frontend_Node (D, FE);

      if No (P) then
         return D;
      end if;

      --  This handles the particular case of package instanciations

      if Kind (N) = K_Full_Type_Declaration
        and then Present (Parent_Unit_Name (Defining_Identifier (N)))
        and then
          Kind
            (Corresponding_Node (Parent_Unit_Name (Defining_Identifier (N)))) =
          K_Package_Instantiation
      then
         Set_Homogeneous_Parent_Unit_Name
           (D,
            Parent_Unit_Name (Defining_Identifier (N)));
         P := Extract_Designator (P);
      else
         Set_Homogeneous_Parent_Unit_Name (D, Extract_Designator (P, False));
         P := Parent_Unit_Name (D);
      end if;

      --  Adding the with clause in the case the parent is a package

      if Add_With_Clause
        and then Present (P)
        and then Kind (P) /= K_Protected_Object_Spec
      then
         Add_With_Package (P);
      end if;

      return D;
   end Extract_Designator;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (N : Name_Id) return Name_Id is
      Pos : Natural := 0;
   begin
      Get_Name_String (N);

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Pos := J;
            exit;
         end if;
      end loop;

      if Pos = 0 or else Pos = 1 then
         Display_Error
           ("""" &
            Get_Name_String (N) &
            """ is not an Ada fully qualified entity name",
            Fatal => True);
      end if;

      if To_Lower (Name_Buffer (1 .. Pos - 1)) = "standard" then
         return No_Name;
      end if;

      Set_Str_To_Name_Buffer (Name_Buffer (1 .. Pos - 1));
      return Name_Find;
   end Unit_Name;

   ----------------
   -- Local_Name --
   ----------------

   function Local_Name (N : Name_Id) return Name_Id is
      Pos : Natural := 0;
   begin
      Get_Name_String (N);

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Pos := J;
            exit;
         end if;
      end loop;

      if Pos = Name_Len or else Pos = Name_Len - 1 then
         Display_Error
           ("""" &
            Get_Name_String (N) &
            """ is not an Ada fully qualified entity name",
            Fatal => True);
      end if;

      Set_Str_To_Name_Buffer (Name_Buffer (Pos + 1 .. Name_Len));
      return Name_Find;
   end Local_Name;

   ----------------------------
   -- Conventional_Base_Name --
   ----------------------------

   function Conventional_Base_Name (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);

      --  Lower and replace all '.' by '-'

      for Index in 1 .. Name_Len loop
         if Name_Buffer (Index) = '.' then
            Name_Buffer (Index) := '-';
         else
            Name_Buffer (Index) := To_Lower (Name_Buffer (Index));
         end if;
      end loop;

      return Name_Find;
   end Conventional_Base_Name;

end Ocarina.Backends.Ada_Tree.Nutils;
