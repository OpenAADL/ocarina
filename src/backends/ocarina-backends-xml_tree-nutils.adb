------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . X M L _ T R E E . N U T I L S      --
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

with GNAT.Table;

with Charset;   use Charset;
with Locations; use Locations;
with Ocarina.Namet;     use Ocarina.Namet;
with Utils;     use Utils;

with Ocarina.Backends.Utils;
with Ocarina.ME_AADL.AADL_Tree.Nodes; use Ocarina.ME_AADL.AADL_Tree.Nodes;
use Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Values;     use Ocarina.Backends.XML_Values;

package body Ocarina.Backends.XML_Tree.Nutils is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   Keyword_Suffix : constant String := "%XML";
   --  Used to mark XML keywords and avoid collision with other languages

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
      Last := XTN.Last_Node (L);
      if No (Last) then
         XTN.Set_First_Node (L, E);
      else
         XTN.Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         XTN.Set_Last_Node (L, Last);
         Last := XTN.Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   -----------------------
   -- Insert_After_Node --
   -----------------------

   procedure Insert_After_Node (E : Node_Id; N : Node_Id) is
      Next : constant Node_Id := XTN.Next_Node (N);
   begin
      XTN.Set_Next_Node (N, E);
      XTN.Set_Next_Node (E, Next);
   end Insert_After_Node;

   ------------------------
   -- Insert_Before_Node --
   ------------------------

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id) is
      Entity : Node_Id;
   begin
      Entity := XTN.First_Node (L);
      if Entity = N then
         XTN.Set_Next_Node (E, Entity);
         XTN.Set_First_Node (L, E);
      else
         while Present (Entity) loop
            exit when XTN.Next_Node (Entity) = N;
            Entity := XTN.Next_Node (Entity);
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
      case XTN.Kind (N) is
         when K_Defining_Identifier =>
            C := New_Node (K_Defining_Identifier);
            XTN.Set_Name (C, XTN.Name (N));
            XTN.Set_Corresponding_Node (C, XTN.Corresponding_Node (N));

         when K_Literal =>
            C := New_Node (K_Literal);
            XTN.Set_Value (C, XTN.Value (N));

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
      C := Make_XML_Comment (M);
      return C;
   end Message_Comment;

   ---------------------
   -- Message_Comment --
   ---------------------

   function Message_Comment (M : String) return Node_Id is
      C : Node_Id;
   begin
      Set_Str_To_Name_Buffer (M);
      C := Make_XML_Comment (Name_Find);
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
      New_Token (Tok_Slash, "/");
      New_Token (Tok_Less, "<");
      New_Token (Tok_Equal, "=");
      New_Token (Tok_Greater, ">");

   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (XTN.First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List_Id) return Natural is
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := XTN.First_Node (L);

         while Present (N) loop
            C := C + 1;
            N := XTN.Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   --------------------
   -- Make_C_Comment --
   --------------------

   function Make_XML_Comment (N : Name_Id) return Node_Id is
      C : Node_Id;
   begin
      C := New_Node (K_XML_Comment);
      Set_Defining_Identifier (C, New_Node (K_Defining_Identifier));
      XTN.Set_Name (Defining_Identifier (C), N);
      return C;
   end Make_XML_Comment;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Defining_Identifier);
      XTN.Set_Name (N, Name);
      return N;
   end Make_Defining_Identifier;

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

   -----------------
   -- Next_N_Node --
   -----------------

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id is
      Result : Node_Id := N;
   begin
      for I in 1 .. Num loop
         Result := XTN.Next_Node (Result);
      end loop;

      return Result;
   end Next_N_Node;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : XTN.Node_Kind;
      From : Node_Id := No_Node) return List_Id
   is
      N : Node_Id;

   begin
      XTN.Entries.Increment_Last;
      N                     := XTN.Entries.Last;
      XTN.Entries.Table (N) := XTN.Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         XTN.Set_Loc (N, XTN.Loc (From));
      else
         XTN.Set_Loc (N, No_Location);
      end if;
      return List_Id (N);
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : XTN.Node_Kind;
      From : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      XTN.Entries.Increment_Last;
      N                     := XTN.Entries.Last;
      XTN.Entries.Table (N) := XTN.Default_Node;
      XTN.Set_Kind (N, Kind);

      if Present (From) then
         XTN.Set_Loc (N, ATN.Loc (From));
      else
         XTN.Set_Loc (N, No_Location);
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
           (Name, Ocarina.Types.Byte (Token_Type'Pos (T) + 1));

         Set_Str_To_Name_Buffer (Image (T));
      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Token_Image (T) := Name_Find;
   end New_Token;

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
      C := XTN.First_Node (L);
      if C = E then
         XTN.Set_First_Node (L, XTN.Next_Node (E));
         if XTN.Last_Node (L) = E then
            XTN.Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if XTN.Next_Node (C) = E then
               XTN.Set_Next_Node (C, XTN.Next_Node (E));
               if XTN.Last_Node (L) = E then
                  XTN.Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := XTN.Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   ---------------
   -- To_C_Name --
   ---------------

   function To_XML_Name (N : Name_Id) return Name_Id is
      First     : Natural := 1;
      Name      : Name_Id;
      Test_Name : Name_Id;
      V         : Ocarina.Types.Byte;
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

      return To_Lower (Name);
   end To_XML_Name;

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

   -------------------
   -- Make_XML_File --
   -------------------

   function Make_XML_File
     (Identifier : Node_Id;
      DTD        : Node_Id := No_Node) return Node_Id
   is
      File         : Node_Id;
      The_XML_Node : Node_Id;

   begin
      File         := New_Node (K_XML_File);
      The_XML_Node := New_Node (XTN.K_XML_Node);

      Set_Defining_Identifier (File, Identifier);
      Set_Corresponding_Node (Identifier, File);

      XTN.Set_Is_HTML (File, False);
      XTN.Set_Root_Node (File, The_XML_Node);
      if Present (DTD) then
         XTN.Set_XML_DTD (File, DTD);
      end if;

      return File;
   end Make_XML_File;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal (Value : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Literal);
      XTN.Set_Value (N, Value);
      return N;
   end Make_Literal;

   -------------------
   -- Make_XML_Node --
   -------------------

   function Make_XML_Node
     (Name_String : String            := "";
      Name_Nameid : Name_Id           := No_Name;
      Kind        : XML_New_Node_Kind := K_String) return Node_Id
   is
      N : Node_Id;
      L : List_Id;
      K : List_Id;
   begin
      N := New_Node (K_XML_Node);

      if Kind = K_String then
         Set_Str_To_Name_Buffer (Name_String);
         XTN.Set_Defining_Identifier (N, Make_Defining_Identifier (Name_Find));
      else
         XTN.Set_Defining_Identifier
           (N,
            Make_Defining_Identifier (Name_Nameid));
      end if;

      L := New_List (XTN.K_List_Id);
      XTN.Set_Items (N, L);

      K := New_List (XTN.K_List_Id);
      XTN.Set_Subitems (N, K);

      return N;
   end Make_XML_Node;

   ----------------------
   -- Make_Assignement --
   ----------------------

   function Make_Assignement
     (Left  : Node_Id;
      Right : Node_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (XTN.K_Assignement);
      XTN.Set_Left_Expression (N, Left);
      XTN.Set_Right_Expression (N, Right);
      return N;
   end Make_Assignement;

   --------------------
   -- Make_Container --
   --------------------

   function Make_Container (Content : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (XTN.K_Container);
      XTN.Set_Content (N, Content);
      return N;
   end Make_Container;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute (Key : String; Value : String; N : Node_Id) is
      R : Node_Id;
      Q : Node_Id;
   begin
      Set_Str_To_Name_Buffer (Key);
      R := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer (Value);
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));
   end Add_Attribute;

   procedure Add_Attribute (Key : String; Value : Value_Id; N : Node_Id) is
   begin
      Add_Attribute (Key, Image (Value), N);
   end Add_Attribute;

end Ocarina.Backends.XML_Tree.Nutils;
