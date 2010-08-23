------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . M A S T _ T R E E . N U T I L S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, European Space Agency (ESA).              --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Table;

with Charset;   use Charset;
with Locations; use Locations;
with Namet;     use Namet;
--  with Utils;     use Utils;

--  with Ocarina.Backends.Utils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
use Ocarina.ME_AADL.AADL_Tree.Nodes;
--  use Ocarina.Backends.Utils;

package body Ocarina.Backends.MAST_Tree.Nutils is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package MTN renames Ocarina.Backends.MAST_Tree.Nodes;

   Keyword_Suffix : constant String := "%MAST";

   type Entity_Stack_Entry is record
      Current_File    : Node_Id;
      Current_Entity  : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is
      new GNAT.Table (Entity_Stack_Entry, Int, No_Depth + 1, 10, 10);

   use Entity_Stack;

   ------------------------
   -- Add_Prefix_To_Name --
   ------------------------

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id)
     return Name_Id
   is
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Get_Name_String_And_Append (Name);
      return Name_Find;
   end Add_Prefix_To_Name;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Entity_Stack.Init;

      Initialized := False;
   end Reset;

   ------------------------
   -- Add_Suffix_To_Name --
   ------------------------

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id
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
      Name   : Name_Id)
     return Name_Id
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
      Last := MTN.Last_Node (L);
      if No (Last) then
         MTN.Set_First_Node (L, E);
      else
         MTN.Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         MTN.Set_Last_Node (L, Last);
         Last := MTN.Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   -----------------------
   -- Insert_After_Node --
   -----------------------

   procedure Insert_After_Node (E : Node_Id; N : Node_Id) is
      Next : constant Node_Id := MTN.Next_Node (N);
   begin
      MTN.Set_Next_Node (N, E);
      MTN.Set_Next_Node (E, Next);
   end Insert_After_Node;

   ------------------------
   -- Insert_Before_Node --
   ------------------------

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id) is
      Entity : Node_Id;
   begin
      Entity := MTN.First_Node (L);
      if Entity = N then
         MTN.Set_Next_Node (E, Entity);
         MTN.Set_First_Node (L, E);
      else
         while Present (Entity) loop
            exit when MTN.Next_Node (Entity) = N;
            Entity := MTN.Next_Node (Entity);
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
      case MTN.Kind (N) is
         when K_Literal =>
            C := New_Node (K_Literal);
            MTN.Set_Value (C, MTN.Value (N));

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
      New_Token (Tok_Assign, "=>");
      New_Token (Tok_Left_Paren, "(");
      New_Token (Tok_Right_Paren, ")");
      New_Token (Tok_Semicolon, ";");

   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (MTN.First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List_Id) return Natural is
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := MTN.First_Node (L);

         while Present (N) loop
            C := C + 1;
            N := MTN.Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Name         : Name_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Defining_Identifier);
      MTN.Set_Name (N, Name);
      return N;
   end Make_Defining_Identifier;

   ------------------
   -- Make_List_Id --
   ------------------

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node)
     return List_Id
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
         Result := MTN.Next_Node (Result);
      end loop;

      return Result;
   end Next_N_Node;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : MTN.Node_Kind;
      From : Node_Id := No_Node)
     return List_Id is
      N : Node_Id;

   begin
      MTN.Entries.Increment_Last;
      N := MTN.Entries.Last;
      MTN.Entries.Table (N) := MTN.Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         MTN.Set_Loc  (N, MTN.Loc (From));
      else
         MTN.Set_Loc  (N, No_Location);
      end if;
      return List_Id (N);
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : MTN.Node_Kind;
      From : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;
   begin
      MTN.Entries.Increment_Last;
      N := MTN.Entries.Last;
      MTN.Entries.Table (N) := MTN.Default_Node;
      MTN.Set_Kind (N, Kind);

      if Present (From) then
         MTN.Set_Loc (N, ATN.Loc (From));
      else
         MTN.Set_Loc (N, No_Location);
      end if;

      return N;
   end New_Node;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token
     (T : Token_Type;
      I : String := "")
   is
      Name : Name_Id;
   begin
      if T in Keyword_Type then
         --  Marking the token image as a keyword for fas searching
         --  purpose, we add the prefix to avoir collision with other
         --  languages keywords

         Set_Str_To_Name_Buffer (Image (T));
         Name := Name_Find;
         Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
         Set_Name_Table_Byte (Name, Types.Byte (Token_Type'Pos (T) + 1));

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
      C := MTN.First_Node (L);
      if C = E then
         MTN.Set_First_Node (L, MTN.Next_Node (E));
         if MTN.Last_Node (L) = E then
            MTN.Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if MTN.Next_Node (C) = E then
               MTN.Set_Next_Node (C, MTN.Next_Node (E));
               if MTN.Last_Node (L) = E then
                  MTN.Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := MTN.Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

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

   --------------------
   -- Make_MAST_File --
   --------------------

   function Make_MAST_File (Identifier : Node_Id) return Node_Id is
      File : Node_Id;
   begin
      File := New_Node (K_MAST_File);
      Set_Defining_Identifier (File, Identifier);
      Set_Corresponding_Node (Identifier, File);

      return File;
   end Make_MAST_File;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal (Value : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Literal);
      MTN.Set_Value (N, Value);
      return N;
   end Make_Literal;

   --------------------
   -- Make_Container --
   --------------------

   function Make_Container (Content : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Container);
      MTN.Set_Content (N, Content);
      return N;
   end Make_Container;

end Ocarina.Backends.MAST_Tree.Nutils;
