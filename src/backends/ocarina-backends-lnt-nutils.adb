------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . L N T . N U T I L S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

with Ada.Characters.Handling;

with GNAT.Table;

with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.Types; use Ocarina.Types;
with Utils;         use Utils;

package body Ocarina.Backends.LNT.Nutils is

   use OLNT;

   type Entity_Stack_Entry is record
      Current_Package : Node_Id;
      Current_Entity  : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is
      new GNAT.Table (Entity_Stack_Entry, Int, No_Depth + 1, 10, 10);

   use Entity_Stack;

   --------------
   -- New_Node --
   --------------

   function New_Node (Kind : OLNT.Node_Kind; Loc : Locations.Location)
    return Node_Id is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      Set_Loc  (N, Loc);

      return N;
   end New_Node;

   function New_Node (Kind : OLNT.Node_Kind) return Types.Node_Id is

      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);

      return N;
   end New_Node;

   --------------------
   -- Make_Container --
   --------------------

   function Make_Node_Container
     (Item       : Node_Id;
      Extra_Item : Node_Id := No_Node)
     return Node_Id
   is
      Container : constant Node_Id := New_Node
        (K_Node_Container,
         Loc (Item));
   begin
      Set_Item (Container, Item);
      Set_Extra_Item (Container, Extra_Item);

      return Container;
   end Make_Node_Container;

   -------------------------
   -- Append_Node_To_List --
   -------------------------
   procedure Append_Node_To_List (E : Types.Node_Id; L : Types.List_Id) is

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

   -------------------------
   -- Append_List_To_List --
   -------------------------

   procedure Append_List_To_List (S : Types.List_Id;
                                  D : in out Types.List_Id)
   is
   begin
      if Present (D) then
         Append_Node_To_List (First_Node (S), D);
      else
         --  This is highly dangerous. Append should be a copy
         --  operation.

         D := S;
      end if;
   end Append_List_To_List;

   ---------------------------
   --  Push_Node_Into_List  --
   ---------------------------

   procedure Push_Node_Into_List (E : Types.Node_Id; L : Types.List_Id) is

      First : Node_Id;
   begin
      if Is_Empty (L) then
         Append_Node_To_List (E, L);
      else
         First := First_Node (L);
         Set_First_Node (L, E);
         Set_Next_Node (E, First);

         if Next_Node (First) = No_Node then
            Set_Last_Node (L, First);
         end if;
      end if;
   end Push_Node_Into_List;

   ---------------------------
   -- Delete_Node_From_List --
   ---------------------------

   procedure Delete_Node_From_List (E : Types.Node_Id; L : Types.List_Id) is

      Node_Iter : Node_Id;
   begin
      if E = First_Node (L) then
         Set_First_Node (L, Next_Node (E));
         if Next_Node (E) = No_Node then
            Set_Last_Node (L, No_Node);
         end if;
      else
         Node_Iter := First_Node (L);
         while Present (Node_Iter) loop
            if Next_Node (Node_Iter) = E then
               if Next_Node (E) /= No_Node then
                  Set_Next_Node (Node_Iter,
                                 Next_Node (E));
               else
                  Set_Next_Node (Node_Iter, No_Node);
                  Set_Last_Node (L, Node_Iter);
               end if;
            end if;

            Node_Iter := Next_Node (Node_Iter);
         end loop;
      end if;
   end Delete_Node_From_List;

   --------------
   -- New_List --
   --------------

   function New_List (Kind : OLNT.Node_Kind) return Types.List_Id is
   begin
      return List_Id (New_Node (Kind));
   end New_List;

   function New_List (
                      N0 : Node_Id := No_Node;
                      N1 : Node_Id := No_Node;
                      N2 : Node_Id := No_Node;
                      N3 : Node_Id := No_Node;
                      N4 : Node_Id := No_Node;
                      N5 : Node_Id := No_Node;
                      N6 : Node_Id := No_Node;
                      N7 : Node_Id := No_Node;
                      N8 : Node_Id := No_Node;
                      N9 : Node_Id := No_Node;
                      N10 : Node_Id := No_Node;
                      N11 : Node_Id := No_Node)
    return Types.List_Id is
      L : List_Id;
   begin
      L := New_List (Ocarina.Backends.LNT.Nodes.K_List_Id);

      if present (N0) then
         Append_Node_To_List (N0, L);
      end if;
      if present (N1) then
         Append_Node_To_List (N1, L);
      end if;

      if present (N2) then
         Append_Node_To_List (N2, L);
      end if;
      if present (N3) then
         Append_Node_To_List (N3, L);
      end if;
      if present (N4) then
         Append_Node_To_List (N4, L);
      end if;
      if present (N5) then
         Append_Node_To_List (N5, L);
      end if;
      if present (N6) then
         Append_Node_To_List (N6, L);
      end if;
      if present (N7) then
         Append_Node_To_List (N7, L);
      end if;
      if present (N8) then
         Append_Node_To_List (N8, L);
      end if;
      if present (N9) then
         Append_Node_To_List (N9, L);
      end if;
      if present (N10) then
         Append_Node_To_List (N10, L);
      end if;
      if present (N11) then
         Append_Node_To_List (N11, L);
      end if;
      return L;
   end New_List;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (L : Types.List_Id) return Boolean is

   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   ---------------------
   -- Make_Identifier --
   ---------------------
   function Make_Identifier (Ident_Name : Types.Name_Id)
     return Types.Node_Id
   is
      Identify : constant Node_Id := New_Node (K_Identifier);
   begin
      Set_Corresponding_Entity (Identify, No_Node);
      Set_Name (Identify, Ident_Name);
      Set_Ocarina_Node (Identify, No_Node);
      return Identify;
   end Make_Identifier;

   ---------------------
   -- Make_Identifier --
   ---------------------
   function Make_Identifier (Ident : String)
     return Types.Node_Id is
   begin
      return Make_Identifier (Get_String_Name (Ident));
   end Make_Identifier;

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
   -- New_Token --
   ---------------

   procedure New_Token
     (T : Token_Type;
      I : String := "") is
   begin
      if T in Keyword_Type then
         Set_Str_To_Name_Buffer (Image (T));
      else
         Set_Str_To_Name_Buffer (I);
      end if;

      Token_Image (T) := Name_Find;

      --  Mark Ada keywords

      if T in Keyword_Type then
         Get_Name_String_And_Append (Token_Image (T));
         Set_Name_Table_Byte (Name_Find, Byte (Token_Type'Pos (T) + 1));
      end if;
   end New_Token;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      S := Ada.Characters.Handling.To_Lower (S);
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (N : Name_Id) return Name_Id is
   begin
      if N = No_Name then
         return No_Name;
      end if;
      Get_Name_String (N);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end To_Lower;

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
   --------------------
   -- New_Identifier --
   --------------------
   function New_Identifier (Component_Name : Name_Id;
                            Prefix : String := "";
                            The_Old_Char : Character := '.';
                            The_New_Char : Character := '_')
     return Node_Id
   is
      S : String := Get_Name_String (Component_Name);
      N : Name_Id;
      procedure Replace (S : in out String;
                   The_Old : Character;
                   The_New : Character);
      procedure Replace (S : in out String;
                   The_Old : Character;
                   The_New : Character)
      is
      begin
         for i in S'Range loop
            if S (i) = The_Old then
               S (i) := The_New;
            end if;
         end loop;
      end Replace;
   begin
      Replace (S, The_Old_Char, The_New_Char);
      N := Get_String_Name (S);
      if (Prefix /= "") then
         N := Add_Prefix_To_Name (Prefix, N);
      end if;
      return Make_Identifier (N);
   end New_Identifier;
   function New_Identifier (Component_Name : Name_Id;
                            Prefix : String := "";
                            The_Old_Char : Character := '.';
                            The_New_Char : Character := '_')
     return Name_Id
   is
      S : String := Get_Name_String (Component_Name);
      N : Name_Id;
      procedure Replace (S : in out String;
                   The_Old : Character;
                   The_New : Character);
      procedure Replace (S : in out String;
                   The_Old : Character;
                   The_New : Character)
      is
      begin
         for i in S'Range loop
            if S (i) = The_Old then
               S (i) := The_New;
            end if;
         end loop;
      end Replace;
   begin
      Replace (S, The_Old_Char, The_New_Char);
      N := Get_String_Name (S);
      if (Prefix /= "") then
         N := Add_Prefix_To_Name (Prefix, N);
      end if;
      return N;
   end New_Identifier;
end Ocarina.Backends.LNT.Nutils;
