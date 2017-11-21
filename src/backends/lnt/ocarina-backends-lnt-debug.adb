------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . L N T . D E B U G            --
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

with Charset;       use Charset;
with Locations;     use Locations;
with Ocarina.Namet; use Ocarina.Namet;
with Utils;         use Utils;

package body Ocarina.Backends.LNT.Debug is

   -----------
   -- Image --
   -----------

   function Image (N : Node_Kind) return String is
      S : String := Node_Kind'Image (N);
   begin
      To_Lower (S);
      for I in S'Range loop
         if S (I) = '_' then
            S (I) := ' ';
         end if;
      end loop;
      return S (3 .. S'Last);
   end Image;

   function Image (N : Name_Id) return String is
   begin
      if N = No_Name then
         return No_Str;
      else
         return Get_Name_String (N);
      end if;
   end Image;

   function Image (N : Node_Id) return String is
   begin
      return Image (Byte (N));
   end Image;

   function Image (N : Mode_Id) return String is
   begin
      case N is
         when Mode_In =>
            return Quoted ("in");
         when Mode_Inout =>
            return Quoted ("inout");
         when Mode_Out =>
            return Quoted ("out");
      end case;
   end Image;

   function Image (N : List_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Value_Id) return String is
   begin
      return Image (Int (N));
   end Image;
   function Image (N : Operator_Id) return String is
   begin
      return Operator_Id'Image (N);
   end Image;
   function Image (N : Boolean) return String is
   begin
      return Boolean'Image (N);
   end Image;

   function Image (N : Byte) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Int) return String is
      S : constant String := Int'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   ---------------
   -- W_Boolean --
   ---------------

   procedure W_Boolean (N : Boolean) is
   begin
      Write_Str (N'Img);
   end W_Boolean;

   ------------
   -- W_Byte --
   ------------

   procedure W_Byte (N : Byte) is
   begin
      Write_Int (Int (N));
   end W_Byte;

   -----------------
   -- W_Full_Tree --
   -----------------

   procedure W_Full_Tree (N : Node_Id) is
      D : Node_Id := First_Node (List_Id (N));
   begin
      N_Indents := 0;
      while Present (D) loop
         W_Node_Id (D);
         D := Next_Node (D);
      end loop;
   end W_Full_Tree;

   ---------------
   -- W_Indents --
   ---------------

   procedure W_Indents is
   begin
      for I in 1 .. N_Indents loop
         Write_Str (" ");
      end loop;
   end W_Indents;

   ---------------
   -- W_List_Id --
   ---------------

   procedure W_List_Id (L : List_Id) is
      E : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      E := First_Node (L);
      while E /= No_Node loop
         W_Node_Id (E);
         E := Next_Node (E);
      end loop;
   end W_List_Id;

   ----------------------
   -- W_Node_Attribute --
   ----------------------

   procedure W_Node_Attribute
     (A : String;
      K : String;
      V : String;
      N : Int := 0)
   is
   begin
      --  Node attributes that must be ignored

      if A = "Next_Node" then
         return;
      end if;

      N_Indents := N_Indents + 1;
      W_Indents;
      Write_Str  (A);
      Write_Char (' ');
      Write_Str  (K);
      Write_Char (' ');

      if K = "Name_Id" then
         Write_Line (Quoted (V));
      else
         Write_Line (V);
      end if;

      --  Node attributes that must be developed

      if A /= "Node" then
         if K = "Node_Id" then
            W_Node_Id (Node_Id (N));
         elsif K = "List_Id" then
            W_List_Id (List_Id (N));
         end if;
      end if;

      N_Indents := N_Indents - 1;
   end W_Node_Attribute;

   -------------------
   -- W_Node_Header --
   -------------------

   procedure W_Node_Header (N : Node_Id) is
   begin
      W_Indents;
      Write_Int  (Int (N));
      Write_Char (' ');
      Write_Str  (Image (Kind (N)));
      Write_Char (' ');
      Write_Line (Image (Loc (N)));
   end W_Node_Header;

   ---------------
   -- W_Node_Id --
   ---------------

   procedure W_Node_Id (N : Node_Id) is
   begin
      if N = No_Node then
         return;
      end if;
      W_Node (N);
   end W_Node_Id;

end Ocarina.Backends.LNT.Debug;
