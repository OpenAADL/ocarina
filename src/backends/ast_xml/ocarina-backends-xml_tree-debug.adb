------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . X M L _ T R E E . D E B U G       --
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

with Charset;                     use Charset;
with Locations;                   use Locations;
with Ocarina.Namet;               use Ocarina.Namet;
with Utils;                       use Utils;
with Ocarina.Backends.XML_Values; use Ocarina.Backends.XML_Values;

package body Ocarina.Backends.XML_Tree.Debug is

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

   function Image (N : Value_Id) return String is
   begin
      return XML_Values.Image (N);
   end Image;

   function Image (N : Node_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : List_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Mode_Id) return String is
   begin
      case N is
         when Mode_In =>
            return Quoted ("in");
         when Mode_Inout =>
            return Quoted ("in out");
         when Mode_Out =>
            return Quoted ("out");
      end case;
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

   ------------
   -- W_Byte --
   ------------

   procedure W_Byte (N : Byte) is
   begin
      Write_Int (Int (N));
   end W_Byte;

   ---------------
   -- W_Indents --
   ---------------

   procedure W_Indents is
   begin
      for I in 1 .. N_Indents loop
         Write_Str ("   ");
      end loop;
   end W_Indents;

   ---------------
   -- W_List_Id --
   ---------------

   procedure W_List_Id (L : List_Id) is
      N : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      N := First_Node (L);
      while Present (N) loop
         W_Node_Id (N);
         N := Next_Node (N);
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
      C : Node_Id;
   begin
      if A = "Next_Node" or else A = "Package_Declaration" then
         return;
      end if;

      N_Indents := N_Indents + 1;
      W_Indents;
      Write_Str (A);
      Write_Char (' ');
      Write_Str (K);
      Write_Char (' ');
      C := Node_Id (N);

      if K = "Name_Id" then
         Write_Line (Quoted (V));
      elsif K = "Node_Id" and then Present (C) then
         case Kind (C) is
            when K_Numeric =>
               Write_Line ('(' & Image (Kind (Node_Id (N))) & ')');
            when others =>
               Write_Line (V);
         end case;
      else
         Write_Line (V);
      end if;

      if A /= "Frontend_Node"
        and then A /= "Corresponding_Node"
        and then A /= "Parent"
        and then A /= "Distributed_Application_Unit"
        and then A /= "Distributed_Application"
        and then A /= "Partition"
      then
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
      Write_Int (Int (N));
      Write_Char (' ');
      Write_Str (Image (Kind (N)));
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

end Ocarina.Backends.XML_Tree.Debug;
