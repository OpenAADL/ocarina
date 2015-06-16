------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . M E _ R E A L . R E A L _ T R E E . D E B U G       --
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

--  Debug function for REAL tree

with Ocarina.ME_REAL.REAL_Tree.Utils;
with Locations; use Locations;
with Ocarina.Namet;     use Ocarina.Namet;

package body Ocarina.ME_REAL.REAL_Tree.Debug is
   use Ocarina.ME_REAL.REAL_Tree.Utils;

   -----------
   -- Image --
   -----------

   function Image (N : Node_Kind) return String is
      I : String := Node_Kind'Image (N);
   begin
      Capitalize (I);
      return I (3 .. I'Last);
   end Image;

   procedure W_Indentation (N : Natural) is
   begin
      for I in 1 .. N loop
         Write_Str ("   ");
      end loop;
   end W_Indentation;

   procedure W_Boolean (N : Boolean) is
   begin
      Write_Str (N'Img);
   end W_Boolean;

   procedure W_Byte (N : Byte) is
   begin
      Write_Int (Int (N));
   end W_Byte;

   procedure W_List_Id (I : Natural; L : List_Id) is
      E : Node_Id;
   begin
      if L = No_List then
         return;
      end if;

      E := First_Node (L);
      while E /= No_Node loop
         W_Node_Id (I, E);
         E := Next_Node (E);
      end loop;
   end W_List_Id;

   procedure W_Node_Attribute
     (A : String;
      T : String;
      V : String;
      N : Int := 0)
   is
   begin
      W_Node_Attribute (0, A, T, V, N);
   end W_Node_Attribute;

   procedure W_Node_Attribute
     (I : Natural;
      A : String;
      T : String;
      V : String;
      N : Int := 0)
   is
      C : Node_Id;
   begin
      if A = "Next_Node" or else A = "Homonym" or else A = "Name" then
         return;
      end if;
      W_Indentation (I + 1);
      Write_Str (A);
      Write_Char (' ');
      Write_Str (T);
      Write_Char (' ');
      C := Node_Id (N);
      if T = "Name_Id" then
         Write_Line (Quoted (V));
      elsif T = "Node_Id" and then Present (C) then
         case Kind (C) is
            when others =>
               Write_Line (V);
         end case;
      else
         Write_Line (V);
      end if;
      if A = "Node" or else A = "Scope" then
         return;
      end if;
      if T = "Node_Id" then
         W_Node_Id (I + 1, Node_Id (N));
      elsif T = "List_Id" then
         W_List_Id (I + 1, List_Id (N));
      end if;
   end W_Node_Attribute;

   procedure W_Node_Id (I : Natural; N : Node_Id) is
      pragma Unreferenced (I);
   begin
      if N = No_Node then
         return;
      end if;
      W_Node (N);
   end W_Node_Id;

   procedure W_Node_Header (I : Natural; N : Node_Id) is
   begin
      W_Indentation (I);
      Write_Int (Int (N));
      Write_Char (' ');
      Write_Str (Image (Kind (N)));
      Write_Char (' ');
      Write_Line (Image (Loc (N)));
   end W_Node_Header;

   procedure W_List_Id (L : List_Id) is
   begin
      W_List_Id (0, L);
   end W_List_Id;

   procedure W_Node_Id (N : Node_Id) is
   begin
      W_Node_Id (0, N);
   end W_Node_Id;

   procedure W_Node_Header (N : Node_Id) is
   begin
      W_Node_Header (0, N);
   end W_Node_Header;

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
      return Image (Int (N));
   end Image;

   function Image (N : List_Id) return String is
   begin
      return Image (Int (N));
   end Image;

   function Image (N : Operator_Id) return String is
   begin
      return Image (Byte (N));
   end Image;

   function Image (N : Value_Id) return String is
   begin
      return Image (Int (N));
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

   procedure W_Full_Tree (Root : Node_Id) is
      D : Node_Id;
   begin
      W_Node_Id (0, Range_Declaration (Root));

      D := First_Node (Declarations (Root));

      while Present (D) loop
         W_Node_Id (0, D);
         D := Next_Node (D);
      end loop;

      W_Node_Id (0, Check_Expression (Root));

   end W_Full_Tree;

end Ocarina.ME_REAL.REAL_Tree.Debug;
