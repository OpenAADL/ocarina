------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2007-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Charset; use Charset;
with Ocarina.Namet;   use Ocarina.Namet;

package body Utils is

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

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
   -- Remove_Prefix_From_Name --
   -----------------------------

   function Remove_Prefix_From_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id
   is
      Length   : Natural;
      Temp_Str : String (1 .. Prefix'Length);
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Length := Name_Len;
      Get_Name_String (Name);
      if Name_Len > Length then
         Temp_Str := Name_Buffer (1 .. Length);
         if Prefix = Temp_Str then
            Set_Str_To_Name_Buffer (Name_Buffer (1 + Length .. Name_Len));
            return Name_Find;
         end if;
      end if;
      return Name;
   end Remove_Prefix_From_Name;

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

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Up : Boolean := True;
   begin
      for I in S'Range loop
         if Up then
            Up := False;
            if S (I) in 'a' .. 'z' then
               S (I) := Character'Val (Character'Pos (S (I)) + Up_To_Low);
            end if;
         end if;
         if S (I) = '_' then
            Up := True;
         end if;
      end loop;
   end Capitalize;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return String is -- "
   begin
      return (1 => D) & S & (1 => D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return Name_Id is -- "
   begin
      Set_Char_To_Name_Buffer (D);
      Add_Str_To_Name_Buffer (S);
      Add_Char_To_Name_Buffer (D);
      return Name_Find;
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return String is -- "
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return Name_Id is -- "
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   -----------------
   -- Remove_Char --
   -----------------

   function Remove_Char (Name : Name_Id; O : Character) return Name_Id is
      New_Name     : Name_Id         := No_Name;
      Initial_Name : constant String := Get_Name_String (Name);
   begin
      Name_Len := 0;

      for Index in Initial_Name'First .. Initial_Name'Last loop
         if Initial_Name (Index) /= O then
            Add_Char_To_Name_Buffer (Initial_Name (Index));
         end if;
      end loop;

      New_Name := Name_Find;

      return New_Name;
   end Remove_Char;

   ------------------
   -- Replace_Char --
   ------------------

   function Replace_Char
     (Name : Name_Id;
      O    : Character;
      N    : Character) return Name_Id
   is
      New_Name     : Name_Id         := No_Name;
      Initial_Name : constant String := Get_Name_String (Name);
   begin
      Name_Len := 0;

      for Index in Initial_Name'First .. Initial_Name'Last loop
         if Initial_Name (Index) = O then
            Add_Char_To_Name_Buffer (N);
         else
            Add_Char_To_Name_Buffer (Initial_Name (Index));
         end if;
      end loop;

      New_Name := Name_Find;

      return New_Name;
   end Replace_Char;

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

   --------------
   -- To_Upper --
   --------------

   function To_Upper (N : Name_Id) return Name_Id is
   begin
      if N = No_Name then
         return No_Name;
      end if;
      Get_Name_String (N);
      To_Upper (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end To_Upper;

   ---------------
   -- Is_Prefix --
   ---------------

   function Is_Prefix (N1 : Name_Id; N2 : Name_Id) return Boolean is
   begin
      if N1 = No_Name then
         return True;
      end if;

      if N2 = No_Name then
         return False;
      end if;

      declare
         S1 : constant String := Get_Name_String (N1);
         S2 : constant String := Get_Name_String (N2);
      begin
         if S1'Length > S2'Length then
            return False;
         end if;
         for I in 1 .. S1'Last loop
            if S1 (I) /= S2 (I) then
               return False;
            end if;
         end loop;
      end;

      return True;
   end Is_Prefix;

end Utils;
