------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                        O C A R I N A . N A M E T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2016 ESA & ISAE.      --
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

with Ocarina.Output; use Ocarina.Output;
with GNAT.Spelling_Checker;

package body Ocarina.Namet is

   Hash_Num : constant Int := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash alogorithm.

   Hash_Max : constant Int := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Int range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of Name_Id;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Hash return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for name stored in Name_Buffer (length in Name_Len)

   -----------------------------
   -- Add_Char_To_Name_Buffer --
   -----------------------------

   procedure Add_Char_To_Name_Buffer (C : Character) is
   begin
      if Name_Len < Name_Buffer'Last then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := C;
      end if;
   end Add_Char_To_Name_Buffer;

   -----------------------------
   -- Add_Dnat_To_Name_Buffer --
   -----------------------------

   procedure Add_Dnat_To_Name_Buffer (V : Dnat) is
   begin
      if V >= 10 then
         Add_Dnat_To_Name_Buffer (V / 10);
      end if;

      Add_Char_To_Name_Buffer (Character'Val (Character'Pos ('0') + V rem 10));
   end Add_Dnat_To_Name_Buffer;

   ----------------------------
   -- Add_Nat_To_Name_Buffer --
   ----------------------------

   procedure Add_Nat_To_Name_Buffer (V : Nat) is
   begin
      if V >= 10 then
         Add_Nat_To_Name_Buffer (V / 10);
      end if;

      Add_Char_To_Name_Buffer (Character'Val (Character'Pos ('0') + V rem 10));
   end Add_Nat_To_Name_Buffer;

   ----------------------------
   -- Add_ULL_To_Name_Buffer --
   ----------------------------

   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL; S : Integer := 1) is
      Hex : constant String := "0123456789ABCDEF";
      Q : constant ULL := U / B;
      R : constant ULL := U mod B;
   begin
      if Q /= 0 or else S > 1 then
         Add_ULL_To_Name_Buffer (Q, B, S - 1);
      end if;
      Add_Char_To_Name_Buffer (Hex (Hex'First + Natural (R)));
   end Add_ULL_To_Name_Buffer;

   ----------------------------
   -- Add_Str_To_Name_Buffer --
   ----------------------------

   procedure Add_Str_To_Name_Buffer (S : String) is
   begin
      for J in S'Range loop
         Add_Char_To_Name_Buffer (S (J));
      end loop;
   end Add_Str_To_Name_Buffer;

   ---------------------
   -- Get_Name_String --
   ---------------------

   procedure Get_Name_String (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;
      Name_Len := Natural (Name_Entries.Table (Id).Name_Len);

      for J in 1 .. Name_Len loop
         Name_Buffer (J) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String;

   function Get_Name_String (Id : Name_Id) return String is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      S := Name_Entries.Table (Id).Name_Chars_Index;

      declare
         R : String (1 .. Natural (Name_Entries.Table (Id).Name_Len));

      begin
         for J in R'Range loop
            R (J) := Name_Chars.Table (S + Int (J));
         end loop;

         return R;
      end;
   end Get_Name_String;

   --------------------------------
   -- Get_Name_String_And_Append --
   --------------------------------

   procedure Get_Name_String_And_Append (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;

      for J in 1 .. Natural (Name_Entries.Table (Id).Name_Len) loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String_And_Append;

   -------------------------
   -- Get_Name_Table_Byte --
   -------------------------

   function Get_Name_Table_Byte (Id : Name_Id) return Byte is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Byte_Info;
   end Get_Name_Table_Byte;

   -------------------------
   -- Get_Name_Table_Info --
   -------------------------

   function Get_Name_Table_Info (Id : Name_Id) return Int is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Int_Info;
   end Get_Name_Table_Info;

   ---------------------
   -- Get_String_Name --
   ---------------------

   function Get_String_Name (The_String : String) return Name_Id is
      pragma Assert (The_String'Length > 0);

      Result : Name_Id;
   begin
      Set_Str_To_Name_Buffer (The_String);
      Result := Name_Find;
      return Result;
   end Get_String_Name;

   ----------
   -- Hash --
   ----------

   function Hash return Hash_Index_Type is
      subtype Int_1_12 is Int range 1 .. 12;
      --  Used to avoid when others on case jump below

      Even_Name_Len : Integer;
      --  Last even numbered position (used for >12 case)

   begin

      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if Name_Len > 12 then
         Even_Name_Len := (Name_Len) / 2 * 2;

         return ((((((((((((
           Character'Pos (Name_Buffer (01))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 10))) * 2 +
           Character'Pos (Name_Buffer (03))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 08))) * 2 +
           Character'Pos (Name_Buffer (05))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 06))) * 2 +
           Character'Pos (Name_Buffer (07))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 04))) * 2 +
           Character'Pos (Name_Buffer (09))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 02))) * 2 +
           Character'Pos (Name_Buffer (11))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len))) mod Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Int_1_12 (Name_Len) is

         when 1 =>
            return
               Character'Pos (Name_Buffer (1));

         when 2 =>
            return ((
              Character'Pos (Name_Buffer (1))) * 64 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (Name_Buffer (1))) * 16 +
              Character'Pos (Name_Buffer (3))) * 16 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (Name_Buffer (1))) * 8 +
              Character'Pos (Name_Buffer (2))) * 8 +
              Character'Pos (Name_Buffer (3))) * 8 +
              Character'Pos (Name_Buffer (4))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (Name_Buffer (4))) * 8 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (5))) * 8 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (Name_Buffer (5))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (6))) * 4 +
              Character'Pos (Name_Buffer (3))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (2))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (9))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (10))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (11))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (11))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (12))) mod Hash_Num;

      end case;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Name_Chars.Init;
      Name_Entries.Init;

      --  Initialize entries for one character names

      for C in Character loop
         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := 1;
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := C;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := ASCII.NUL;
      end loop;

      --  Clear hash table

      for J in Hash_Index_Type loop
         Hash_Table (J) := No_Name;
      end loop;
   end Initialize;

   ----------------
   -- Name_Enter --
   ----------------

   function Name_Enter return Name_Id is
   begin
      Name_Entries.Increment_Last;
      Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
        Name_Chars.Last;
      Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
      Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
      Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
      Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

      --  Set corresponding string entry in the Name_Chars table

      for J in 1 .. Name_Len loop
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := Name_Buffer (J);
      end loop;

      Name_Chars.Increment_Last;
      Name_Chars.Table (Name_Chars.Last) := ASCII.NUL;

      return Name_Entries.Last;
   end Name_Enter;

   ---------------
   -- Name_Find --
   ---------------

   function Name_Find return Name_Id is
      New_Id : Name_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index

   begin
      --  Quick handling for one character names

      if Name_Len = 0 then
         return No_Name;

      elsif Name_Len = 1 then
         return Name_Id (First_Name_Id + Character'Pos (Name_Buffer (1)));

      --  Otherwise search hash table for existing matching entry

      else
         Hash_Index := Ocarina.Namet.Hash;
         New_Id := Hash_Table (Hash_Index);

         if New_Id = No_Name then
            Hash_Table (Hash_Index) := Name_Entries.Last + 1;

         else
            Search : loop
               if Name_Len /=
                 Integer (Name_Entries.Table (New_Id).Name_Len)
               then
                  goto No_Match;
               end if;

               S := Name_Entries.Table (New_Id).Name_Chars_Index;

               for I in 1 .. Name_Len loop
                  if Name_Chars.Table (S + Int (I)) /= Name_Buffer (I) then
                     goto No_Match;
                  end if;
               end loop;

               return New_Id;

               --  Current entry in hash chain does not match

               <<No_Match>>
                  if Name_Entries.Table (New_Id).Hash_Link /= No_Name then
                     New_Id := Name_Entries.Table (New_Id).Hash_Link;
                  else
                     Name_Entries.Table (New_Id).Hash_Link :=
                       Name_Entries.Last + 1;
                     exit Search;
                  end if;

            end loop Search;
         end if;

         --  We fall through here only if a matching entry was not
         --  found in the hash table. We now create a new entry in the
         --  names table. The hash link pointing to the new entry
         --  (Name_Entries.Last+1) has been set.

         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

         --  Set corresponding string entry in the Name_Chars table

         for I in 1 .. Name_Len loop
            Name_Chars.Increment_Last;
            Name_Chars.Table (Name_Chars.Last) := Name_Buffer (I);
         end loop;

         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := ASCII.NUL;

         return Name_Entries.Last;
      end if;
   end Name_Find;

   -----------------------------
   -- Set_Char_To_Name_Buffer --
   -----------------------------

   procedure Set_Char_To_Name_Buffer (C : Character) is
   begin
      Name_Len := 0;
      Add_Char_To_Name_Buffer (C);
   end Set_Char_To_Name_Buffer;

   -----------------------------
   -- Set_Dnat_To_Name_Buffer --
   -----------------------------

   procedure Set_Dnat_To_Name_Buffer (V : Dnat) is
   begin
      Name_Len := 0;
      Add_Dnat_To_Name_Buffer (V);
   end Set_Dnat_To_Name_Buffer;

   -------------------------
   -- Set_Name_Table_Byte --
   -------------------------

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Byte_Info := Val;
   end Set_Name_Table_Byte;

   -------------------------
   -- Set_Name_Table_Info --
   -------------------------

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Int_Info := Val;
   end Set_Name_Table_Info;

   ----------------------------
   -- Set_Nat_To_Name_Buffer --
   ----------------------------

   procedure Set_Nat_To_Name_Buffer (V : Nat) is
   begin
      Name_Len := 0;
      Add_Nat_To_Name_Buffer (V);
   end Set_Nat_To_Name_Buffer;

   ----------------------------
   -- Set_Str_To_Name_Buffer --
   ----------------------------

   procedure Set_Str_To_Name_Buffer (S : String) is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (S);
   end Set_Str_To_Name_Buffer;

   --------
   -- wn --
   --------

   procedure wn (Id : Name_Id) is
   begin
      Write_Name (Id);
      Write_Eol;
   end wn;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name (Id : Name_Id) is
   begin
      if Id >= First_Name_Id then
         Get_Name_String (Id);
         Write_Str (Name_Buffer (1 .. Name_Len));
      end if;
   end Write_Name;

   ------------------------
   -- Is_Bad_Spelling_Of --
   ------------------------

   function Is_Bad_Spelling_Of (Found, Expect : Name_Id) return Boolean is
      FB : constant String := Get_Name_String (Found);
      EB : constant String := Get_Name_String (Expect);

   begin
      return GNAT.Spelling_Checker.Is_Bad_Spelling_Of (FB, EB);
   end Is_Bad_Spelling_Of;

   ---------------------
   -- Is_Bad_Spelling --
   ---------------------

   procedure Is_Bad_Spelling (Found : Name_Id) is
   begin
      for J in Name_Entries.First .. Name_Entries.Last loop
         if Found /= J and then Is_Bad_Spelling_Of (Found, J) then
            Write_Str ("Possible mispelling of " & Get_Name_String (J));
            exit;
         end if;
      end loop;
   end Is_Bad_Spelling;

end Ocarina.Namet;
