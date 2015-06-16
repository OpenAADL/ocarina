------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 S p e c                                  --
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

with Types; use Types;

package Namet is

--  This package contains routines for handling the names table. The table
--  is used to store character strings for identifiers and operator symbols,
--  as well as other string values such as unit names and file names.

--  The names are hashed so that a given name appears only once in the table,
--  except that names entered with Name_Enter as opposed to Name_Find are
--  omitted from the hash table.

--  The first 26 entries in the names table (with Name_Id values in the range
--  First_Name_Id .. First_Name_Id + 25) represent names which are the one
--  character lower case letters in the range a-z, and these names are created
--  and initialized by the Initialize procedure.

--  Two values, one of type Int and one of type Byte, are stored with each
--  names table entry and subprograms are provided for setting and retrieving
--  these associated values. The usage of these values is up to the client.

   Name_Buffer : String (1 .. 16 * 1024);
   --  This buffer is used to set the name to be stored in the table for the
   --  Name_Find call, and to retrieve the name for the Get_Name_String call.
   --  The plus 1 in the length allows for cases of adding ASCII.NUL. The
   --  16K here is intended to be an infinite value that ensures that we
   --  never overflow the buffer (names this long are too absurd to worry!)

   Name_Len : Natural;
   --  Length of name stored in Name_Buffer. Used as an input parameter for
   --  Name_Find, and as an output value by Get_Name_String, or Write_Name.

   -----------------
   -- Subprograms --
   -----------------

   procedure Get_Name_String (Id : Name_Id);
   --  Get_Name_String is used to retrieve the string associated with an entry
   --  in the names table. The resulting string is stored in Name_Buffer
   --  and Name_Len is set. It is an error to call Get_Name_String with one
   --  of the special name Id values (No_Name or Error_Name).

   function Get_Name_String (Id : Name_Id) return String;
   --  This functional form returns the result as a string without affecting
   --  the contents of either Name_Buffer or Name_Len.

   procedure Get_Name_String_And_Append (Id : Name_Id);
   --  Like Get_Name_String but the resulting characters are appended to
   --  the current contents of the entry stored in Name_Buffer, and Name_Len
   --  is incremented to include the added characters.

   function Get_Name_Table_Byte (Id : Name_Id) return Byte;
   pragma Inline (Get_Name_Table_Byte);
   --  Fetches the Byte value associated with the given name

   function Get_Name_Table_Info (Id : Name_Id) return Int;
   pragma Inline (Get_Name_Table_Info);
   --  Fetches the Int value associated with the given name

   function Get_String_Name (The_String : String) return Name_Id;
   --  Enters 'The_String' in the name table and returns the
   --  corresponding name id.

   procedure Initialize;
   --  Initializes the names table, including initializing the first 26
   --  entries in the table (for the 1-character lower case names a-z)
   --  Note that Initialize must not be called if Tree_Read is used.

   function Name_Find return Name_Id;
   --  Name_Find is called with a string stored in Name_Buffer whose length
   --  is in Name_Len (i.e. the characters of the name are in subscript
   --  positions 1 to Name_Len in Name_Buffer). It searches the names
   --  table to see if the string has already been stored. If so the Id of
   --  the existing entry is returned. Otherwise a new entry is created with
   --  its Name_Table_Info field set to zero. The contents of Name_Buffer
   --  and Name_Len are not modified by this call.

   function Name_Enter return Name_Id;
   --  Name_Enter has the same calling interface as Name_Find. The difference
   --  is that it does not search the table for an existing match, and also
   --  subsequent Name_Find calls using the same name will not locate the
   --  entry created by this call. Thus multiple calls to Name_Enter with the
   --  same name will create multiple entries in the name table with different
   --  Name_Id values. This is useful in the case of created names, which are
   --  never expected to be looked up. Note: Name_Enter should never be used
   --  for one character names, since these are efficiently located without
   --  hashing by Name_Find in any case.

   procedure Add_Char_To_Name_Buffer (C : Character);
   pragma Inline (Add_Char_To_Name_Buffer);
   --  Add given character to the end of the string currently stored in the
   --  Name_Buffer, incrementing Name_Len.

   procedure Set_Char_To_Name_Buffer (C : Character);
   pragma Inline (Set_Char_To_Name_Buffer);
   --  Equivalent to Name_Len := 0; followed by Add_Char_To_Name_Buffer (C);

   procedure Add_Nat_To_Name_Buffer (V : Nat);
   --  Add decimal representation of given value to the end of the string
   --  currently stored in Name_Buffer, incrementing Name_Len as required.

   procedure Set_Nat_To_Name_Buffer (V : Nat);
   pragma Inline (Set_Nat_To_Name_Buffer);
   --  Equivalent to Name_Len := 0; followed by Add_Nat_To_Name_Buffer (V);

   procedure Add_Dnat_To_Name_Buffer (V : Dnat);
   --  Add decimal representation of given value to the end of the string
   --  currently stored in Name_Buffer, incrementing Name_Len as required.

   procedure Set_Dnat_To_Name_Buffer (V : Dnat);
   pragma Inline (Set_Dnat_To_Name_Buffer);
   --  Equivalent to Name_Len := 0; followed by Add_Nat_To_Name_Buffer (V);

   procedure Add_Str_To_Name_Buffer (S : String);
   --  Add characters of string S to the end of the string currently stored
   --  in the Name_Buffer, incrementing Name_Len by the length of the string.

   procedure Set_Str_To_Name_Buffer (S : String);
   pragma Inline (Set_Str_To_Name_Buffer);
   --  Equivalent to Name_Len := 0; followed by Add_Str_To_Name_Buffer (S);

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int);
   pragma Inline (Set_Name_Table_Info);
   --  Sets the Int value associated with the given name

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte);
   pragma Inline (Set_Name_Table_Byte);
   --  Sets the Byte value associated with the given name

   procedure Write_Name (Id : Name_Id);
   --  Write_Name writes the characters of the specified name using the
   --  standard output procedures in package Output. No end of line is
   --  written, just the characters of the name. On return Name_Buffer and
   --  Name_Len are set as for a call to Get_Name_String. The name is written
   --  in encoded form (i.e. including Uhh, Whhh, Qx, _op as they appear in
   --  the name table). If Id is Error_Name, or No_Name, no text is output.

   procedure wn (Id : Name_Id);
--   pragma Export (Ada, wn);
   --  Like Write_Name, but includes new line at end. Intended for use
   --  from the debugger only.

   function Is_Bad_Spelling_Of (Found, Expect : Name_Id) return Boolean;
   --  Return true if Found is a possible misspelling of Expect

   procedure Is_Bad_Spelling (Found : Name_Id);
   --  Scan all entries in name table, return the first possible
   --  misspelling of Found.

   ---------------------------
   -- Table Data Structures --
   ---------------------------

   --  The following declarations define the data structures used to store
   --  names. The definitions are in the private part of the package spec,
   --  rather than the body, since they are referenced directly by gigi.

private

   --  This table stores the actual string names. Although logically there
   --  is no need for a terminating character (since the length is stored
   --  in the name entry table), we still store a NUL character at the end
   --  of every name (for convenience in interfacing to the C world).

   package Name_Chars is new GNAT.Table
     (Table_Component_Type => Character,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 0,
      Table_Initial        => 50_000,
      Table_Increment      => 100);

   type Name_Entry is record
      Name_Chars_Index : Int;
      --  Starting location of characters in the Name_Chars table minus
      --  one (i.e. pointer to character just before first character). The
      --  reason for the bias of one is that indexes in Name_Buffer are
      --  one's origin, so this avoids unnecessary adds and subtracts of 1.

      Name_Len : Short;
      --  Length of this name in characters

      Byte_Info : Byte;
      --  Byte value associated with this name

      Hash_Link : Name_Id;
      --  Link to next entry in names table for same hash code

      Int_Info : Int;
      --  Int Value associated with this name
   end record;

   --  This is the table that is referenced by Name_Id entries.
   --  It contains one entry for each unique name in the table.

   package Name_Entries is new GNAT.Table
     (Table_Component_Type => Name_Entry,
      Table_Index_Type     => Name_Id,
      Table_Low_Bound      => First_Name_Id,
      Table_Initial        => 6_000,
      Table_Increment      => 100);

end Namet;
