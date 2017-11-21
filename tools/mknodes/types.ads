------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 S p e c                                  --
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

with Unchecked_Deallocation;

package Types is
   pragma Preelaborate (Types);

--  This package contains host independent type definitions which are used
--  in more than one unit in the compiler. They are gathered here for easy
--  reference, though in some cases the full description is found in the
--  relevant module which implements the definition. The main reason that
--  they are not in their "natural" specs is that this would cause a lot of
--  inter-spec dependencies, and in particular some awkward circular
--  dependencies would have to be dealt with.

   -------------------------------
   -- General Use Integer Types --
   -------------------------------

   type Int is range -2**31 .. +2**31 - 1;
   --  Signed 32-bit integer

   type Dint is range -2**63 .. +2**63 - 1;
   --  Double length (64-bit) integer

   subtype Nat is Int range 0 .. Int'Last;
   --  Non-negative Int values

   subtype Dnat is Dint range 0 .. Dint'Last;

   subtype Pos is Int range 1 .. Int'Last;
   --  Positive Int values

   type Word is mod 2**32;
   --  Unsigned 32-bit integer

   type Byte is mod 2**8;
   for Byte'Size use 8;
   --  8-bit unsigned integer

   type size_t is mod 2**Standard'Address_Size;
   --  Memory size value, for use in calls to C routines

   --------------------------------------
   -- 8-Bit Character and String Types --
   --------------------------------------

   --  We use Standard.Character and Standard.String freely, since we are
   --  compiling ourselves, and we properly implement the required 8-bit
   --  character code as required in Ada 95. This section defines a few
   --  general use constants and subtypes.

   EOF : constant Character := ASCII.SUB;
   --  The character SUB (16#1A#) is used in DOS and other systems derived
   --  from DOS (OS/2, NT etc) to signal the end of a text file. Internally
   --  all source files are ended by an EOF character, even on Unix systems.
   --  An EOF character acts as the end of file only as the last character
   --  of a source buffer, in any other position, it is treated as a blank
   --  if it appears between tokens, and as an illegal character otherwise.
   --  This makes life easier dealing with files that originated from DOS,
   --  including concatenated files with interspersed EOF characters.

   subtype Graphic_Character is Character range ' ' .. '~';
   --  Graphic characters, as defined in ARM

   subtype Line_Terminator is Character range ASCII.LF .. ASCII.CR;
   --  Line terminator characters (LF, VT, FF, CR)

   subtype Upper_Half_Character is
     Character range Character'Val (16#80#) .. Character'Val (16#FF#);
   --  Characters with the upper bit set

   type Character_Ptr is access all Character;
   type String_Ptr is access all String;
   --  Standard character and string pointers

   procedure Free is new Unchecked_Deallocation (String, String_Ptr);
   --  Procedure for freeing dynamically allocated String values

   procedure Change_If_Empty (Str_Ptr : in out String_Ptr; Value : String);
   pragma Inline (Change_If_Empty);
   --  If Str_Ptr is null or points to the empty string, reallocate
   --  it with the given Value.

   -----------------------------------------
   -- Types Used for Text Buffer Handling --
   -----------------------------------------

   --  We can't use type String for text buffers, since we must use the
   --  standard 32-bit integer as an index value, since we count on all
   --  index values being the same size.

   type Text_Ptr is new Int;
   --  Type used for subscripts in text buffer

   type Text_Buffer is array (Text_Ptr range <>) of Character;
   --  Text buffer used to hold source file or library information file

   type Text_Buffer_Ptr is access all Text_Buffer;
   --  Text buffers for input files are allocated dynamically and this type
   --  is used to reference these text buffers.

   procedure Free is new Unchecked_Deallocation (Text_Buffer, Text_Buffer_Ptr);
   --  Procedure for freeing dynamically allocated text buffers

   ------------------------------------------
   -- Types Used for Source Input Handling --
   ------------------------------------------

   type Logical_Line_Number is range 0 .. Int'Last;
   for Logical_Line_Number'Size use 32;
   --  Line number type, used for storing logical line numbers (i.e. line
   --  numbers that include effects of any Source_Reference pragmas in the
   --  source file). The value zero indicates a line containing a source
   --  reference pragma.

   No_Line_Number : constant Logical_Line_Number := 0;
   --  Special value used to indicate no line number

   type Physical_Line_Number is range 1 .. Int'Last;
   for Physical_Line_Number'Size use 32;
   --  Line number type, used for storing physical line numbers (i.e.
   --  line numbers in the physical file being compiled, unaffected by
   --  the presence of source reference pragmas.

   type Column_Number is range 0 .. 32767;
   for Column_Number'Size use 16;
   --  Column number (assume that 2**15 is large enough, see declaration
   --  of Hostparm.Max_Line_Length)

   No_Column_Number : constant Column_Number := 0;
   --  Special value used to indicate no column number

   -----------------------------
   -- Types for Namet Package --
   -----------------------------

   --  Name_Id values are used to identify entries in the names table. Except
   --  for the special values No_Name, and Error_Name, they are subscript
   --  values for the Names table defined in package Namet.

   --  Note that with only a few exceptions, which are clearly documented, the
   --  type Name_Id should be regarded as a private type. In particular it is
   --  never appropriate to perform arithmetic operations using this type.

   Names_Low_Bound : constant := 300_000_000;
   --  Low bound for name Id values

   Names_High_Bound : constant := 399_999_999;
   --  Maximum number of names that can be allocated is 100 million, which is
   --  in practice infinite and there is no need to check the range.

   type Name_Id is range Names_Low_Bound .. Names_High_Bound;
   for Name_Id'Size use 32;
   --  Type used to identify entries in the names table

   No_Str : constant String := "";

   No_Name : constant Name_Id := Names_Low_Bound;
   --  The special Name_Id value No_Name is used in the parser to indicate
   --  a situation where no name is present (e.g. on a loop or block).

   First_Name_Id : constant Name_Id := Names_Low_Bound + 2;
   --  Subscript of first entry in names table

   type Node_Id is new Int;
   No_Node : constant Node_Id := 0;

   function Present (E : Node_Id) return Boolean;
   --  Return true when E is not No_Node

   function No (E : Node_Id) return Boolean;
   --  Return true when E is No_Node

   function Safe_XOR (Right : Boolean; Left : Boolean) return Boolean;
   --  This function performs a safe xor operation with respect to the
   --  No_Direct_Boolean_Operators restriction.

   type List_Id is new Node_Id;
   No_List : constant List_Id := 0;

   type Node_List is record
      First, Last : Node_Id := No_Node;
   end record;

   type Operator_Id is new Byte;
   type Value_Id is new Int;

   Mode_In    : constant := 0;
   Mode_Inout : constant := 1;
   Mode_Out   : constant := 2;

   type Mode_Id is new Byte range Mode_In .. Mode_Out;

   Pragma_Id           : constant := 0;
   Pragma_Prefix       : constant := 1;
   Pragma_Version      : constant := 2;
   Pragma_Unrecognized : constant := 3;

   type Pragma_Type is new Byte range Pragma_Id .. Pragma_Unrecognized;

   type Base_Type is new Node_Id;

   type Short_Short is range -2**7 .. 2**7 - 1;
   for Short_Short'Size use 8;

   type Short is range -2**15 .. 2**15 - 1;
   for Short'Size use 16;

   type Long is range -2**31 .. 2**31 - 1;
   for Long'Size use 32;

   type Long_Long is range -2**63 .. 2**63 - 1;
   for Long_Long'Size use 64;

   type Octet is mod 2**8;
   for Octet'Size use 8;

   type Unsigned_Short_Short is mod 2**8;
   for Unsigned_Short_Short'Size use 8;

   type Unsigned_Short is mod 2**16;
   for Unsigned_Short'Size use 16;

   type Unsigned_Long is mod 2**32;
   for Unsigned_Long'Size use 32;

   type Unsigned_Long_Long is mod 2**64;
   for Unsigned_Long_Long'Size use 64;

   --  Floating point types. We assume that we are on an IEEE machine, and
   --  that the types Short_Float and Long_Float in Standard refer to the
   --  32-bit short and 64-bit long IEEE forms. Furthermore, if there is
   --  an extended float, we assume that it is available as Long_Long_Float.
   --  Note: it is harmless, and explicitly permitted, to include additional
   --  types in interfaces, so it is not wrong to have IEEE_Extended_Float
   --  defined even if the extended format is not available.

   type Float is new Short_Float;
   type Double is new Long_Float;
   type Long_Double is new Long_Long_Float;

   FSS  : constant := Short_Short'First;
   LSS  : constant := Short_Short'Last;
   FS   : constant := Short'First;
   LS   : constant := Short'Last;
   FL   : constant := Long'First;
   LL   : constant := Long'Last;
   FLL  : constant := Long_Long'First;
   LLL  : constant := Long_Long'Last;
   FO   : constant := Octet'First;
   LO   : constant := Octet'Last;
   FUSS : constant := Unsigned_Short_Short'First;
   LUSS : constant := Unsigned_Short_Short'Last;
   FUS  : constant := Unsigned_Short'First;
   LUS  : constant := Unsigned_Short'Last;
   FUL  : constant := Unsigned_Long'First;
   LUL  : constant := Unsigned_Long'Last;
   FULL : constant := Unsigned_Long_Long'First;
   LULL : constant := Unsigned_Long_Long'Last;

   function Shift_Left
     (Value  : Unsigned_Long_Long;
      Amount : Natural) return Unsigned_Long_Long;

   function Shift_Right
     (Value  : Unsigned_Long_Long;
      Amount : Natural) return Unsigned_Long_Long;

   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);

end Types;
