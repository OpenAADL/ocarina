------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2014 ESA & ISAE.                       --
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
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains low level output routines used by the compiler
--  for writing error messages and informational output. It is also used
--  by the debug source file output routines (see Sprintf.Print_Eol).

with GNAT.OS_Lib;

with Types; use Types;

package Output is
   pragma Elaborate_Body (Output);

   -----------------
   -- Subprograms --
   -----------------

   procedure Set_Output (New_Output : GNAT.OS_Lib.File_Descriptor);
   --  Sets subsequent output to appear on the given file

   procedure Set_Standard_Error;
   --  Sets subsequent output to appear on the standard error file
   --  (whatever that might mean for the host operating system, if
   --  anything).

   procedure Set_Standard_Output;
   --  Sets subsequent output to appear on the standard output file
   --  (whatever that might mean for the host operating system, if
   --  anything). Output to standard output is the default mode before
   --  any call to either of the Set procedures.

   function Current_Output return GNAT.OS_Lib.File_Descriptor;
   --  Return the current output

   procedure Write_Char (C : Character);
   --  Write one character to the standard output file. Note that the
   --  character should not be LF or CR (use Write_Eol for end of line)

   procedure Write_Eol (N : Natural := 1);
   --  Write an end of line (whatever is required by the system in use,
   --  e.g. CR/LF for DOS, or LF for Unix) to the standard output file.
   --  This routine also empties the line buffer, actually writing it
   --  to the file. Note that Write_Eol is the only routine that causes
   --  any actual output to be written.

   procedure Write_Int (Val : Int);
   --  Write an integer value with no leading blanks or zeroes. Negative
   --  values are preceded by a minus sign).

   procedure Write_Str (S : String);
   --  Write a string of characters to the standard output file. Note that
   --  end of line is handled separately using WRITE_EOL, so the string
   --  should not contain either of the characters LF or CR, but it may
   --  contain horizontal tab characters.

   procedure Write_Line (S : String);
   --  Equivalent to Write_Str (S) followed by Write_Eol;

   function Column return Nat;
   pragma Inline (Column);
   --  Returns the number of the column about to be written (e.g. a value
   --  of 1 means the current line is empty).

   Space_Increment : Natural := 2;
   N_Space         : Natural := 0;

   procedure Decrement_Indentation;
   procedure Increment_Indentation;

   procedure Set_Space_Increment (Value : Natural);
   procedure Write_Indentation (Offset : Integer := 0);
   procedure Write_Space;

end Output;
