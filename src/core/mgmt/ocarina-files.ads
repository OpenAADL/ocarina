------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                        O C A R I N A . F I L E S                         --
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

--  This package contains the lexical analyzer routines. This is used
--  by the parser for scanning AADL source files.  This package is
--  conformant to the AADL v1.0 issued on 2004-09

with GNAT.Table;

with Ocarina.Types;     use Ocarina.Types;
with Locations; use Locations;

package Ocarina.Files is

   package Sources is new GNAT.Table (Name_Id, Nat, 1, 10, 10);

   Buffer          : Text_Buffer_Ptr;
   Buffer_Location : Location;
   --  The file is loaded in Buffer and Buffer_Location is used to
   --  scan it.

   procedure Add_File_To_Parse_List
     (File_Name  : Name_Id;
      Add_Suffix : Boolean);
   --  Add File_Name to the list of files to be parsed. If Add_Suffix
   --  is true, and File_Name does not end with ".aadl", add this
   --  suffix. It Add_Suffix is false, and File_Name does not end with
   --  ".aadl", simply discard this name.

   function Search_File (File_Name : Name_Id) return Name_Id;

   function Load_File (File_Name : Name_Id) return Location;
   --  Load the file into the buffer

   procedure Save_Location (State : out Location);
   pragma Inline (Save_Location);
   --  Save the current location to go back to it if necessary.

   procedure Restore_Location (State : Location);
   pragma Inline (Restore_Location);
   --  Go back to the location specified in the 'State' parameter

   procedure New_Line;
   --  Increment the line number and save the current position in the
   --  buffer in order to compute later on the column number.

   procedure Skip_Line;
   --  Skip current line

   procedure Skip_Spaces;
   --  Skip all spaces and empty lines

   function End_Of_File return Boolean;
   --  Return TRUE if there is no more useful data in file or EOF is reached

end Ocarina.Files;
