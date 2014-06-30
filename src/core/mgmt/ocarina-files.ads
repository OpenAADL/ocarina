------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                        O C A R I N A . F I L E S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

--  This package contains the lexical analyzer routines. This is used
--  by the parser for scanning AADL source files.  This package is
--  conformant to the AADL v1.0 issued on 2004-09

with GNAT.Table;

with Types;     use Types;
with Locations; use Locations;

package Ocarina.Files is

   package Sources is new GNAT.Table (Name_Id, Nat, 1, 10, 10);

   Buffer          : Text_Buffer_Ptr;
   Buffer_Location : Location;
   --  The file is loaded in Buffer and Buffer_Location is used to
   --  scan it.

   procedure Add_File_To_Parse_List
     (File_Name  : Name_Id;
      Add_Suffix : Boolean := True);

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
