------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                            L O C A T I O N S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with Types;

package Locations is

   type Location is record
      Base_Name : Types.Name_Id;  --  Base name of file
      Dir_Name  : Types.Name_Id;  --  Directory name of file
      Line      : Types.Int;      --  Index of current line in buffer
      First_Pos : Types.Text_Ptr; --  Index of first character in the line
      Last_Pos  : Types.Text_Ptr; --  Index of last character read on the line
      Scan      : Types.Text_Ptr; --  Index of current character in the line
      EOF       : Types.Text_Ptr; --  Index of very last character in buffer
      Buffer    : Types.Text_Buffer_Ptr;
   end record;

   No_Location : constant Location
     := Location'(Types.No_Name, Types.No_Name, 0, 0, 0, 0, 0, null);

   function Image (Loc : Location) return String;
   --  Return <base_name>:<line>:<column>. If Base_Name is null, then
   --  return null string.

   procedure Initialize
     (Loc    : in out Location;
      Name   : Types.Name_Id;
      Size   : Types.Int;
      Buffer : Types.Text_Buffer_Ptr);
   --  Initialize Loc in particular Buffer, Base_Name, Dir_Name and
   --  EOF. Scan, First and Last are automatically set at the
   --  beginning of the buffer.

   procedure Update_Name_And_Line
     (Loc  : in out Location;
      Name : Types.Name_Id;
      Line : Types.Int);
   --  Update Loc in particular Base_Name, Dir_Name and Line. This
   --  routine is used to deal with preprocessed files. The
   --  preprocessed file includes info on the file and the line in tis
   --  file which the current text comes from.

end Locations;
