------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                            L O C A T I O N S                             --
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

with Ocarina.Types;

package Locations is

   type Location is record
      Base_Name : Ocarina.Types.Name_Id;
      --  Base name of file
      Dir_Name  : Ocarina.Types.Name_Id;
      --  Directory name of file
      Line      : Ocarina.Types.Int;
      --  Index of current line in buffer
      First_Pos : Ocarina.Types.Text_Ptr;
      --  Index of first character in the line
      Last_Pos  : Ocarina.Types.Text_Ptr;
      --  Index of last character read on the line
      Scan      : Ocarina.Types.Text_Ptr;
      --  Index of current character in the line
      EOF       : Ocarina.Types.Text_Ptr;
      --  Index of very last character in buffer
      Buffer    : Ocarina.Types.Text_Buffer_Ptr;
   end record;

   No_Location : constant Location :=
     Location'(Ocarina.Types.No_Name,
               Ocarina.Types.No_Name, 0, 0, 0, 0, 0, null);

   function Image (Loc : Location) return String;
   --  Return <base_name>:<line>:<column>. If Base_Name is null, then
   --  return null string.

   procedure Initialize
     (Loc    : in out Location;
      Name   :        Ocarina.Types.Name_Id;
      Size   :        Ocarina.Types.Int;
      Buffer :        Ocarina.Types.Text_Buffer_Ptr);
   --  Initialize Loc in particular Buffer, Base_Name, Dir_Name and
   --  EOF. Scan, First and Last are automatically set at the
   --  beginning of the buffer.

   procedure Update_Name_And_Line
     (Loc  : in out Location;
      Name :        Ocarina.Types.Name_Id;
      Line :        Ocarina.Types.Int);
   --  Update Loc in particular Base_Name, Dir_Name and Line. This
   --  routine is used to deal with preprocessed files. The
   --  preprocessed file includes info on the file and the line in tis
   --  file which the current text comes from.

end Locations;
