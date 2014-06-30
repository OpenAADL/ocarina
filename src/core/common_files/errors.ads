------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                               E R R O R S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2012-2014 ESA & ISAE.                    --
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

with Locations; use Locations;
with Types;     use Types;
with Ada.Exceptions;

package Errors is

   procedure Display_Error (S : String);
   procedure DE (S : String) renames Display_Error;
   procedure Display_Warning (S : String);
   procedure DW (S : String) renames Display_Warning;
   procedure Display_Message (S : String);
   procedure DM (S : String) renames Display_Message;
   --  Display an error and output error message S. S may include
   --  meta-characters.
   --
   --  '%' designates a string representing Error_Name (N) where N is
   --  the number of '%' and '#' in the substring.
   --
   --  '#' designates a quoted string representing Error_Name (N).
   --
   --  '!' designates a location representing Error_Loc (L) where L is
   --  the number of '!' in the substring.
   --
   --  '$' designates an integer representing Error_Int (I) where I is
   --  the number of '$' in the substring.

   procedure Initialize;

   Error_Loc  : array (1 .. 2) of Location;
   Error_Int  : array (1 .. 2) of Int;
   Error_Name : array (1 .. 2) of Name_Id;

   N_Errors   : Int := 0;
   N_Warnings : Int := 0;

   procedure Display_Bug_Box (E : Ada.Exceptions.Exception_Occurrence);

   procedure Exit_On_Error (Error : Boolean; Reason : String);

end Errors;
