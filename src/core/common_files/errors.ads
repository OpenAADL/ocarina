------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                               E R R O R S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2012-2015 ESA & ISAE.                    --
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

with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
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

   --  Error reporting
   --  * by default, Exit_On_Error directly exists;
   --  * if Use_Exception_To_Exit is called, Exit_On_Error raises the
   --    Ocarina_Error exception

   procedure Use_Exception_To_Exit;
   Ocarina_Error : exception;

   procedure Exit_On_Error (Error : Boolean; Reason : String);

end Errors;
