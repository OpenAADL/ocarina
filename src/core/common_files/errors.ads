with Locations; use Locations;
with Types;     use Types;
with Ada.Exceptions;

package Errors is

   procedure Display_Error  (S : String);
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

end Errors;
