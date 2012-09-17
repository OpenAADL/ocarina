------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                        O C A R I N A . F I L E S                         --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.OS_Lib;

with Errors;
with Namet;

with Ocarina.Options;       use Ocarina.Options;

package body Ocarina.Files is

   use ASCII;
   use GNAT.OS_Lib;

   use Errors;
   use Namet;

   ----------------------------
   -- Add_File_To_Parse_List --
   ----------------------------

   procedure Add_File_To_Parse_List
     (File_Name : Name_Id; Add_Suffix : Boolean := True)
   is

      File_Name_With_Extension : Name_Id;
      Do_Add : Boolean := True;

   begin
      Get_Name_String (File_Name);
      if Add_Suffix then
         if Name_Len < 5
          or else Name_Buffer (Name_Len - 4 .. Name_Len) /= ".aadl"
         then
            Add_Str_To_Name_Buffer (".aadl");
         end if;
      end if;
      File_Name_With_Extension := Name_Find;

      for J in 1 .. Sources.Last loop
         if File_Name_With_Extension = Sources.Table (J) then
            Do_Add := False;
         end if;
      end loop;

      if Do_Add then
         Sources.Append (File_Name_With_Extension);
      end if;
   end Add_File_To_Parse_List;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File return Boolean is
   begin
      Skip_Spaces;
      return Buffer_Location.Scan > Buffer_Location.EOF;
   end End_Of_File;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      if Buffer (Buffer_Location.Scan) = CR
        and then Buffer (Buffer_Location.Scan + 1) = LF
      then
         Buffer_Location.Scan  := Buffer_Location.Scan + 2;

         --  Microsoft OS's
      else
         Buffer_Location.Scan  := Buffer_Location.Scan + 1;

         --  Other OS's
      end if;

      Buffer_Location.First_Pos := Buffer_Location.Scan;
      Buffer_Location.Last_Pos  := Buffer_Location.Scan;
      Buffer_Location.Line      := Buffer_Location.Line + 1;
   end New_Line;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (File_Name : Name_Id) return Location is
      File_Desc : File_Descriptor;
      Result    : Integer;
      Length    : Integer;
   begin
      --  Open the file

      Get_Name_String (File_Name);
      Name_Buffer (Name_Len + 1) := NUL;

      File_Desc := Open_Read (Name_Buffer'Address, Binary);
      if File_Desc = Invalid_FD then
         DE ("cannot open file " & Get_Name_String (File_Name));
         return No_Location;
      end if;

      --  Load file in a buffer

      Length := Integer (File_Length (File_Desc));
      Buffer := new Text_Buffer (1 .. Text_Ptr (Length + 1));

      Buffer_Location.Scan := 1;
      loop
         Result := Read
           (File_Desc, Buffer (Buffer_Location.Scan)'Address, Length);

         exit when Result = Length;

         if Result <= 0 then
            DE ("cannot read file " & Get_Name_String (File_Name));
            return No_Location;
         end if;

         Buffer_Location.Scan := Buffer_Location.Scan + Text_Ptr (Result);
         Length  := Length - Result;
      end loop;

      Close (File_Desc);
      --  Source file is totally loaded

      Initialize (Buffer_Location, File_Name, Int (Length), Buffer);
      return Buffer_Location;
   end Load_File;

   ----------------------
   -- Restore_Location --
   ----------------------

   procedure Restore_Location (State : Location) is
   begin
      Buffer_Location := State;
      Buffer          := State.Buffer;
   end Restore_Location;

   -------------------
   -- Save_Location --
   -------------------

   procedure Save_Location (State : out Location) is
   begin
      State := Buffer_Location;
   end Save_Location;

   -----------------
   -- Search_File --
   -----------------

   function Search_File (File_Name : Name_Id) return Name_Id is
   begin
      if File_Name = No_Name then
         return No_Name;
      end if;

      --  Check in the current directory first since -I- is not allowed

      Get_Name_String (File_Name);

      if Name_Len = 0 then
         return No_Name;
      end if;

      if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
         return File_Name;
      end if;

      --  Check in the library paths

      for Path in Library_Paths.First .. Library_Paths.Last loop
         Get_Name_String (Library_Paths.Table (Path));
         Add_Char_To_Name_Buffer (Directory_Separator);
         Get_Name_String_And_Append (File_Name);

         if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
            return Name_Find;
         end if;
      end loop;

      return No_Name;
   end Search_File;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line is
   begin
      while Buffer_Location.Scan <= Buffer_Location.EOF loop
         case Buffer (Buffer_Location.Scan) is
            when LF | FF | CR | VT =>
               New_Line;
               exit;
            when others =>
               null;
         end case;

         Buffer_Location.Scan := Buffer_Location.Scan + 1;
      end loop;
   end Skip_Line;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
   begin
      while Buffer_Location.Scan <= Buffer_Location.EOF loop
         case Buffer (Buffer_Location.Scan) is
            when ' ' | HT =>
               Buffer_Location.Scan := Buffer_Location.Scan + 1;
            when LF | FF | CR | VT =>
               New_Line;
            when others =>
               exit;
         end case;
      end loop;
   end Skip_Spaces;

end Ocarina.Files;
