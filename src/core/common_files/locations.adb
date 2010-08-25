------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                            L O C A T I O N S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2008-2009, GET-Telecom Paris.                --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Directory_Operations;

with Namet;
with Types; use type Types.Name_Id, Types.Int, Types.Text_Ptr;

package body Locations is

   -----------
   -- Image --
   -----------

   function Image (Loc : Location) return String is
      Column : constant Types.Nat
        := Types.Nat (Loc.Last_Pos - Loc.First_Pos + 1);
      Backup : Types.Name_Id;
      --  Backup name buffer
      Result : Types.Name_Id;
      --  Store returned value before restoring name buffer
   begin
      if Loc.Base_Name = Types.No_Name then
         return Types.No_Str;

      else
         --  A critical issue consist in preserving Name_Buffer. In
         --  other words, this function must not have side effect.

         --  Save name buffer to restore it later on
         Backup := Namet.Name_Find;

         Namet.Get_Name_String (Loc.Base_Name);
         Namet.Add_Char_To_Name_Buffer (':');
         Namet.Add_Nat_To_Name_Buffer (Types.Nat (Loc.Line));
         Namet.Add_Char_To_Name_Buffer (':');
         if Column < 10 then
            Namet.Add_Char_To_Name_Buffer ('0');
         end if;
         Namet.Add_Nat_To_Name_Buffer (Column);
         Result := Namet.Name_Find;

         --  Restore backup into name buffer

         if Backup /= Types.No_Name then
            Namet.Get_Name_String (Backup);
         end if;

         --  Return result using a Get_Name_String variant with no side effect
         return Namet.Get_Name_String (Result);
      end if;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Loc    : in out Location;
      Name   : Types.Name_Id;
      Size   : Types.Int;
      Buffer : Types.Text_Buffer_Ptr)
   is
   begin
      Loc.Base_Name :=
        Namet.Get_String_Name (GNAT.Directory_Operations.Base_Name
                                 (Namet.Get_Name_String (Name)));
      Loc.Dir_Name  :=
        Namet.Get_String_Name (GNAT.Directory_Operations.Dir_Name
                                 (Namet.Get_Name_String (Name)));
      Loc.Line      := 1;
      Loc.First_Pos := 1;
      Loc.Last_Pos  := 1;
      Loc.Scan      := 1;
      Loc.EOF       := Types.Text_Ptr (Size);
      Loc.Buffer    := Buffer;
   end Initialize;

   --------------------------
   -- Update_Name_And_Line --
   --------------------------

   procedure Update_Name_And_Line
     (Loc  : in out Location;
      Name : Types.Name_Id;
      Line : Types.Int)
   is
   begin
      Loc.Line      := Line;
      Loc.Base_Name :=
        Namet.Get_String_Name (GNAT.Directory_Operations.Base_Name
                                 (Namet.Get_Name_String (Name)));
      Loc.Dir_Name  :=
        Namet.Get_String_Name (GNAT.Directory_Operations.Dir_Name
                                 (Namet.Get_Name_String (Name)));
   end Update_Name_And_Line;

end Locations;
