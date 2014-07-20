------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                             O U T F I L E S                              --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;

package body Outfiles is

   ----------------
   -- Set_Output --
   ----------------

   function Set_Output (File_Name : Name_Id) return File_Descriptor is
   begin
      if not Print_On_Stdout then
         declare
            Fd : File_Descriptor;
         begin
            Get_Name_String (File_Name);

            --  Create a new file and overwrites existing file with
            --  the same name

            Fd := Create_File (Name_Buffer (1 .. Name_Len), Text);

            if Fd = Invalid_FD then
               raise Program_Error;
            end if;

            --  Setting the output

            Set_Output (Fd);
            return Fd;
         end;
      end if;

      return Invalid_FD;
   end Set_Output;

   --------------------
   -- Release_Output --
   --------------------

   procedure Release_Output (Fd : File_Descriptor) is
   begin
      if not Print_On_Stdout and then Fd /= Invalid_FD then
         Close (Fd);
         Set_Standard_Output;
      end if;
   end Release_Output;

end Outfiles;
