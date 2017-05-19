------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.LNT.TREE_GENERATOR_PORT                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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
with Ocarina.Backends;
with GNAT.OS_Lib;   use GNAT.OS_Lib;
with Ocarina.Backends.Build_Utils;
use Ocarina.Backends.Build_Utils;
with Ocarina.Backends.Messages;
use Ocarina.Backends.Messages;
with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.Backends.LNT.Nutils;
use Ocarina.Backends.LNT.Nutils;
with Ada.Text_IO; use Ada.Text_IO;

package body Ocarina.Backends.LNT.Tree_Generator_Port is
   procedure Generate_LNT_Port is
      Is_Done : Boolean := False;
   begin
      Put_Line ("Begin Port");

      if Is_Empty (LNT_States_List) then
         Set_Str_To_Name_Buffer (Get_Runtime_Path ("lnt") &
          Directory_Separator &
           "LNT_Generic_Process_For_Port_Connections.lnt");
         Copy_File (Get_Name_String (Name_Find),
          Get_Name_String (Generated_Sources_Directory), Is_Done, Overwrite);
      else
         Set_Str_To_Name_Buffer (Get_Runtime_Path ("lnt") &
          Directory_Separator &
           "LNT_Generic_Process_For_Port_Connections_BA.lnt");
         Copy_File (Get_Name_String (Name_Find),
          Get_Name_String (Generated_Sources_Directory), Is_Done, Overwrite);
      end if;
      if not Is_Done then
         Display_Error ("Problem in LNT Port_Connections module generation !",
            Fatal   => False,
            Warning => True);
      end if;
   end Generate_LNT_Port;

end Ocarina.Backends.LNT.Tree_Generator_Port;
