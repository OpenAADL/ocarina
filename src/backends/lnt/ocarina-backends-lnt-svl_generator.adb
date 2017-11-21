------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B A C K E N D S . L N T . S V L _ G E N E R A T O R    --
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

with Outfiles;        use Outfiles;
with Ocarina.Namet;   use Ocarina.Namet;
with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Ocarina.Output;  use Ocarina.Output;
with Ocarina.Options; use Ocarina.Options;

package body Ocarina.Backends.LNT.Svl_Generator is

   procedure Make_Demo_Svl;

   -------------------
   -- Make_SVL_File --
   -------------------

   procedure Make_SVL_File is
      Fd              : File_Descriptor;
   begin
      if Output_Filename /= No_Name and then
         Get_Name_String (Output_Filename) = "-"
      then
         Set_Standard_Output;
         Set_Space_Increment (3);
         Make_Demo_Svl;
      else
         Fd := Set_Output (Get_String_Name ("demo.svl"));
         Set_Space_Increment (3);
         Make_Demo_Svl;
         Close (Fd);
      end if;
   end Make_SVL_File;

   -------------------
   -- Make_Demo_Svl --
   -------------------

   procedure Make_Demo_Svl is
   begin
      --  LTS generation
      Write_Quoted_Str ("Main.bcg");
      Write_Str ("=  divbranching reduction of ");
      Write_Quoted_Str (Get_Name_String (System_Name) & "_Main.lnt");
      Write_Line (";");
      --  adding deadlock property
      Write_Line ("property PROPERTY_Deadlock is");
      Increment_Indentation;
      Write_Indentation;
      Write_Str ("deadlock of ");
      Write_Quoted_Str ("Main.bcg");
      Write_Line (";");
      Write_Indentation;
      Write_Line ("expected FALSE;");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end property;");
      --  adding deadlock property
      Write_Line ("property Scheduling_Test (ID) is");
      Increment_Indentation;
      Write_Indentation;
      Write_Quoted_Str ("Main_$ID.bcg");
      Write_Str ("=");
      Write_Quoted_Str ("Main.bcg");
      Write_Line ("|=");
      Write_Indentation;
      Write_Str ("[ true* . ");
      Write_Quoted_Str ("ACTIVATION_$ID !T_Error");
      Write_Line (" ] false;");
      Write_Indentation;
      Write_Line ("expected TRUE;");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end property;");

      --  for I in 1 .. Thread_Number loop
      --  Write_Line ("check Scheduling_Test (" & Integer'Image (I) & ");");
      --  end loop;

   end Make_Demo_Svl;

end Ocarina.Backends.LNT.Svl_Generator;
