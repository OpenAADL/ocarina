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
      Write_Str ("% DEFAULT_MCL_LIBRARIES=");
      Write_Quoted_Str ("standard.mcl");
      Write_Line ("");
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

      --  adding Scheduling_Test property
      Write_Line ("property Scheduling_Test (ID)");
      Increment_Indentation;
      Write_Indentation;
      Write_Quoted_Str ("Scheduling Test ");
      Write_Line ("is");
      Write_Quoted_Str ("Main.bcg");
      Write_Line ("|= with evaluator3");
      Write_Indentation;
      Write_Str ("NEVER (");
      Write_Quoted_Str ("ACTIVATION_$ID !T_Error");
      Write_Line (");");
      Write_Indentation;
      Write_Line ("expected TRUE;");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end property;");

      for I in 1 .. Thread_Number loop
         Write_Line ("check Scheduling_Test (" & Integer'Image (I) & ");");
      end loop;

      --  adding Connection property
      Write_Line ("property Connection (ID)");
      Increment_Indentation;
      Write_Indentation;
      Write_Quoted_Str ("Connection Test ");
      Write_Line ("is");
      Write_Quoted_Str ("Main.bcg");
      Write_Line ("|= with evaluator3");
      Write_Indentation;
      Write_Str ("AFTER_1_INEVITABLE_2 ");
      Write_Str ("('SEND_$ID !*' , 'RECEIVE_$ID !*')");
      Write_Line (";");
      Write_Indentation;
      Write_Line ("expected TRUE;");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end property;");

      --  adding Is_Preempted property
      Write_Line ("property Is_Preempted (ID)");
      Increment_Indentation;
      Write_Indentation;
      Write_Quoted_Str ("Is_Preempted Test ");
      Write_Line ("is");
      Write_Quoted_Str ("Main.bcg");
      Write_Line ("|= with evaluator3");
      Write_Indentation;
      Write_Str ("SOME (");
      Write_Str ("true* .");
      Write_Quoted_Str ("ACTIVATION_$ID !T_DISPATCH_PREEMPTION");
      Write_Str (". true* . (");
      Write_Quoted_Str ("ACTIVATION_$ID !T_PREEMPTION");
      Write_Str (")* . true* . ");
      Write_Quoted_Str ("ACTIVATION_$ID !T_PREEMPTION_COMPLETION");
      Write_Line (");");
      Write_Indentation;
      Write_Line ("expected TRUE;");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end property;");

      for I in 1 .. Thread_Number loop
         Write_Line ("check Is_Preempted (" & Integer'Image (I) & ");");
      end loop;

      --  adding Data_Loss property
      Write_Line ("property Data_Loss (ID)");
      Increment_Indentation;
      Write_Indentation;
      Write_Quoted_Str ("Between two consecutive SEND_$ID actions," &
         " there is a RECEIVE_$ID");
      Write_Line (" is");
      Write_Quoted_Str ("Main.bcg");
      Write_Line ("|= with evaluator3");
      Write_Indentation;
      Write_Str ("NEVER (");
      Write_Str ("true* .");
      Write_Quoted_Str ("SEND_$ID !AADLDATA");
      Write_Str (". (not");
      Write_Quoted_Str ("RECEIVE_$ID !AADLDATA");
      Write_Str (")* .");
      Write_Quoted_Str ("SEND_$ID !AADLDATA");
      Write_Line (");");
      Write_Indentation;
      Write_Line ("expected TRUE;");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end property;");

      --  adding Transition property
      Write_Line ("property Transition (TH_NAME, Si, Sj)");
      Increment_Indentation;
      Write_Indentation;
      Write_Quoted_Str ("Thread $TH_NAME, being in state $Si," &
         " the corresponding $Sj state is eventually reachable");
      Write_Line (" is");
      Write_Quoted_Str ("Main.bcg");
      Write_Line ("|= with evaluator3");
      Write_Indentation;
      Write_Str ("AFTER_1_INEVITABLE_2 (");
      Write_Quoted_Str ("DISPLAY_STATE !$TH_NAME !$Si");
      Write_Str (",");
      Write_Quoted_Str ("DISPLAY_STATE !$TH_NAME !$Sj");
      Write_Line (");");
      Write_Indentation;
      Write_Line ("expected TRUE;");
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end property;");
   end Make_Demo_Svl;

end Ocarina.Backends.LNT.Svl_Generator;
