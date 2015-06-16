------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . P O K _ C                --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Instances;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Generator;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Execution_Tests;
with Ocarina.Backends.Execution_Utils;
with Ocarina.Backends.POK_C.Runtime;
with Ocarina.Backends.POK_C.Naming;
with Ocarina.Backends.POK_C.Main;
with Ocarina.Backends.POK_C.Activity;
with Ocarina.Backends.POK_C.Deployment;
with Ocarina.Backends.C_Common.Types;
with Ocarina.Backends.C_Common.Subprograms;
with Ocarina.Backends.POK_C.Makefile;
with Ocarina.Backends.ARINC653_Conf;
with Ocarina.Backends.POK_Cheddar;
with Ocarina.Backends.XML_Tree.Generator;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;

package body Ocarina.Backends.POK_C is
   use Ocarina.Instances;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Tree.Generator;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Execution_Utils;
   use Ocarina.Backends.Execution_Tests;
   use Ocarina.Backends.POK_C.Deployment;
   use Ocarina.Backends.POK_C.Naming;
   use Ocarina.Backends.XML_Tree.Generator;
   use Ocarina.Backends.ARINC653_Conf;

   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;

   Generated_Sources_Directory : Name_Id := No_Name;
   Remove_Generated_Sources    : Boolean := False;
   Build_Generated_Sources     : Boolean := False;
   Do_Regression_Test          : Boolean := False;
   Do_Coverage_Test            : Boolean := False;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   ---------------------------------
   -- Display_Communication_Error --
   ---------------------------------

   procedure Display_Communication_Error is
   begin
      Display_Error
        ("This kind of communication is not handled " &
         "at this time, we support event data and data ports " &
         "for inter-partition communication and event data, data " &
         "and event ports for intra-partition communication",
         Fatal => True);
   end Display_Communication_Error;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
      Success       : Boolean := True;
   begin
      Instance_Root := Instantiate_Model (AADL_Root);

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);
      --  Abort if the construction of the C tree failed

      if No (AADL_Root) then
         Display_Error ("Code generation failed", Fatal => True);
      end if;

      --  Enter the output directory

      Enter_Directory (Generated_Sources_Directory);

      if not Remove_Generated_Sources then
         --  Create the source files

         C_Tree.Generator.Generate (C_Root);
      end if;

      if POK_Flavor /= DEOS and then POK_Flavor /= VXWORKS then
         Makefile.Visit (Instance_Root);
      end if;

      --  If the user requested to build the applications then build it

      if Build_Generated_Sources then
         --  Build the source files

         Makefile.Build (Instance_Root);
      end if;

      if POK_Flavor = ARINC653 then
         ARINC653_Conf.Visit_Architecture_Instance (Instance_Root);

         if ARINC653_Conf.Get_XML_Root /= No_Node then
            XML_Tree.Generator.Generate (ARINC653_Conf.Get_XML_Root);
         end if;
      end if;

      POK_Cheddar.Visit_Architecture_Instance (Instance_Root);
      XML_Tree.Generator.Generate (POK_Cheddar.Get_XML_Root);

      if Do_Regression_Test or else Do_Coverage_Test then
         --  Execution_Utils package initialization

         Execution_Utils.Init;
         Execution_Utils.Visit (Instance_Root);

         Execution_Tests.Init;

         if Do_Regression_Test then
            Success :=
              Execute_Regression_Test
                (Scenario_Dir.all,
                 Ref_Map,
                 Integer (Timeout));

            if not Create_Referencial then
               Write_Eol;
               if Success then
                  Write_Line ("--- All regression tests PASSED ---");
               else
                  Write_Line ("--- Regression tests FAILED ---");
               end if;
            end if;
         end if;

         if Do_Coverage_Test then
            Success :=
              Success and then Execute_Coverage_Test (Integer (Timeout));
         end if;

         --  Free memory

         Execution_Utils.Reset;
         Execution_Tests.Reset;
         Free (Scenario_Dir);

         --  Exit if non-regression tests failed

         if not Success then
            OS_Exit (1);
         end if;
      end if;

      --  Leave the output directory
      Leave_Directory;
   end Generate;

   function Use_ARINC653_API return Boolean is
   begin
      return POK_Flavor = ARINC653 or else POK_Flavor = DEOS
         or else POK_Flavor = VXWORKS;
   end Use_ARINC653_API;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Generated_Sources_Directory := Get_String_Name (".");
      Initialize_Option_Scan;
      loop
         case Getopt ("* b er ec z o: k:") is
            when ASCII.NUL =>
               exit;

            when 'k' =>
               if Parameter = "arinc653" then
                  POK_Flavor := ARINC653;
               end if;

               if Parameter = "deos" then
                  POK_Flavor := DEOS;
               end if;

               if Parameter = "vxworks653" then
                  POK_Flavor := VXWORKS;
               end if;

               if Parameter = "no-assert" then
                  Add_Assertions := False;
               end if;

            when 'b' =>
               Build_Generated_Sources := True;

            when 'e' =>
               if Full_Switch = "er" then
                  Do_Regression_Test := True;
               elsif Full_Switch = "ec" then
                  Do_Coverage_Test := True;
               end if;

            when 'z' =>
               Remove_Generated_Sources := True;

            when 'o' =>
               declare
                  D : constant String := Parameter;
               begin
                  if Full_Switch = "o" and then D'Length /= 0 then
                     Generated_Sources_Directory := Get_String_Name (D);
                  end if;
               end;

            when others =>
               null;
         end case;
      end loop;

      Register_Backend ("POK_C", Generate'Access, PolyORB_Kernel_C);
      Ocarina.Backends.POK_C.Runtime.Initialize;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ocarina.Backends.POK_C.Runtime.Reset;
   end Reset;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
      D : constant Node_Id := CTU.New_Node (CTN.K_HI_Distributed_Application);
      N : Name_Id;
   begin
      CTN.Set_Units (D, CTU.New_List (CTN.K_List_Id));
      CTN.Set_HI_Nodes (D, CTU.New_List (CTN.K_List_Id));
      N := Get_String_Name ("generated-code");
      CTN.Set_Name (D, N);

      C_Root := D;

      Naming.Header_File.Visit_First_Pass (E);
      Naming.Header_File.Visit_Second_Pass (E);
      C_Common.Types.Header_File.Visit (E, C_Root);
      Deployment.Header_File.Visit (E);
      C_Common.Subprograms.Header_File.Visit (E, C_Root);
      Activity.Header_File.Visit (E);

      C_Common.Types.Source_File.Visit (E, C_Root);
      Deployment.Source_File.Visit (E);
      C_Common.Subprograms.Source_File.Visit (E);
      Activity.Source_File.Visit (E);
      Main.Source_File.Visit (E);

   end Visit_Architecture_Instance;

end Ocarina.Backends.POK_C;
