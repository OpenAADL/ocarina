------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . P O _ H I _ R T S J           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.Namet;             use Ocarina.Namet;
with GNAT.Command_Line; use GNAT.Command_Line;

with Ocarina.Instances;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Build_Utils;
with Ocarina.Backends.RTSJ_Tree.Generator;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;
with Ocarina.Backends.PO_HI_RTSJ.Main;
with Ocarina.Backends.PO_HI_RTSJ.Activity;
with Ocarina.Backends.PO_HI_RTSJ.Deployment;
with Ocarina.Backends.PO_HI_RTSJ.Subprograms;
with Ocarina.Backends.PO_HI_RTSJ.Naming;
with Ocarina.Backends.PO_HI_RTSJ.Generated_Types;
with Ocarina.Backends.PO_HI_RTSJ.Transport_High_Level;

package body Ocarina.Backends.PO_HI_RTSJ is

   use Ocarina.Instances;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Build_Utils;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;
   use Ocarina.Backends.PO_HI_RTSJ.Main;
   use Ocarina.Backends.PO_HI_RTSJ.Activity;
   use Ocarina.Backends.PO_HI_RTSJ.Deployment;
   use Ocarina.Backends.PO_HI_RTSJ.Subprograms;
   use Ocarina.Backends.PO_HI_RTSJ.Naming;
   use Ocarina.Backends.PO_HI_RTSJ.Generated_Types;
   use Ocarina.Backends.PO_HI_RTSJ.Transport_High_Level;

   Generated_Sources_Directory : Name_Id := No_Name;
   Compile_Generated_Sources   : Boolean := False;
   Remove_Generated_Sources    : Boolean := False;
   --  Execute_Generated_Sources : Boolean := False;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      --  Generate source files
      Naming.Source_File.Visit (E);
      Main.Source_File.Visit (E);
      Deployment.Source_File.Visit (E);
      Subprograms.Source_File.Visit (E);
      Activity.Source_File.Visit (E);
      Generated_Types.Source_File.Visit (E);
      Transport_High_Level.Source_File.Visit (E);
   end Visit_Architecture_Instance;

   --------------
   -- Generate --
   --------------
   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
   begin
      --  Instantiate the AADL tree
      Instance_Root := Instantiate_Model (AADL_Root);

      --  Expand the AADL instance
      --  Add informations on the model to facilitate code generation
      Expand (Instance_Root);

      --  Visit the tree to generate source files and makefiles
      --  Abort if the construction of the Java tree failed
      Visit_Architecture_Instance (Instance_Root);

      if No (RTSJ_Root) then
         Display_Error ("Code generation failed", Fatal => True);
      end if;

      --  At this point, we have un valid Java tree
      --  We can begin the generation of Java source files

      --  Enter the output directory
      Enter_Directory (Generated_Sources_Directory);

      if Remove_Generated_Sources then
         Build_Utils.Makefiles.Clean (Instance_Root);
      else
         --  Create the source files
         RTSJ_Tree.Generator.Generate (RTSJ_Root);

         if Compile_Generated_Sources then
            Build_Utils.Makefiles.Build (Instance_Root);
         end if;
      end if;
      --  Leave the output directory
      Leave_Directory;
   end Generate;

   ----------
   -- Init --
   ----------
   procedure Init is
   begin
      Generated_Sources_Directory := Get_String_Name (".");
      Initialize_Option_Scan;
      loop
         case Getopt ("* b z e o:") is
            when ASCII.NUL =>
               exit;

            when 'b' =>
               Compile_Generated_Sources := True;

            when 'z' =>
               Remove_Generated_Sources := True;

            when 'e' =>
               Compile_Generated_Sources := True;
            --  Execute_Generated_Sources := True;

            when 'o' =>
               declare
                  D : constant String := Parameter;
               begin
                  if D'Length /= 0 then
                     Generated_Sources_Directory := Get_String_Name (D);
                  end if;
               end;

            when others =>
               null;

         end case;
      end loop;

      --  Register code generator for Real-Time Java
      Register_Backend ("polyorb_hi_rtsj", Generate'Access, PolyORB_HI_RTSJ);

      --  Initialize some units
      Ocarina.Backends.PO_HI_RTSJ.Runtime.Initialize;
   end Init;

   -----------
   -- Reset --
   -----------
   procedure Reset is
   begin
      Ocarina.Backends.PO_HI_RTSJ.Runtime.Reset;
   end Reset;

end Ocarina.Backends.PO_HI_RTSJ;
