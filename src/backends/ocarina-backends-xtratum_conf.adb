------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . B A C K E N D S . X T R A T U M _ C O N F         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2015 ESA & ISAE.                    --
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
with Ocarina.Backends.Messages;
with Ocarina.Backends.Expander;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Tree.Generator;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Xtratum_Conf.System_Description;
with Ocarina.Backends.Xtratum_Conf.Hardware_Description;
with Ocarina.Backends.Xtratum_Conf.Xm_Hypervisor;
with Ocarina.Backends.Xtratum_Conf.Resident_Sw;
with Ocarina.Backends.Xtratum_Conf.Partition_Table;
with Ocarina.Backends.Xtratum_Conf.Channels;

with GNAT.Command_Line; use GNAT.Command_Line;

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.Xtratum_Conf is

   use Ocarina.Instances;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.XML_Tree.Generator;
   use Ocarina.Backends.Utils;

   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   Remove_Generated_Sources    : Boolean := False;
   Generated_Sources_Directory : Name_Id := No_Name;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
   begin

      Instance_Root := Instantiate_Model (AADL_Root);

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);
      --  Abort if the construction of the tree failed

      if No (XML_Root) then
         Display_Error
           ("Xtratum configuration generation failed",
            Fatal => True);
      end if;

      --  At this point, we have a valid tree, we can begin the XML
      --  file generation.

      --  Enter the output directory

      Enter_Directory (Generated_Sources_Directory);

      if not Remove_Generated_Sources then
         --  Create the source files

         XML_Tree.Generator.Generate (XML_Root);

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
         case Getopt ("* b z o:") is
            when ASCII.NUL =>
               exit;

            when 'z' =>
               Remove_Generated_Sources := True;

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

      Register_Backend
        ("Xtratum_Configuration",
         Generate'Access,
         Xtratum_Configuration);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      XML_Root := XTU.New_Node (XTN.K_HI_Distributed_Application);
      Get_Name_String
        (Normalize_Name (AIN.Name (AIN.Identifier (Root_System (E)))));
      XTN.Set_Name (XML_Root, Name_Find);
      XTN.Set_Units (XML_Root, XTU.New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (XML_Root, XTU.New_List (XTN.K_List_Id));

      XTU.Push_Entity (XML_Root);

      System_Description.Visit (E);
      Hardware_Description.Visit (E);
      Xm_Hypervisor.Visit (E);
      Resident_Sw.Visit (E);
      Partition_Table.Visit (E);
      Channels.Visit (E);

      XTU.Pop_Entity;
   end Visit_Architecture_Instance;

   ------------------
   -- Get_XML_Root --
   ------------------

   function Get_XML_Root return Node_Id is
   begin
      return XML_Root;
   end Get_XML_Root;

end Ocarina.Backends.Xtratum_Conf;
