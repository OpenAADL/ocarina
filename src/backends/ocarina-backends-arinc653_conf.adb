------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . A R I N C 6 5 3 _ C O N F        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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

with Ocarina.Instances;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Expander;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Tree.Generator;
with Ocarina.Backends.ARINC653_Conf.Partitions;
with Ocarina.Backends.ARINC653_Conf.Memory;
with Ocarina.Backends.ARINC653_Conf.Connections;
with Ocarina.Backends.ARINC653_Conf.Scheduling;
with Ocarina.Backends.ARINC653_Conf.Partition_HM;
with Ocarina.Backends.ARINC653_Conf.System_HM;
with Ocarina.Backends.ARINC653_Conf.Module_HM;
with Ocarina.Backends.Utils;

with GNAT.Command_Line; use GNAT.Command_Line;

with Namet; use Namet;

package body Ocarina.Backends.ARINC653_Conf is

   use Ocarina.Instances;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.XML_Tree.Generator;
   use Ocarina.Backends.Utils;

   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

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
            ("ARINC653 configuration generation failed", Fatal => True);
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

      Register_Backend ("ARINC653_Conf", Generate'Access, Statistics);
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
      Set_Str_To_Name_Buffer ("generated-code");
      XTN.Set_Name (XML_Root, Name_Find);
      XTN.Set_Units (XML_Root, XTU.New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (XML_Root, XTU.New_List (XTN.K_List_Id));

      XTU.Push_Entity (XML_Root);

      System_HM.First_Pass.Visit (E);
      System_HM.Visit (E);
      Module_HM.Visit (E);
      Partitions.Visit (E);
      Memory.Visit (E);
      Scheduling.Visit (E);
      Connections.Visit (E);
      Partition_HM.Visit (E);

      XTU.Pop_Entity;

   end Visit_Architecture_Instance;

   ------------------
   -- Get_XML_Root --
   ------------------

   function Get_XML_Root return Node_Id
   is
   begin
      return XML_Root;
   end Get_XML_Root;

end Ocarina.Backends.ARINC653_Conf;
