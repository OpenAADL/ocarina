------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . B A C K E N D S . C H E D D A R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010, GET-Telecom Paris.                   --
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
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Cheddar.Main;
with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Tree.Generator;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Namet; use Namet;

package body Ocarina.Backends.Cheddar is

   use Ocarina.Instances;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Expander;

   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   Generated_Sources_Directory : Name_Id := No_Name;
   Invoke_Cheddar : Boolean := False;
   Cheddarlite_Path : String_Access;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;

   begin
      Instance_Root := Instantiate_Model (AADL_Root);

      if No (Instance_Root) then
         Display_Error ("Cannot instantiate the AADL model", Fatal => True);
      end if;

      --  Expand the AADL instance

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);

      --  Abort if the construction of the XML tree failed

      if No (XML_Root) then
         Display_Error ("XML generation failed", Fatal => True);
      end if;

      Enter_Directory (Generated_Sources_Directory);

      --  Create the XML file

      XML_Tree.Generator.Generate (XML_Root);

      if Invoke_Cheddar then
         if Cheddarlite_Path = null then
            Cheddarlite_Path := Locate_Exec_On_Path ("cheddarlite");
         end if;

         declare
            Args : GNAT.OS_Lib.Argument_List (1 .. 4);
            Success : Boolean;
         begin
            Args (1) := new String'("-file");
            Args (2) := new String'("rma_impl_cheddar.xml");
            Args (3) := new String'("-request");
            Args (4) := new String'("all");

            Spawn (Cheddarlite_Path.all, Args, Success);
            if not Success then
               raise Program_Error;
            end if;
         end;
      end if;

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
         case Getopt ("* b o:") is
            when ASCII.NUL =>
               exit;

            when 'b' =>
               Invoke_Cheddar := True;

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

      Register_Backend ("cheddar", Generate'Access, Cheddar_XML);
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
      XTN.Set_Name (XML_Root, No_Name);
      XTN.Set_Units (XML_Root, XTU.New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (XML_Root, XTU.New_List (XTN.K_List_Id));

      XTU.Push_Entity (XML_Root);

      Cheddar.Main.Visit (E);

      XTU.Pop_Entity;
   end Visit_Architecture_Instance;

end Ocarina.Backends.Cheddar;
