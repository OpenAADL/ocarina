------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . B A C K E N D S . M A S T                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2012 ESA & ISAE.                    --
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

with Ocarina.Instances;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Utils;
--  with Ocarina.Backends.MAST_Tree.Nodes;
with Ocarina.Backends.MAST_Tree.Nutils;
with Ocarina.Backends.MAST.Main;
with Ocarina.Backends.MAST_Tree.Generator;
with GNAT.Command_Line; use GNAT.Command_Line;

with Namet; use Namet;

package body Ocarina.Backends.MAST is

   use Ocarina.Instances;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Expander;

--     package MTN renames Ocarina.Backends.MAST_Tree.Nodes;
   package MTU renames Ocarina.Backends.MAST_Tree.Nutils;

   Generated_Sources_Directory : Name_Id := No_Name;
   Remove_Generated_Sources    : Boolean := False;

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

      --  Expand the AADL instance

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);
      --  Abort if the construction of the XML tree failed

      if No (Mast_File) then
         Display_Error ("MAST generation failed", Fatal => True);
      end if;

      Enter_Directory (Generated_Sources_Directory);

      if not Remove_Generated_Sources then
         MAST_Tree.Generator.Generate (MAST_File);
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
         case Getopt ("* z o:") is
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

      Register_Backend ("mast", Generate'Access, MAST_Scheduling);
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
      MAST_File := MTU.Make_MAST_File
         (MTU.Make_Defining_Identifier
            (Get_String_Name ("mast-model")));
      MAST.Main.Visit (E);
   end Visit_Architecture_Instance;

   -------------------
   -- Get_MAST_File --
   -------------------

   function Get_MAST_File return Node_Id
   is
   begin
      return MAST_File;
   end Get_MAST_File;

end Ocarina.Backends.MAST;
