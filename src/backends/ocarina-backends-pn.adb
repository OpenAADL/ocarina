------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . B A C K E N D S . P N                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2008-2009, GET-Telecom Paris.                --
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

with Namet;

with Ocarina.Backends.Expander;
with Ocarina.Backends.PN.Components;
with Ocarina.Backends.PN.Nodes;
with Ocarina.Instances;

with Ocarina.Backends.PN.Format.Tina;
with Ocarina.Backends.PN.Format.Cami;

with Ocarina.Backends.PN.Printer;

with Output;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.PN is

   package OPFT renames Ocarina.Backends.PN.Format.Tina;
   package OPFC renames Ocarina.Backends.PN.Format.Cami;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Types.Node_Id) is
      use Namet;
      use Ocarina.Instances;
      use Ocarina.Backends.Expander;
      use Ocarina.Backends.PN.Components;
      use Ocarina.Backends.PN.Printer;
      use OPFT;
      use OPFC;
      use Output;
      use Ocarina.Backends.Utils;

      Pn_Generated, Instance_Root : Node_Id;
      pragma Warnings (Off, Pn_Generated);

   begin
      Write_Line ("------------------------------------------");
      Write_Line ("------ Ocarina Petri Nets Generator ------");
      Write_Line ("------------------------------------------");
      Write_Line (" ");

      -----------
      --  work for TPN generation

      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);

      --  Expand the AADL instance

      Expand (Instance_Root);

      if Instance_Root /= No_Node then
         Pn_Generated := Process_Architecture_Instance (Instance_Root, 1);
      else
         Pn_Generated := No_Node;
      end if;

      if Pn_Generated /= No_Node then
         Set_Printers (OPFT.Print_Place'Access,
                       OPFT.Print_Trans'Access,
                       OPFT.Print_Formalism_Information'Access);
         Write_Line ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
         Write_Line ("~~~~~~~~~~~ Timed Petri Nets ~~~~~~~~~~~");
         Write_Line ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
         Print_Pn_Generated (Pn_Generated);
      end if;

      Reset_Handlings;
      -----------
      --  work for CPN generation

      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);

      --  Expand the AADL instance

      Expand (Instance_Root);

      if Instance_Root /= No_Node then
         Pn_Generated := Process_Architecture_Instance (Instance_Root, 0);
      else
         Pn_Generated := No_Node;
      end if;

      if Pn_Generated /= No_Node then
         Set_Printers (OPFC.Print_Place'Access,
                       OPFC.Print_Trans'Access,
                       OPFC.Print_Formalism_Information'Access);
         Write_Line ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
         Write_Line ("~~~~~~~~~~~ Colored Petri Nets ~~~~~~~~~~~");
         Write_Line ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
         Print_Pn_Generated (Pn_Generated);
      end if;

   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
      use Namet;
   begin
      Set_Str_To_Name_Buffer ("_");
      Separator := Name_Find;
      Register_Backend ("petri_nets", Generate'Access, Petri_Nets);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ocarina.Backends.PN.Nodes.Entries.Init;
   end Reset;

end Ocarina.Backends.PN;
