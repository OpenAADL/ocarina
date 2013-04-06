------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . B A C K E N D S . P N                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

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

   use Namet;
   use Ocarina.Instances;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.PN.Components;
   use Ocarina.Backends.PN.Printer;
   use OPFT;
   use OPFC;
   use Output;
   use Ocarina.Backends.Utils;

   procedure Generate_TINA (AADL_Root : Types.Node_Id);
   procedure Generate_CAMI (AADL_Root : Types.Node_Id);

   -------------------
   -- Generate_TINA --
   -------------------

   procedure Generate_TINA (AADL_Root : Types.Node_Id) is
      PN_Generated, Instance_Root : Node_Id;

   begin
      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);

      --  Expand the AADL instance

      Expand (Instance_Root);

      if Present (Instance_Root) then
         --  Generate Petri Net

         PN_Generated := Process_Architecture_Instance (Instance_Root, 1);

         --  Set TINA printers
         Set_Printers (OPFT.Print_Place'Access,
                       OPFT.Print_Trans'Access,
                       OPFT.Print_Formalism_Information'Access);

         Set_Output (Create_File ("model.nd", Binary));
         Print_PN_Generated (PN_Generated);
         Set_Standard_Error;
      end if;
   end Generate_TINA;

   -------------------
   -- Generate_CAMI --
   -------------------

   procedure Generate_CAMI (AADL_Root : Types.Node_Id) is
      PN_Generated, Instance_Root : Node_Id;

   begin
      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);

      --  Expand the AADL instance

      Expand (Instance_Root);

      if Instance_Root /= No_Node then
         PN_Generated := Process_Architecture_Instance (Instance_Root, 0);
         Set_Printers (OPFC.Print_Place'Access,
                       OPFC.Print_Trans'Access,
                       OPFC.Print_Formalism_Information'Access);
         Set_Output (Create_File ("model.cami", Binary));
         Print_PN_Generated (PN_Generated);
         Set_Standard_Error;
      end if;
   end Generate_CAMI;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Types.Node_Id) is
   begin
      Generate_TINA (AADL_Root);
      Reset_Handlings;
      Generate_CAMI (AADL_Root);
   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
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
