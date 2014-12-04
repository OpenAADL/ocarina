------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                     O C A R I N A . B A C K E N D S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

--  This package is the root of all the source code generators of
--  Ocarina. It provides routines to register and select a code
--  generators. All the code generators should be implemented as child
--  packages of Ocarina.Backends. All the calls to these generators
--  have to be done through the procedure Generate exported by this
--  package. After selecting the wanted code generator.

with Ocarina.Types; use Ocarina.Types;

package Ocarina.Backends is

   type Backend_Subprogram is access procedure (Instance : Node_Id);
   --  Each code generator has to define such a subprogram. Instance
   --  is the root of the AADL instance tree from which the user wants
   --  to generate the code.

   type Backend_Kind is
     (Invalid_Backend,
      AADL,
      AADL_Min,
      AADL_Annex,
      AADLXML,
      ASN1_Deployment,
      Behavior_PP,
      Bound_T,
      Carts_XML,
      Cheddar_XML,
      Deos_XML,
      Connection_Matrix_Analysis,
      Functions_Matrix_Analysis,
      MAST_Scheduling,
      Petri_Nets,
      PolyORB_HI_Ada,
      PolyORB_HI_C,
      PolyORB_HI_RTSJ,
      PolyORB_Kernel_C,
      Statistics,
      Subprograms_Generator,
      Xtratum_Configuration,
      REAL_PP,
      REAL_Theorem,
      Alloy_Backend);
   --  Supported code generators. For each kind, at most one generator
   --  must be implemented.

   procedure Register_Backend
     (Name    : String;
      Process : Backend_Subprogram;
      Kind    : Backend_Kind);
   --  Register a new backend.

   function Get_Current_Backend_Kind return Backend_Kind;
   procedure Set_Current_Backend_Name (Name : String);
   function Get_Current_Backend_Name return Name_Id;

   procedure Generate_Code (Root : Node_Id; Backend_Name : Name_Id := No_Name);
   --  Call the backend name if it is set or the current backend name
   --  set in the command line. If the backend name and the current
   --  backend name are set, backend name has the priority.

   procedure Init;
   --  Initialize the Backends module by registering the several
   --  implemented code generators.

   procedure Reset;
   --  Resets the Backends module by resetting the node entries of
   --  the several trees.

   procedure Usage;
   --  Print usage for all registered backends

end Ocarina.Backends;
