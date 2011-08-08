------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . B A C K E N D S . P O _ H I _ C              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2008-2010, European Space Agency (ESA).           --
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

--  This is the root unit for the C code generator to the PolyORB-HI
--  middleware.

package Ocarina.Backends.PO_HI_C is

   procedure Generate (AADL_Root : Node_Id);
   --  The main entry point of the PolyORB-HI code
   --  generator. AADL_Root is the tree root of an AADL model
   --  instance.

   procedure Init;
   --  Fills the corresponding location in the generator table by the
   --  information on this generator and execute some initialization
   --  routines necessary for its work.

   procedure Reset;
   --  Reset the internal units of the PO_HI_C generator

   function Use_Performance_Analysis return Boolean;
   --  Specify if we use performance analysis or not.

   procedure Set_Performance_Analysis (Use_It : Boolean);
   --  Indicate to the generator if we use performance
   --  analysis or not.

   procedure Set_ASN1_Deployment (Use_It : Boolean);
   --  Indicate if we would like to generate deployment
   --  data using ASN1.

   function Use_ASN1_Deployment return Boolean;
   --  Just returns a boolean to indicate if the user
   --  wants the ASN1 deployment information.

private
   C_Root : Node_Id;
   --  The root of the C trees

end Ocarina.Backends.PO_HI_C;
