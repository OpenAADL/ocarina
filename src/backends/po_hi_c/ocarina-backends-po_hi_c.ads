------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . B A C K E N D S . P O _ H I _ C              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2016 ESA & ISAE.      --
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
   C_Root : Node_Id := No_Node;
   --  The root of the C trees

end Ocarina.Backends.PO_HI_C;
