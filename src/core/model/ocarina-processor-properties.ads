------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . P R O C E S S O R . P R O P E R T I E S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Types;

package Ocarina.Processor.Properties is

   use Ocarina.Types;

   procedure Diffuse_Package_Properties_To_Entities (Root : Node_Id);
   --  Move properties of packages to the appropriate declarations

   function Compute_Property_Values (Root : Node_Id) return Boolean;
   --  Resolve all the property values, so that all property values
   --  are explicit (no more value() statements). Return True if there
   --  was no problem during the resolution process.

   function Convert_To_Base (L : Node_Id; U : Node_Id) return Node_Id;
   --  Converts the literal L associated to the unit U into a literal
   --  associated with the base dentifier of the units type.

end Ocarina.Processor.Properties;
