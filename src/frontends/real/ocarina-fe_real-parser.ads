------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . F E _ R E A L . P A R S E R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

--  Parser for REAL

with Ocarina.Types;
with Locations;
with GNAT.Table;

package Ocarina.FE_REAL.Parser is
   use Ocarina.Types;
   use Locations;

   function Process
     (AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location;
      Container : Node_Id  := No_Node) return Node_Id;
   --  Proceed to parsing

   procedure Init;
   --  Initialize the parser

   procedure Load_REAL_Library (File_Name : Name_Id);

   package REAL_Libs is new GNAT.Table (Name_Id, Nat, 1, 10, 10);
   --  Table of REAL libraries to consider

end Ocarina.FE_REAL.Parser;
