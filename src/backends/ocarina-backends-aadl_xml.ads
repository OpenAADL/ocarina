------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . B A C K E N D S . A A D L _ X M L             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2011-2015 ESA & ISAE.                    --
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

--  This backend generates an XML file compatible with the AADL_XML profile

package Ocarina.Backends.AADL_XML is

   procedure Generate (AADL_Root : Node_Id);
   --  The main entry point of the AADL_XML backend

   procedure Init;
   --  Fills the corresponding location in the generator table by the
   --  information on this generator and execute some initialization
   --  routines necessary for its work.

   procedure Reset;

private
   XML_Root                : Node_Id;
   Current_XML_Node        : Node_Id;
   Distributed_Application : Node_Id;
   HI_Node                 : Node_Id;
   HI_Unit                 : Node_Id;
   --  The root of the XML trees

end Ocarina.Backends.AADL_XML;
