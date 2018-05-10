------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . X M L _ C O M M O N . M A P P I N G   --
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

with Ocarina.Backends.Properties;

package Ocarina.Backends.XML_Common.Mapping is

   use Ocarina.Backends.Properties;

   function Map_Time (T : Time_Type) return Node_Id;

   function Map_To_XML_Node
     (Name  : String;
      Value : Unsigned_Long_Long) return Node_Id;
   function Map_To_XML_Node (Name : String; Value : Name_Id) return Node_Id;
   --  Build a node whose structure is <name>value</name>

   function Map_Node_Identifier_To_XML_Node
     (Name     : String;
      The_Node : Node_Id) return Node_Id;
   --  Build a node whose structure is <name>the_node'identifier</name>

   function Map_Node_Identifier_To_XML_Node
     (Name     : String;
      The_Node : Name_Id) return Node_Id;

end Ocarina.Backends.XML_Common.Mapping;
