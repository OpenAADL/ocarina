------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                       O C A R I N A . P A R S E R                        --
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

with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;

package Ocarina.Parser is

   type Parser_Subprogram is access function
     (AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location;
      Container : Node_Id  := No_Node) return Node_Id;

   function Parse
     (Language  : Name_Id;
      AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location;
      Container : Node_Id  := No_Node) return Node_Id;
   --  Parse the file File_Name and return the Node_Id or the root of
   --  the resulting AADL tree, or No_Node if the parsing failed. If
   --  AADL_Root is not No_Node, then add the parsed entities to
   --  AADL_Root. This function automatically selects the appropriate
   --  parser, according to the file name.

   procedure Init_Parsers;
   --  Initialize all the registered parsers

   procedure Register_Parser (Language : String; Parser : Parser_Subprogram);

   procedure Reset_Parsers;
   --  Unregister all the registered parsers

end Ocarina.Parser;
