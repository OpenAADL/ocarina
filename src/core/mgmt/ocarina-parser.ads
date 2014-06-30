------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                       O C A R I N A . P A R S E R                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

with Locations; use Locations;
with Types;     use Types;

package Ocarina.Parser is

   type Parser_Subprogram is access function
     (AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location) return Node_Id;

   function Parse
     (Language  : Name_Id;
      AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location) return Node_Id;
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
