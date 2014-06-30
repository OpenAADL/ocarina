------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2007-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

with Types; use Types;

package Utils is

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id;

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;

   function Remove_Prefix_From_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id;
   --  This function returns a new name without the prefix. If the
   --  prefix does not exist, the returned name is equal to the given
   --  name

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;
   --  This function returns a new name without the suffix. If the
   --  suffix does not exist, the returned name is equal to the given
   --  name.

   procedure Capitalize (S : in out String);
   --  Change in S any leading character or any successor of an
   --  underscore into its corresponding uppercase character.

   function Quoted (S : String; D : Character := '"') return String; --  "
   function Quoted (S : String; D : Character := '"') return Name_Id; --  "
   function Quoted (N : Name_Id; D : Character := '"') return String; --  "
   function Quoted (N : Name_Id; D : Character := '"') return Name_Id; --  "
   --  Embrace string S or name N with character D

   function To_Lower (N : Name_Id) return Name_Id;
   function To_Upper (N : Name_Id) return Name_Id;

   function Replace_Char
     (Name : Name_Id;
      O    : Character;
      N    : Character) return Name_Id;
   --  Replace occurence of character O in the Name by character N.

   function Remove_Char (Name : Name_Id; O : Character) return Name_Id;
   --  Remove occurences of character O in the Name.

   function Is_Prefix (N1 : Name_Id; N2 : Name_Id) return Boolean;
   --  Is N1 a prefix of N2

end Utils;
