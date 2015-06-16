------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2007-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Types; use Ocarina.Types;

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
