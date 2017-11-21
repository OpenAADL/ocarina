------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . F E _ A A D L . L E X E R                 --
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

--  This package contains the lexical analyzer routines. This is used
--  by the parser for scanning AADL source files.  This package is
--  conformant to the AADL v1.0 issued on 2004-09

with Ocarina.Types;     use Ocarina.Types;
with Locations; use Locations;

with Ocarina.Files;
with Ocarina.ME_AADL.Tokens; use Ocarina.ME_AADL.Tokens;

package Ocarina.FE_AADL.Lexer is

   ---------------------------------------------------
   -- Global variables updated by the token scanner --
   ---------------------------------------------------

   Token : Token_Type;
   Token_Location : Location renames Ocarina.Files.Buffer_Location;
   Token_Name           : Name_Id;   --  for T_Identifier (Lower case)
   Token_Display_Name   : Name_Id;   --  for T_Identifier (Carbon copy)
   String_Literal_Value : Name_Id;   --  for T_String
   Raw_Text_Value       : Name_Id;   --  for T_Raw_Text

   Token_Owner          : Property_Owner_Token;
   --  for property owner categories which have right to a special
   --  treatement between Identifiers and Keywords

   Quiet                : Boolean := False;
   --  In some cases, we need the lexer to be silent an not to report
   --  token errors (especially when scanning through an annex clause
   --  to find the end annex toekn).

   function Current_Token_Image return String;
   --  Return an image of the current token

   function Load_File
     (File_Name : Name_Id) return Location renames
     Ocarina.Files.Load_File;

   procedure Save_Lexer
     (State : out Location) renames
     Ocarina.Files.Save_Location;

   procedure Restore_Lexer
     (State : Location) renames
     Ocarina.Files.Restore_Location;

   function End_Of_File return Boolean renames Ocarina.Files.End_Of_File;

   procedure Scan_Token (Ignore_Invalid_Character : Boolean := False);
   --  Scan token and update the global variables declared above.

   procedure Scan_Raw_Text (T : Token_Type);
   --  Scan all text until token T is reached. Scanned text is saved in name
   --  table, use Raw_Text_Value to retrieve text contents

   procedure Skip_Tokens
     (Delimiter         : Token_Type;
      Include_Delimiter : Boolean := True);
   --  Skip tokens until we find Delimiter
   --  This procedure verifies that skipped tokens are well embraced

   procedure Skip_Tokens
     (Delimiters        : Token_List_Type;
      Include_Delimiter : Boolean := True);
   --  Same as above, with each token T in given list, calls Skip_Tokens (T)

end Ocarina.FE_AADL.Lexer;
