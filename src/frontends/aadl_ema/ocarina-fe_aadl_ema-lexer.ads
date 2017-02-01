------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . F E _ A A D L _ E M A . L E X E R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2015-2016 ESA & ISAE.                    --
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

--  This package contains the lexical analyzer routines.

with Locations;
with Ocarina.Types;
with Ocarina.Files;
with Ocarina.ME_AADL_EMA.EMA_Tokens;

package Ocarina.FE_AADL_EMA.Lexer is

   pragma Elaborate_Body (Lexer);

   use Ocarina.ME_AADL_EMA.EMA_Tokens;
   use Locations;
   use Ocarina.Types;

   procedure Scan_Token;
   --  Scans a token and updates global variables Token, Token_Name
   --  (for identifiers and literals) and Token_Location.

   procedure Scan_Token (T : Token_Type);
   --  Same as above. When the current token is not the expected token
   --  T, an error message is output and Token is set to T_Error. As a
   --  special case, when T_Semi_Colon is expected, we output an error
   --  location at the end of the line instead of the current token
   --  location.

   procedure Scan_Token (L : Token_List_Type);
   --  Same as above. When the current token is not in the list of the
   --  expected tokens L, an error message is output.

   procedure Scan_Property_Identifier (Resulted_Prefix : out Token_Type;
   Resulted_Prefix_Name : out Name_Id);
   --  it is similar to Scan_Identifier in the lexer.adb but we have to
   --  find the prefix 'EMV2::' or 'ARP4761::' or 'MILSTD882'

   function Next_Token return Token_Type;
   --  Return next token but do not update the lexer state that is
   --  Token, Token_Name and Token_Location.

   procedure Restore_Lexer (State : Location)
     renames Ocarina.Files.Restore_Location;

   procedure Save_Lexer (State : out Location)
     renames Ocarina.Files.Save_Location;

   procedure Skip_Declaration (Delimiter : Token_Type);
   --  Skip until we find a potential end of declaration. Delimiter
   --  indicates the kind of delimiters we are looking for (';', ',',
   --  ':','::') or ('{', '(', '[','->[') or ('}', ')', ']',']->').
   --  We ensure that the declaration is well embraced.

   procedure Skip_Line;
   --  Skip current line

   function Current_Token_Image return String;

   procedure Scan_Numeric_Literal_Value;
   --
   --  Numeric Literals:
   --
   --  There are two kinds of numeric literals, real literals and
   --  integer literals. A real literal is a numeric literal that
   --  includes a point; an integer literal is a numeric literal
   --  without a point.

   --  the only use is in case if the numeric number
   --  is an aadl integer or real

end Ocarina.FE_AADL_EMA.Lexer;
