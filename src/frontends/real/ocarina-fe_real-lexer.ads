------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . F E _ R E A L . L E X E R                 --
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

--  Lexer for the REAL frontend

with Ocarina.Types;
with Locations;
with Ocarina.Files;
with Ocarina.ME_REAL.Tokens;

package Ocarina.FE_REAL.Lexer is

   pragma Elaborate_Body (Lexer);

   use Ocarina.ME_REAL.Tokens;
   use Ocarina.Types;
   use Locations;

   procedure Scan_Token;
   --  Scan token and update global variables Token, Token_Name
   --  (for identifiers and literals) and Token_Location.

   procedure Scan_Token (T : Token_Type);
   --  Same as above. When the current token is not the expected token
   --  T, an error message is output and Token is set to T_Error. As a
   --  special case, when T_Semi_Colon is expected, we output an error
   --  location at the end of the line instead of the current token
   --  location.

   function Next_Token return Token_Type;
   --  Return next token but do not update the lexer state that is
   --  Token, Token_Name and Token_Location.

   procedure Restore_Lexer
     (State : Location) renames
     Ocarina.Files.Restore_Location;

   procedure Save_Lexer
     (State : out Location) renames
     Ocarina.Files.Save_Location;

   procedure Skip_Declaration (Delimiter : Token_Type);
   --  Skip until we find a potential end of declaration. Delimiter
   --  indicates the kind of delimiters we are looking for (';', ',',
   --  ':') or ('{', '(', '[') or ('}', ')', ']'). We ensure that the
   --  declaration is well embraced.

   --  Various literal values updated when the corresponding token is read

   Boolean_Literal_Value : Boolean;
   Integer_Literal_Value : Long_Long_Integer;
   Integer_Literal_Base  : Integer;
   Float_Literal_Value   : Long_Long_Float;
   String_Literal_Value  : Name_Id;

end Ocarina.FE_REAL.Lexer;
