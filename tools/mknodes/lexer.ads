------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                L E X E R                                 --
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

with GNAT.OS_Lib;

with Locations;
with Types;

package Lexer is
   pragma Elaborate_Body (Lexer);

   type Token_Type is
     (T_Error,
      T_Identifier,

   --  About basic keywords

      T_Interface,               --  First keyword
      T_Module,
      T_Typedef,

   --  About attributes

      T_Attribute,

   --  Basic types

      T_Short,
      T_Long,
      T_Char,
      T_Boolean,
      T_Octet,                   --  Last keyword

   --  Graphic characters

      T_Colon,
      T_Comma,
      T_Semi_Colon,
      T_Left_Brace,
      T_Left_Bracket,
      T_Left_Paren,
      T_Right_Brace,
      T_Right_Bracket,
      T_Right_Paren,

      T_Colon_Colon,

      T_EOF);

   First_Token_Pos : constant := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos  : constant := Token_Type'Pos (Token_Type'Last);

   type Token_List_Type is array (Positive range <>) of Token_Type;

   subtype Keyword_Type is Token_Type range T_Interface .. T_Octet;

   Token          : Token_Type;
   Token_Name     : Types.Name_Id;
   Token_Location : Locations.Location;

   function Image (T : Token_Type) return String;
   --  Return an image of token T.

   procedure Scan_Token;
   --  Scan token and update global variables Token, Token_Name
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

   function Next_Token return Token_Type;
   --  Return next token but do not update the lexer state that is
   --  Token, Token_Name and Token_Location.

   procedure Save_Lexer (State : out Locations.Location);
   --  Saves the current location in the State variable

   procedure Restore_Lexer (State : Locations.Location);
   --  Modifies the current location in the IDL specification

   procedure Skip_Declaration (Delimiter : Token_Type);
   --  Skip until we find a potential end of declaration. Delimiter
   --  indicates the kind of delimiters we are looking for (';', ',',
   --  ':') or ('{', '(', '[') or ('}', ')', ']'). We ensure that the
   --  declaration is well embraced.

   procedure Skip_Line;
   --  Skip current line

   procedure Process
     (Source_File : GNAT.OS_Lib.File_Descriptor;
      Source_Name : Types.Name_Id);
   --  Load file Source in the lexer

   procedure Write (T : Token_Type);

private

   function Quoted_Image (T : Token_Type) return String;
   --  Return an image of token T. Keywords are output between double
   --  quotes and characters between single quotes.

   procedure Load_File (Source_File : GNAT.OS_Lib.File_Descriptor);
   --  Loads a file in the buffer and then closes it.

   procedure Scan_Identifier (Fatal : Boolean);
   --
   --  Names : 3.2.3
   --  An identifier is an arbitrarily long sequence of ASCII
   --  alphabetic, digit and underscore characters.  The first
   --  character must be an ASCII alphabetic character. All characters
   --  are significant.
   --
   --  Keywords : 3.2.4
   --  keywords must be written exactly as in the above
   --  list. Names that collide with keywords (...) are
   --  illegal. For example, "boolean" is a valid keyword, "Boolean"
   --  and "BOOLEAN" are illegal identifiers.

   procedure Scan_Token (Fatal : Boolean);
   --  Scan token but do not report any error and do not fail on minor
   --  errors like detecting a string which appears to be a wide string.

   procedure New_Token (Token : Token_Type; Image : String);
   --  Evaluate token image and store it in Token_Image table. When
   --  Token is a graphical character, embrace its image between
   --  single quotes ('<<' and '>>' are considered as graphical
   --  characters). When Token is a keyword, embrace its image between
   --  double quotes. Enter the lower-case form of a keyword image
   --  into the name table and set name table byte to its token
   --  position in order to resolve it easily.

   procedure New_Line;
   --  Increment the line number and save the current position in the
   --  buffer in order to compute later on the column number.

   procedure Skip_Spaces;
   --  Skip all spaces

   function To_Token (Name : Types.Name_Id) return Token_Type;
   --  Return the token matching Name. Otherwise, return T_Error.

end Lexer;
