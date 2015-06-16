------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . F E _ R E A L . L E X E R                 --
--                                                                          --
--                                 B o d y                                  --
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

with Charset; use Charset;
with Errors;  use Errors;
with Ocarina.Namet;   use Ocarina.Namet;

package body Ocarina.FE_REAL.Lexer is

   use Ocarina.Files;
   use ASCII;

   procedure Scan_Chars_Literal_Value
     (Delimiter : Character := '"'; --  "
      Adjacent  : Boolean   := True);
   --
   --  Char Literals : (3.2.5.2)
   --  A character literal is one or more characters enclosed in
   --  single quotes, as in 'x'. Nongraphic characters must be
   --  represented using escape sequences as defined in Table
   --  3-9. (escape sequences are \n, \t, \v, \b, \r, \f, \a, \\, \?,
   --  \', \", \ooo, \xhh and \uhhhh)
   --
   --  The escape \ooo consists of the backslash followed by one, two
   --  or three octal digits that are taken to specify the value of
   --  the desired character. The escape \xhh consists of the
   --  backslash followed by x followed by one or two hexadecimal
   --  digits that are taken to specify the value of the desired
   --  character.
   --
   --  The escape \uhhhh consist of a backslash followed by the
   --  character 'u', followed by one, two, three or four hexadecimal
   --  digits.
   --
   --  String Literals : (3.2.5.1)
   --  A string literal is a sequence of characters (...) surrounded
   --  by double quotes, as in "...".
   --
   --  Adjacent string literals are concatenated. (...) Within a
   --  string, the double quote character " must be preceded by a \.
   --  A string literal may not contain the character '\0'.
   --
   --  Wide is used to say if the scanner should scan a wide string or
   --  not. If not and a character looks like '/u...' then an error is
   --  raised

   procedure Scan_String_Literal_Value (Adjacent : Boolean := False);
   procedure Scan_Character_Literal_Value;

   procedure Scan_Numeric_Literal_Value;
   --
   --  Integers Literals : (3.2.5.1)
   --  An integer literal consisting of a sequence of digits is taken
   --  to be decimal (base ten), unless it begins with 0 (digit zero).
   --  A sequence of digits starting with 0 is taken to be an octal
   --  integer (base eight).  The digits 8 and 9 are not octal digits.
   --  A sequence of digits preceded by 0x or 0X is taken to be a
   --  hexadecimal integer (base sixteen).  The hexadecimal digits
   --  include a or A through f or F with decimal values ten to
   --  through fifteen, repectively. For example, the number twelve
   --  can be written 12, 014 or 0XC
   --
   --  Floating-point literals : (3.2.5.3)
   --  A floating-point literal consists of an integer part, a decimal
   --  point, a fraction part, an e or E, and an optionnaly signed
   --  integer exponent.  The integer and fraction parts both consists
   --  of a sequence of decimal (base ten) digits. Either the integer
   --  part or the fraction part (but not both may be missing; either
   --  the decimal point or the letter e (or E) and the exponent (but
   --  not both) may be missing.
   --
   --  Fixed-point literals : (3.2.5.5)
   --  A fixed-point decimal literal consists of an integer part, a
   --  decimal point, a fraction part and a d or D. The integer and
   --  fraction part both consist of a sequence of decimal (base ten)
   --  digits. Either the integer part or the fraction part (but not
   --  both) may be missing; the decimal point (but not the letter d
   --  (or D)) may be missing

   procedure Scan_Integer_Literal_Value
     (Base : Integer;
      Size : Natural := Natural'Last);
   --  Scan an integer literal in Base with a maximum of Size
   --  digits. The result is stored in Integer_Literal_Value and Token
   --  is set to T_Integer_Literal. When the procedure cannot read any
   --  digit or when a digit is greater than the base, Token is set to
   --  T_Error and Integer_Literal_Value is set to 0.

   procedure Scan_Identifier;
   --
   --  Names : 3.2.3
   --  An identifier is an arbritrarily long sequence of ASCII
   --  alphabetic, digit and underscore characters.  The first
   --  character must be an ASCII alphabetic character. All characters
   --  are significant.
   --
   --  Keywords : 3.2.4
   --  keywords must be written exactly as in the above
   --  list. Names that collide with keywords (...) are
   --  illegal. For example, "boolean" is a valid keyword, "Boolean"
   --  and "BOOLEAN" are illegal identifiers.

   procedure Store_Encoded_Character (C : Natural);
   --  Use the brackets notation, where a wide character is
   --  represented by the sequence ["xx"] or ["xxxx"] where xx are
   --  hexadecimal characters to store C.

   function To_Token (Name : Name_Id) return Token_Type;
   --  Return the token matching Name. Otherwise, return T_Error.

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token return Token_Type is
      Current_Token_Name     : Name_Id;
      Current_Token          : Token_Type;
      Current_Token_Location : Location;
      Next_Token_Value       : Token_Type;
   begin
      Current_Token_Name     := Token_Name;
      Current_Token          := Token;
      Current_Token_Location := Token_Location;
      Scan_Token;
      Next_Token_Value := Token;
      Token_Name       := Current_Token_Name;
      Token            := Current_Token;
      Token_Location   := Current_Token_Location;
      return Next_Token_Value;
   end Next_Token;

   ----------------------------------
   -- Scan_Character_Literal_Value --
   ----------------------------------

   procedure Scan_Character_Literal_Value is
   begin
      Scan_Chars_Literal_Value (''', False);
   end Scan_Character_Literal_Value;

   ------------------------------
   -- Scan_Chars_Literal_Value --
   ------------------------------

   procedure Scan_Chars_Literal_Value
     (Delimiter : Character := '"'; --  "
      Adjacent  : Boolean   := True)
   is
      C      : Character;
      Length : Natural := 0;
   begin
      if Delimiter = ''' then
         Token := T_Character_Literal;
      elsif Delimiter = '"' then --  "
         Token := T_String_Literal;
      end if;

      Name_Len := 0;

      --  Skip delimiter

      Token_Location.Scan := Token_Location.Scan + 1;
      loop
         C                   := Buffer (Token_Location.Scan);
         Token_Location.Scan := Token_Location.Scan + 1;

         if C = EOF then
            Token := T_Error;
            return;
         end if;

         --  Exit when (C = ''') or (C = '"' and not Adjacent)

         if C = Delimiter then
            exit when not Adjacent;

            --  Look for adjacent strings

            Skip_Spaces;
            exit when Buffer (Token_Location.Scan) /= Delimiter;

            C                   := Buffer (Token_Location.Scan + 1);
            Token_Location.Scan := Token_Location.Scan + 2;
         end if;

         --  Output only once error message for character literal of
         --  more than one character.

         if Delimiter = ''' and then Length = 1 then
            Error_Loc (1) := Token_Location;
            DE ("strings are delimited by double quote character");
         end if;

         Length := Length + 1;

         --  Read escaped character

         if C = '\' then
            case Buffer (Token_Location.Scan) is
               when 'n' |
                 't'    |
                 'v'    |
                 'b'    |
                 'r'    |
                 '"'    | -- "
                 'f'    |
                 'a'    |
                 '\'    |
                 '''    |
                 '?'    =>
                  Add_Char_To_Name_Buffer (Buffer (Token_Location.Scan));
                  Token_Location.Scan := Token_Location.Scan + 1;

               --  Read 1, 2 or 3 octal digits

               when '0' .. '7' =>

                  Scan_Integer_Literal_Value (8, 3);
                  if Token = T_Error then
                     Error_Loc (1) := Token_Location;
                     DE ("cannot parse octal digits");
                     return;
                  end if;
                  Store_Encoded_Character (Natural (Integer_Literal_Value));

               --  Read 1 or 2 hexadecimal digits

               when 'x' =>
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Scan_Integer_Literal_Value (16, 2);
                  if Token = T_Error then
                     Error_Loc (1) := Token_Location;
                     DE ("cannot parse hexadecimal digits");
                     return;
                  end if;
                  Store_Encoded_Character (Natural (Integer_Literal_Value));

               --  Read 1, 2, 3 or 4 hexadecimal digits

               when 'u' =>
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Scan_Integer_Literal_Value (16, 4);
                  if Token = T_Error then
                     Error_Loc (1) := Token_Location;
                     DE ("cannot parse hexadecimal digits");
                     return;
                  end if;
                  if Token = T_String_Literal then
                     Token := T_Wide_String_Literal;
                  elsif Token = T_Character_Literal then
                     Token := T_Wide_Character_Literal;
                  end if;
                  Store_Encoded_Character (Natural (Integer_Literal_Value));

               when others =>
                  Error_Loc (1) := Token_Location;
                  DE ("unexcepted escaped character");
            end case;

         else
            Add_Char_To_Name_Buffer (C);
         end if;
      end loop;

      String_Literal_Value := Name_Find;
      Token_Name           := String_Literal_Value;
   end Scan_Chars_Literal_Value;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   procedure Scan_Identifier is
      Escaped : Boolean := False;
   begin

      --  Read escaped identifier

      if Buffer (Token_Location.Scan) = '_' then
         Escaped             := True;
         Token_Location.Scan := Token_Location.Scan + 1;
      end if;

      --  Read identifier

      Name_Len := 0;
      while Is_Identifier_Character (Buffer (Token_Location.Scan)) loop
         Name_Len               := Name_Len + 1;
         Name_Buffer (Name_Len) := Buffer (Token_Location.Scan);
         Token_Location.Scan    := Token_Location.Scan + 1;
      end loop;

      Token_Name := Name_Find;
      Token      := T_Identifier;

      --  Check whether it is a keyword

      if not Escaped then
         Token := To_Token (Token_Name);
         if Token = T_Error then
            Token := T_Identifier;
         elsif Token = T_True then
            Token                 := T_Boolean_Literal;
            Boolean_Literal_Value := True;
         elsif Token = T_False then
            Token                 := T_Boolean_Literal;
            Boolean_Literal_Value := False;
         end if;
      end if;
   end Scan_Identifier;

   --------------------------------
   -- Scan_Integer_Literal_Value --
   --------------------------------

   procedure Scan_Integer_Literal_Value
     (Base : Integer;
      Size : Natural := Natural'Last)
   is
      C      : Character;
      Length : Integer := 0;
      Digit  : Integer;
   begin
      Integer_Literal_Value := 0;
      Token                 := T_Integer_Literal;

      while Length < Size loop
         C := To_Lower (Buffer (Token_Location.Scan));
         if C in '0' .. '9' then
            Digit := Character'Pos (C) - Character'Pos ('0');

         elsif Base = 16 and then C in 'a' .. 'f' then
            Digit := Character'Pos (C) - Character'Pos ('a') + 10;

         else
            exit;
         end if;

         if Digit >= Base then
            Error_Loc (1) := Token_Location;
            DE ("digit >= base");
            Token                 := T_Error;
            Integer_Literal_Value := 0;
            return;
         end if;

         Length              := Length + 1;
         Token_Location.Scan := Token_Location.Scan + 1;

         Integer_Literal_Value :=
           Integer_Literal_Value * Long_Long_Integer (Base) +
           Long_Long_Integer (Digit);
      end loop;

      if Length = 0 then
         Integer_Literal_Value := 0;
         Token                 := T_Error;
      end if;
   end Scan_Integer_Literal_Value;

   --------------------------------
   -- Scan_Numeric_Literal_Value --
   --------------------------------

   procedure Scan_Numeric_Literal_Value is
      C             : Character;
      Integer_Part  : Long_Long_Integer := -1;
      Fraction_Part : Long_Long_Integer := -1;
      Exponent_Part : Long_Long_Integer := -1;
      Exponent_Sign : Long_Long_Integer := 1;
   begin
      Token                := T_Error;
      Integer_Literal_Base := 10;

      --  Specific case to get the base

      if Buffer (Token_Location.Scan) = '0' then
         C := To_Lower (Buffer (Token_Location.Scan + 1));

         --  Base is 16

         if C = 'x' then
            Integer_Literal_Base := 16;
            Token_Location.Scan  := Token_Location.Scan + 2;

            C := To_Lower (Buffer (Token_Location.Scan));
            if C not in '0' .. '9' and then C not in 'a' .. 'f' then
               Error_Loc (1) := Token_Location;
               DE ("digit excepted");
               return;
            end if;

         --  Base is 8

         elsif C in '0' .. '9' then
            Integer_Literal_Base := 8;
            Token_Location.Scan  := Token_Location.Scan + 1;

         end if;
      end if;

      --  Read the integer part. If Base is not 10, we are sure that
      --  the current character is a digit in Base.

      if Buffer (Token_Location.Scan) /= '.' then
         Scan_Integer_Literal_Value (Integer_Literal_Base);
         if Token = T_Error then
            return;
         end if;
         Integer_Part := Integer_Literal_Value;

         if Integer_Literal_Base /= 10 then
            Token := T_Integer_Literal;
            return;
         end if;
      end if;

      C := To_Lower (Buffer (Token_Location.Scan));

      --  Read the fraction part.

      if C = '.' then
         Token_Location.Scan := Token_Location.Scan + 1;
         if Buffer (Token_Location.Scan) in '0' .. '9' then
            Scan_Integer_Literal_Value (10);
            Fraction_Part := Integer_Literal_Value;
         end if;

         --  Update C to get either the exponent or the fixed suffix

         if To_Lower (Buffer (Token_Location.Scan)) in 'd' .. 'e' then
            C := To_Lower (Buffer (Token_Location.Scan));
         end if;
      end if;

      --  Read the exponent.

      if C = 'e' then
         Token_Location.Scan := Token_Location.Scan + 1;

         --  Read the exponent sign.

         if Buffer (Token_Location.Scan) = '-' then
            Exponent_Sign       := -1;
            Token_Location.Scan := Token_Location.Scan + 1;
         elsif Buffer (Token_Location.Scan) = '+' then
            Exponent_Sign       := 1;
            Token_Location.Scan := Token_Location.Scan + 1;
         end if;

         --  Read the exponent part.

         if Buffer (Token_Location.Scan) in '0' .. '9' then
            Scan_Integer_Literal_Value (10);
            Exponent_Part := Integer_Literal_Value;
         end if;

      --  Skip fixed literal suffix

      elsif C = 'd' then
         Token_Location.Scan := Token_Location.Scan + 1;
      end if;

      if Integer_Part = -1 and then Fraction_Part = -1 then
         Error_Loc (1) := Token_Location;
         DE ("both integer and fraction part cannot be missing");
         return;
      end if;

      if C = 'e' and then Exponent_Part = -1 then
         Error_Loc (1) := Token_Location;
         DE ("exponent part cannot be missing");
         return;
      end if;

      --  Find a fixed point literal

      if C = 'd' then
         Token := T_Fixed_Point_Literal;

      --  Find a floating point literal

      elsif C = 'e' or else C = '.' or else Fraction_Part /= -1 then
         Token := T_Floating_Point_Literal;

      --  Find an integer literal

      else
         Token := T_Integer_Literal;
      end if;

      if Integer_Part = -1 then
         Integer_Part := 0;
      end if;

      if Fraction_Part = -1 then
         Fraction_Part := 0;
      end if;

      if Exponent_Part = -1 then
         Exponent_Part := 0;
      end if;

      Set_Dnat_To_Name_Buffer (Dnat (Integer_Part));

      if Token /= T_Integer_Literal then
         Add_Char_To_Name_Buffer ('.');
         Add_Dnat_To_Name_Buffer (Dnat (Fraction_Part));

         if Token = T_Floating_Point_Literal then
            Add_Char_To_Name_Buffer ('E');
            if Exponent_Sign = -1 then
               Add_Char_To_Name_Buffer ('-');
            end if;
            Add_Dnat_To_Name_Buffer (Dnat (Exponent_Part));
         end if;

         Float_Literal_Value :=
           Long_Long_Float'Value (Name_Buffer (1 .. Name_Len));

         if Token = T_Fixed_Point_Literal then
            Add_Char_To_Name_Buffer ('D');
         end if;
      end if;

      Token_Name := Name_Find;
   end Scan_Numeric_Literal_Value;

   -------------------------------
   -- Scan_String_Literal_Value --
   -------------------------------

   procedure Scan_String_Literal_Value (Adjacent : Boolean := False) is
   begin
      Scan_Chars_Literal_Value ('"', Adjacent); -- "
   end Scan_String_Literal_Value;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (T : Token_Type) is
      Loc : Location := Token_Location;
   begin
      Scan_Token;

      if T /= Token then
         if T = T_Semi_Colon then
            Loc.Last_Pos := Loc.Scan;
            if Buffer (Loc.Last_Pos) = LF
              or else Buffer (Loc.Last_Pos) = FF
              or else Buffer (Loc.Last_Pos) = CR
              or else Buffer (Loc.Last_Pos) = VT
            then
               Loc.Last_Pos := Loc.Last_Pos - 1;
            end if;
         else
            Loc := Token_Location;
         end if;
         Token := T_Error;
      end if;
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token is
   begin
      Token := T_Error;
      while Token = T_Error loop
         Skip_Spaces;
         Token_Location.Last_Pos := Token_Location.Scan;

         if Token_Location.Scan > Token_Location.EOF or else Token = T_EOF then
            Token := T_EOF;
            return;
         end if;

         case Buffer (Token_Location.Scan) is
            when LF | FF | CR | VT =>
               New_Line;

            when ';' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Semi_Colon;

            when '{' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Left_Brace;

            when '}' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Right_Brace;

            when ':' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token               := T_Affect;
               else
                  Token := T_Colon;
               end if;

            when ',' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Comma;

            when '(' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Left_Paren;

            when ')' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Right_Paren;

            when '=' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Equal;

            when '<' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token               := T_Less_Equal;
               elsif Buffer (Token_Location.Scan) = '>' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token               := T_Different;
               else
                  Token := T_Less;
               end if;

            when '>' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token               := T_Greater_Equal;
               else
                  Token := T_Greater;
               end if;

            when '+' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Plus;

            when '-' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '-' then
                  Skip_Line;
               else
                  Token := T_Minus;
               end if;

            when '/' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Slash;

            when '*' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '*' then
                  if Buffer (Token_Location.Scan + 1) = '}' then
                     Token_Location.Scan := Token_Location.Scan + 2;
                     Token               := T_EOF;
                  else
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token               := T_Power;
                  end if;
               else
                  Token := T_Star;
               end if;

            when '[' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Left_Bracket;

            when ']' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Right_Bracket;

            when '0' .. '9' =>
               Scan_Numeric_Literal_Value;

            when '.' =>
               Scan_Numeric_Literal_Value;

            when ''' =>
               Scan_Character_Literal_Value;

            when '"' => -- "
               Scan_String_Literal_Value (True);

            when '|' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token               := T_Sothat;

            when '_' =>
               if Is_Alphabetic_Character
                   (Buffer (Token_Location.Scan + 1))
               then
                  Scan_Identifier;
               else
                  Error_Loc (1) := Token_Location;
                  DE ("incorrect escaped identifier");
               end if;

            when EOF =>
               Token_Location.Scan := Token_Location.Scan + 3;
               Token               := T_EOF;

            when others =>
               if Is_Alphabetic_Character (Buffer (Token_Location.Scan)) then

                  --
                  --  Wide Chars : 3.5.2.2
                  --  Wide characters litterals have an L prefix, for example :
                  --      const wchar C1 = L'X';
                  --
                  --  Wide Strings : 3.5.2.4
                  --  Wide string literals have an L prefix, for example :
                  --      const wstring S1 = L"Hello";

                  if Buffer (Token_Location.Scan) = 'L' then

                     --  Read wide character literal

                     if Buffer (Token_Location.Scan + 1) = ''' then
                        Token_Location.Scan := Token_Location.Scan + 1;
                        Scan_Character_Literal_Value;
                        if Token = T_Character_Literal then
                           Token := T_Wide_Character_Literal;
                        end if;
                        return;

                     --  Read wide string literal

                     elsif Buffer (Token_Location.Scan + 1) = '"' then --  "
                        Token_Location.Scan := Token_Location.Scan + 1;
                        Scan_String_Literal_Value (True);
                        if Token = T_String_Literal then
                           Token := T_Wide_String_Literal;
                        end if;
                        return;
                     end if;
                  end if;

                  Scan_Identifier;

               else
                  Error_Loc (1) := Token_Location;
                  DE ("invalid character");
               end if;
         end case;
      end loop;

      --  W_Line (Image (Token));

   end Scan_Token;

   ----------------------
   -- Skip_Declaration --
   ----------------------

   procedure Skip_Declaration (Delimiter : Token_Type) is
      Braces : Integer := 0;
      State  : Location;
   begin
      loop
         Save_Lexer (State);
         Scan_Token;

         exit when Token = T_EOF;

         if Token in T_Left_Brace .. T_Left_Paren then
            Braces := Braces + 1;

         elsif Token in T_Right_Brace .. T_Right_Paren then
            exit when Braces <= 0
              and then Delimiter in T_Right_Brace .. T_Right_Paren;
            Braces := Braces - 1;

         elsif Token in T_Colon .. T_Semi_Colon then
            exit when Braces <= 0
              and then Delimiter in T_Colon .. T_Semi_Colon;
         end if;
      end loop;

      --  When we reach the end of the file without finding a proper
      --  delimiter, we cannot rescue the lexer.

      if Token /= T_EOF then
         Restore_Lexer (State);
         Scan_Token (Delimiter);
      end if;
   end Skip_Declaration;

   -----------------------------
   -- Store_Encoded_Character --
   -----------------------------

   procedure Store_Encoded_Character (C : Natural) is

      procedure Set_Hex_Chars (N : Natural);
      --  Stores given value, which is in the range 0 .. 255, as two hex
      --  digits (using lower case a-f) in Name_Buffer, incrementing Name_Len

      procedure Set_Hex_Chars (N : Natural) is
         Hexd : constant String := "0123456789abcdef";

      begin
         Add_Char_To_Name_Buffer (Hexd (N / 16 + 1));
         Add_Char_To_Name_Buffer (Hexd (N mod 16 + 1));
      end Set_Hex_Chars;

   begin
      if C < 256 then
         Add_Char_To_Name_Buffer (Character'Val (C));

      else
         Add_Str_To_Name_Buffer ("[""");
         Set_Hex_Chars (C / 256);
         Set_Hex_Chars (C mod 256);
         Add_Str_To_Name_Buffer ("]""");
      end if;
   end Store_Encoded_Character;

   --------------
   -- To_Token --
   --------------

   function To_Token (Name : Name_Id) return Token_Type is
      pragma Unreferenced (Name);

      N : Name_Id;
      B : Byte;
   begin
      N := Get_String_Name (Prefix & To_Lower (Name_Buffer (1 .. Name_Len)));
      B := Get_Name_Table_Byte (N);
      if B <= Last_Token_Pos then
         return Token_Type'Val (B);
      end if;
      return T_Error;
   end To_Token;

end Ocarina.FE_REAL.Lexer;
