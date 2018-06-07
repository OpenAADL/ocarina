------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . F E _ A O 4 A A D L . L E X E R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2016-2018 ESA & ISAE.                    --
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

with Ada.Characters.Handling;

with Ocarina.Types;  use Ocarina.Types;
with Charset;        use Charset;
with Errors;         use Errors;
with Ocarina.Namet;  use Ocarina.Namet;

with Ocarina.AO4AADL_Values;

package body Ocarina.FE_AO4AADL.Lexer is

   use Ocarina.Files;
   use ASCII;

   Display_Name_Buffer : String (1 .. 1024);
   --  This buffer is used to store temporarily an AADL identifier

   Display_Name_Len    : Natural;
   --  Length of name stored in AADL_Name_Buffer

   Max_Integer_Value : constant Unsigned_Long_Long := 2 ** 63 - 1;
   --  Largest value the parser is willing to handle

   procedure Scan_String_Literal_Value;
   --
   --  String Literals:
   --
   --  A string_literal is formed by a sequence of graphic characters
   --  (possibly none) enclosed between two quotation marks used as
   --  string brackets.
   --  Syntax:
   --     string_lateral ::= "{string_element}"
   --     string_element ::= "" | non_quotation_mark_graphic_character

   procedure Scan_Based_Fraction_Value (Base : Unsigned_Short_Short);
   --  Scan a fraction in the given base notation.  This procedure
   --  checks digits for validity.

   procedure Scan_Based_Integer_Value (Base : Unsigned_Short_Short);
   --  Scan an integer in the given base notation. This procedure
   --  checks digits for validity.  Example: 1234 is an invalid number
   --  in base 4.

   procedure Scan_Decimal_Fraction_Value;
   --  Scan a fraction in the conventional decimal notation.

   procedure Scan_Decimal_Integer_Value;
   --  Scan an integer in the conventional decimal notation.
   --  This procedure is used in Scan_Numeric_Literal_Value because of three
   --  reasons:
   --     1. all numeric literal begins with a decimal integer literal
   --     2. exponent value is always in decimal format
   --     3. Scan_Based_Integer_Value (Base) checks digits for validity
   --        which is more complex (so less efficient)

   procedure Scan_Numeric_Literal_Value;
   --
   --  Numeric Literals:
   --
   --  There are two kinds of numeric literals, real literals and
   --  integer literals. A real literal is a numeric literal that
   --  includes a point; an integer literal is a numeric literal
   --  without a point.

   procedure Scan_Identifier;
   --  An identifier is an arbritrarily long sequence of ASCII
   --  alphabetic, digit and underscore characters.  The first
   --  character must be an ASCII alphabetic character. All characters
   --  are significant.
   --
   --  keywords must be written exactly as in the above
   --  list. Names that collide with keywords (...) are
   --  illegal. For example, "boolean" is a valid keyword, "Boolean"
   --  and "BOOLEAN" are illegal identifiers.

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
      Next_Token_Value       := Token;
      Token_Name             := Current_Token_Name;
      Token                  := Current_Token;
      Token_Location         := Current_Token_Location;
      return Next_Token_Value;
   end Next_Token;

   -------------------------------
   -- Scan_Based_Fraction_Value --
   -------------------------------

   procedure Scan_Based_Fraction_Value (Base : Unsigned_Short_Short) is
      Ch     : Character;
      Size   : Integer := 0;  --  number of scanned digits
      Digit  : Unsigned_Short_Short;
      Factor : Long_Long_Float;

   begin
      Real_Literal_Value := 0.0;
      Factor             := 1.0 / Long_Long_Float (Base);
      Token              := T_Real_Literal;

      loop
         Ch := Ada.Characters.Handling.To_Upper (Buffer (Token_Location.Scan));

         if Ch in '0' .. '9' then
            Digit := Character'Pos (Ch) - Character'Pos ('0');
         elsif Ch in 'A' .. 'F' then
            Digit := Character'Pos (Ch) - Character'Pos ('A') + 10;
         elsif Ch = '_' then
            Token_Location.Scan := Token_Location.Scan + 1;
         else
            exit;
         end if;

         if Ch /= '_' then
            if Digit >= Base then
               Error_Loc (1) := Token_Location;
               DE ("digit '|" & Ch & "' is invalid in base " &
                   Unsigned_Short_Short'Image (Base));
               Token := T_Error;
               Real_Literal_Value := 0.0;
               return;
            end if;

            Size := Size + 1;
            Token_Location.Scan := Token_Location.Scan + 1;

            Real_Literal_Value :=
              Real_Literal_Value + Long_Long_Float (Digit) * Factor;
            Factor := Factor / Long_Long_Float (Base);
         end if;
      end loop;

      if Size = 0 then
         Error_Loc (1) := Token_Location;
         DE ("invalid digit '|" & Ch & "'");
         Token := T_Error;
      end if;
   end Scan_Based_Fraction_Value;

   ------------------------------
   -- Scan_Based_Integer_Value --
   ------------------------------

   procedure Scan_Based_Integer_Value (Base : Unsigned_Short_Short) is
      Ch    : Character;
      Size  : Integer := 0;          --  number of scanned digits
      Digit : Unsigned_Short_Short;

   begin
      Integer_Literal_Value := 0;
      Token := T_Integer_Literal;

      loop
         Ch := Ada.Characters.Handling.To_Upper (Buffer (Token_Location.Scan));
         if Ch in '0' .. '9' then
            Digit := Character'Pos (Ch) - Character'Pos ('0');
         elsif Ch in 'A' .. 'F' then
            Digit := Character'Pos (Ch) - Character'Pos ('A') + 10;
         elsif Ch = '_' then
            Token_Location.Scan := Token_Location.Scan + 1;
         else
            exit;
         end if;

         if Ch /= '_' then
            if Digit >= Base then
               Error_Loc (1) := Token_Location;
               DE ("digit '|" & Ch & "' is invalid in base " &
                   Unsigned_Short_Short'Image (Base));
               Token := T_Error;
               Integer_Literal_Value := 0;
               return;
            end if;

            Size := Size + 1;
            Token_Location.Scan := Token_Location.Scan + 1;

            Integer_Literal_Value :=
              Integer_Literal_Value * Unsigned_Long_Long (Base) +
              Unsigned_Long_Long (Digit);
         end if;
      end loop;

      if Size = 0 then
         Error_Loc (1) := Token_Location;
         DE ("invalid digit '|" & Ch & "'");
         Token := T_Error;
      end if;
   end Scan_Based_Integer_Value;

   ---------------------------------
   -- Scan_Decimal_Fraction_Value --
   ---------------------------------

   procedure Scan_Decimal_Fraction_Value is
      Ch     : Character;
      Size   : Integer := 0;  --  number of scanned digits
      Digit  : Unsigned_Short_Short;
      Factor : Long_Long_Float;

   begin
      Real_Literal_Value := 0.0;
      Factor := 0.1;
      Token := T_Real_Literal;

      loop
         Ch := Buffer (Token_Location.Scan);

         if Ch in '0' .. '9' then
            Digit := Character'Pos (Ch) - Character'Pos ('0');
            Real_Literal_Value := Real_Literal_Value +
              Long_Long_Float (Digit) *  Factor;
            Factor := Factor / 10.0;
            Size   := Size + 1;
            Token_Location.Scan := Token_Location.Scan + 1;
         elsif Ch = '_' then
            Token_Location.Scan := Token_Location.Scan + 1;  --  '_' ignored
         else
            exit;
         end if;
      end loop;

      if Size = 0 then
         Error_Loc (1) := Token_Location;
         DE ("invalid decimal digit '|" & Ch & "'");
         Token := T_Error;
      end if;
   end Scan_Decimal_Fraction_Value;

   --------------------------------
   -- Scan_Decimal_Integer_Value --
   --------------------------------

   procedure Scan_Decimal_Integer_Value is
      Ch   : Character;
      Size : Integer := 0;  --  number of scanned digits
   begin
      Integer_Literal_Value := 0;
      Token := T_Integer_Literal;

      loop
         Ch := Buffer (Token_Location.Scan);
         if Ch in '0' .. '9' then
            if Size >= Max_Number_Of_Digits then
               --  we remove all remaining digits from the buffer
               loop
                  if Ch in '0' .. '9' or else Ch = '_' then
                     Ch := Buffer (Token_Location.Scan);
                     Token_Location.Scan := Token_Location.Scan + 1;
                  else
                     exit;
                  end if;
               end loop;
               --  we send error log
               Error_Loc (1) := Token_Location;
               DW ("too long number, digit ignored");
               return;
            else
               Integer_Literal_Value := Integer_Literal_Value * 10 +
                 Character'Pos (Ch) - Character'Pos ('0');
               Size := Size + 1;
            end if;
            Token_Location.Scan := Token_Location.Scan + 1;
         elsif Ch = '_' then
            Token_Location.Scan := Token_Location.Scan + 1;  --  '_' ignored
         else
            exit;
         end if;
      end loop;

      if Size = 0 then
         Error_Loc (1) := Token_Location;
         DE ("invalid decimal digit '|" & Ch & "'");
         Token := T_Error;
      end if;
   end Scan_Decimal_Integer_Value;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   procedure Scan_Identifier is
      B             : Byte;
      Is_Identifier : Boolean := False;
   begin
      --  The first character of identifier is an alphabetic
      --  character.  Buffer (Token_Location.Scan) is tested in
      --  Scan_Token before procedure call.

      Name_Len := 0;   --  initialize string buffer
      Add_Char_To_Name_Buffer (To_Lower (Buffer (Token_Location.Scan)));

      Display_Name_Len := 1;
      Display_Name_Buffer (Display_Name_Len) := Buffer (Token_Location.Scan);

      Token_Location.Scan := Token_Location.Scan + 1;

      while Is_Identifier_Character (Buffer (Token_Location.Scan)) loop
         Add_Char_To_Name_Buffer (To_Lower (Buffer (Token_Location.Scan)));

         Display_Name_Len := Display_Name_Len + 1;
         Display_Name_Buffer (Display_Name_Len) :=
           Buffer (Token_Location.Scan);

         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;

      --  check whether it is a reserved word

      Token_Name := Name_Find;
      Add_Str_To_Name_Buffer (Suffix);

      B := Get_Name_Table_Byte (Name_Find);

      if B in First_Reserved_Word_Pos .. Last_Reserved_Word_Pos then
         if Token_Name = Token_Image (Token_Type'Val (B)) then
            Token := Token_Type'Val (B);
         else
            raise Program_Error;
         end if;
      else
         Token := T_Identifier;
         Is_Identifier := True;
      end if;

      if Is_Identifier then
         --  Add identifier display string with case-sensitive
         Name_Len := 0;

         for I in 1 .. Display_Name_Len loop
            Add_Char_To_Name_Buffer (Display_Name_Buffer (I));
         end loop;

         Token_Display_Name := Name_Find;
      end if;
   end Scan_Identifier;

   --------------------------------
   -- Scan_Numeric_Literal_Value --
   --------------------------------

   procedure Scan_Numeric_Literal_Value is
      Ch           : Character;
      Is_Real      : Boolean := False;   --  scanned number is a real number
      Int_Save     : Unsigned_Long_Long; --  temporary value
      Exp_Sign     : Boolean;            --  exponent sign (True - / False +)
      Exp_Sign_Loc : Location;           --  location of exponent sign
      Factor       : Long_Long_Float;    --  base ** exponent
   begin
      Scan_Decimal_Integer_Value;
      Numeric_Literal_Base := 10;  --  by default, base is ten
      Numeric_Literal_Exp := 0;    --  by default, exponent is zero

      if Token /= T_Error then
         Ch := Buffer (Token_Location.Scan);

         if Ch = '#' then --  based number
            Token_Location.Scan := Token_Location.Scan + 1;

            if Integer_Literal_Value < 2 then
               Error_Loc (1) := Token_Location;
               DE ("numeric base "
                     & Ocarina.AO4AADL_Values.Image (Integer_Literal_Value) &
                     " is too small, must be at least 2");
               Token := T_Error;
               return;
            elsif Integer_Literal_Value > 16 then
               Error_Loc (1) := Token_Location;
               DE ("numeric base "
                   & Ocarina.AO4AADL_Values.Image (Integer_Literal_Value) &
                   " is too big, must be at most 16");
               Token := T_Error;
               return;
            else          --  base is OK
               Numeric_Literal_Base :=
                 Unsigned_Short_Short (Integer_Literal_Value);

               --  scan for integer part

               Scan_Based_Integer_Value (Numeric_Literal_Base);

               if Token /= T_Error then
                  Ch := Buffer (Token_Location.Scan);

                  if Ch = '.'
                    and then Buffer (Token_Location.Scan + 1) /= '.'
                  then
                     --  based real number, only if there is a single
                     --  dot. If there are two, then it may be a
                     --  range.

                     Is_Real := True;
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Scan_Based_Fraction_Value (Numeric_Literal_Base);

                     if Token /= T_Error then
                        --  Mix two parts

                        Real_Literal_Value := Real_Literal_Value +
                          Long_Long_Float (Integer_Literal_Value);
                     else
                        return;
                     end if;
                  end if;
               else
                  return;
               end if;

               Ch := Buffer (Token_Location.Scan);

               if Ch /= '#' then
                  Error_Loc (1) := Token_Location;
                  DE ("'|#' is expected at the end of based number");
                  return;
               else
                  Token_Location.Scan := Token_Location.Scan + 1;
               end if;
            end if;

         elsif Ch = '.'
           and then Buffer (Token_Location.Scan + 1) /= '.'
         then   --  decimal real number
            Is_Real := True;
            Token_Location.Scan := Token_Location.Scan + 1;
            Scan_Decimal_Fraction_Value;

            if Token /= T_Error then
               --  Mix two parts

               Real_Literal_Value := Real_Literal_Value +
                 Long_Long_Float (Integer_Literal_Value);
            else
               return;
            end if;
         end if;

         --  Scan exponent

         Ch := Buffer (Token_Location.Scan);

         if Ch = 'E' or else Ch = 'e' then
            Token_Location.Scan := Token_Location.Scan + 1;
            Ch := Buffer (Token_Location.Scan);

            if Ch = '-' then
               Exp_Sign := True;
               Save_Lexer (Exp_Sign_Loc);
               Token_Location.Scan := Token_Location.Scan + 1;
            else
               Exp_Sign := False;
               if Ch = '+' then   --  ignore '+'
                  Token_Location.Scan := Token_Location.Scan + 1;
               end if;
            end if;

            Int_Save := Integer_Literal_Value;  --  save before scan
            Scan_Decimal_Integer_Value;

            if Token /= T_Error then
               if Integer_Literal_Value >
                 Unsigned_Long_Long (Integer'Last)
               then
                  Error_Loc (1) := Token_Location;
                  DW ("exponent too big");
                  return;
               end if;

               if Exp_Sign then
                  Numeric_Literal_Exp := -Integer (Integer_Literal_Value);
               else
                  Numeric_Literal_Exp := Integer (Integer_Literal_Value);
               end if;

               if Numeric_Literal_Exp > 64 then --  FIXME : check max value
                  Error_Loc (1) := Token_Location;
                  DW ("exponent too big");
                  return;
               end if;

               Factor := Ocarina.AO4AADL_Values.Power
                 (Integer (Numeric_Literal_Base), Numeric_Literal_Exp);
               if Is_Real then
                  Real_Literal_Value   := Real_Literal_Value * Factor;
               else
                  Integer_Literal_Value :=
                    Unsigned_Long_Long (Long_Long_Float (Int_Save) * Factor);

                  if Exp_Sign then
                     Error_Loc (1) := Exp_Sign_Loc;
                     DW ("negative exponent in integer literal");
                  end if;
               end if;
            else
               return;
            end if;
         end if;

         if Is_Real then
            Token := T_Real_Literal;
         else
            Token := T_Integer_Literal;
         end if;
      end if;

      if not Is_Real then
         if Integer_Literal_Value > Max_Integer_Value then
            Error_Loc (1) := Token_Location;
            DW ("value too big");
            return;
         end if;
      end if;

   exception
      when Constraint_Error =>
         Error_Loc (1) := Token_Location;
         DW ("value out of range");
   end Scan_Numeric_Literal_Value;

   -------------------------------
   -- Scan_String_Literal_Value --
   -------------------------------

   procedure Scan_String_Literal_Value is
      Quoted : Boolean := False;   --  A quotation mark '"' is scanned
      Ch     : Character;

   begin
      Name_Len := 0;   --  initialize string buffer
      loop
         Ch := Buffer (Token_Location.Scan);

         if Ch = '"' then
            if Quoted then
               Quoted := False;
               Add_Char_To_Name_Buffer ('"');
            else
               Quoted := True;
            end if;
            Token_Location.Scan := Token_Location.Scan + 1;
         else
            if Quoted then
               exit;   --  end of string, DO NOT increment scan position
            else
               if Ada.Characters.Handling.Is_Graphic (Ch) then
                  Add_Char_To_Name_Buffer (Ch);
               else
                  Error_Loc (1) := Token_Location;
                  DE ("non graphic character '|" & Ch &
                      "' is not allowed in string lateral, ignored");

                  if Token_Location.Scan > Token_Location.EOF then
                     DE ("scanning string, end of file reached, exit");
                     exit;
                  end if;
               end if;
               Token_Location.Scan := Token_Location.Scan + 1;
            end if;
         end if;
      end loop;

      Token := T_String_Literal;

      if Name_Len = 0 then
         String_Literal_Value := No_Name;
      else
         String_Literal_Value := Name_Find;
      end if;
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

         if Token_Location.Scan >= Token_Location.EOF or else
           Token = T_EOF
         then
            Token := T_EOF;
            return;
         end if;

         case Buffer (Token_Location.Scan) is
            when LF | FF | CR | VT =>
               New_Line;

            when ';' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Semi_Colon;

            when '{' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Brace;

            when '}' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Brace;

            when ':' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Assignment;
               elsif Buffer (Token_Location.Scan) = ':' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Colon_Colon;
               else
                  Token := T_Colon;
               end if;

            when ',' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Comma;

            when '(' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Paren;

            when ')' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Paren;

            when '=' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Equal;

            when '<' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Less_Or_Equal;
               else
                  Token := T_Less_Than;
               end if;

            when '>' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Greater_Or_Equal;
               else
                  Token := T_Greater_Than;
               end if;

            when '+' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Plus;

            when '-' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '-' then
                  Skip_Line;
               else
                  Token := T_Minus;
               end if;

            when '/' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Divide;

            when '*' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '*' then
                  if Buffer (Token_Location.Scan + 1) = '}' then
                     Token_Location.Scan := Token_Location.Scan + 2;
                     Token := T_EOF;
                  end if;
               else
                  Token := T_Star;
               end if;

            when '[' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Left_Bracket;

            when ']' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Right_Bracket;

            when '0' .. '9' =>
               Scan_Numeric_Literal_Value;

            when '.' =>     --  '.' or '..'
               Token_Location.Scan := Token_Location.Scan + 1;

               if Buffer (Token_Location.Scan) = '.' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Interval;
               else
                  Token := T_Dot;
               end if;

            when ''' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Tick;

            when '"' => -- "
               Token_Location.Scan := Token_Location.Scan + 1;
               Scan_String_Literal_Value;

            when '|' =>
               Token_Location.Scan := Token_Location.Scan + 1;

               if Buffer (Token_Location.Scan) = '|' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Or_Logic;
               else
                  Token := T_Error;
               end if;

            when '&' =>
               Token_Location.Scan := Token_Location.Scan + 1;

               if Buffer (Token_Location.Scan) = '&' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Concat;
               else
                  Token := T_Error;
               end if;

            when '_' =>
               if Is_Alphabetic_Character
                 (Buffer (Token_Location.Scan + 1))
               then
                  Scan_Identifier;
               else
                  Error_Loc (1) := Token_Location;
                  DE ("incorrect escaped identifier");
               end if;

            when '?' =>
               Token_Location.Scan := Token_Location.Scan + 1;
               Token := T_Interrogative;

            when '!' =>      --  '!' or '!='
               Token_Location.Scan := Token_Location.Scan + 1;
               if Buffer (Token_Location.Scan) = '=' then
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token := T_Non_Equal;
               else
                  Token := T_Exclamation;
               end if;

            when EOF =>
               Token_Location.Scan := Token_Location.Scan + 3;
               Token := T_EOF;

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
                        --  Scan_Character_Literal_Value;
                        if Token = T_Character_Literal then
                           Token := T_Character_Literal;
                        end if;
                        return;

                     --  Read wide string literal

                     elsif Buffer (Token_Location.Scan + 1) = '"' then --  "
                        Token_Location.Scan := Token_Location.Scan + 1;
                        Scan_String_Literal_Value;
                        if Token = T_String_Literal then
                           Token := T_String_Literal;
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

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (L : Token_List_Type) is
   begin
      pragma Assert (L'Length > 1);
      Scan_Token;
      for Index in L'Range loop
         if L (Index) = Token then
            return;  --  All is well
         end if;
      end loop;

      --  Give error message

      Name_Len := 0;
      Add_Str_To_Name_Buffer (Quoted_Image (L (L'First)));
      for Index in L'First + 1 .. L'Last loop
         Add_Str_To_Name_Buffer (" or ");
         Add_Str_To_Name_Buffer (Quoted_Image (L (Index)));
      end loop;
      Error_Loc (1) := Token_Location;
      Error_Name (1) := Name_Find;
      DE ("expected tokens %");
      Token := T_Error;
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

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line is
   begin
      loop
         case Buffer (Token_Location.Scan) is
            when LF | FF | CR | VT =>
               New_Line;
               exit;
            when others =>
               null;
         end case;
         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;
   end Skip_Line;

   -------------------------
   -- Current_Token_Image --
   -------------------------

   function Current_Token_Image return String is
   begin
      case Token is
         when T_Identifier =>
            return "identifier '" & Get_Name_String (Token_Display_Name) & "'";

         when T_Quotation_Mark .. T_False =>
            return "token " & Quoted_Image (Token);

         when T_Precedence .. T_Proceed =>
            return "keyword " & Quoted_Image (Token);

         when T_Real_Literal =>
            return "real value ["
              & Ocarina.AO4AADL_Values.Image (Real_Literal_Value,
                                              Numeric_Literal_Base,
                                              Numeric_Literal_Exp)
              & "]";

         when T_Integer_Literal =>
            return "integer value ["
              & Ocarina.AO4AADL_Values.Image (Integer_Literal_Value,
                                              Numeric_Literal_Base,
                                              Numeric_Literal_Exp)
              & "]";

         when T_String_Literal =>
            if String_Literal_Value = No_Name then
               return "null string literal";
            else
               return
                 "string """ & Get_Name_String (String_Literal_Value) & """";
            end if;

         when others =>
            return Image (Token);
      end case;
   end Current_Token_Image;

end Ocarina.FE_AO4AADL.Lexer;
