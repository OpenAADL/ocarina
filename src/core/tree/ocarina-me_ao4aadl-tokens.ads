------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . M E _ A O 4 A A D L . T O K E N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

--  Description of AO4AADL tokens

with Ocarina.Types;
with Locations;
with Ocarina.Files;

package Ocarina.ME_AO4AADL.Tokens is

   use Locations;
   use Ocarina.Types;
   use Ocarina.Files;

   Suffix : constant String := "%ao4aadl%";
   --  This string is used to avoid clashing between several language
   --  keywords when associating them to the corresponding positions
   --  in Token_Type.

   type Token_Type is
     (T_Error,
      T_Identifier,

      --  Special_Characters

      T_Quotation_Mark,        --  "    (for T_String_Literal)
      T_Underline,             --  _

      T_Comma,                 --  ,     First Delimiter

      T_Dot,                   --  .

      T_Colon,                 --  :
      T_Semi_Colon,             --  ;
      T_Colon_Colon,           --  ::
      T_Assignment,            --  :=
      T_Interval,              --  ..

      T_Tick,                  --  '

      T_Left_Paren,            --  (     First Opening Delimiter
      T_Left_Bracket,   --  [
      T_Left_Brace,    --  {

      T_Right_Paren,     --  )     First Closing Delimiter
      T_Right_Bracket,  --  ]
      T_Right_Brace,   --  }

      T_Interrogative,         --  ?
      T_Exclamation,           --  !

      --  Relational operator

      T_Less_Than,             --  <
      T_Equal,                 --  =
      T_Greater_Than,          --  >
      T_Greater_Or_Equal,      --  >=
      T_Less_Or_Equal,         --  <=
      T_Non_Equal,             --  !=

      T_Plus,                  --  +
      T_Minus,                 --  -
      T_Or_Logic,              --  ||
      T_Concat,                --  &&

      T_And,                   --  and
      T_Not,                   --  not
      T_Or,                    --  or

      --  Multiplying operator

      T_Star,                  --  *
      T_Divide,                --  /

      --  Structure

      T_If,                    --  if
      T_Elsif,                 --  elsif
      T_Else,                  --  else
      T_For,                   --  for
      T_While,                 --  while

      --  Boolean

      T_True,                  --  true
      T_False,                 --  false

      --  Reserved Words

      T_Precedence,            --  precedence
      T_Aspect,                --  aspect
      T_Applied,               --  applied
      T_To,                    --  to
      T_Thread,                --  thread
      T_Process,               --  process
      T_Subprogram,            --  subprogram
      T_System,                --  system
      T_Pointcut,              --  pointcut
      T_Call,                  --  call
      T_Execution,             --  execution
      T_Args,                  --  args
      T_Inport,                --  inport
      T_Outport,               --  outport
      T_InOutPort,             --  inoutport
      T_Advice,                --  advice
      T_Before,                --  before
      T_After,                 --  after
      T_Around,                --  around
      T_Variables,              --  variables
      T_Initially,             --  initially
      T_Computation,           --  computation
      T_Count,                 --  count
      T_Delay,                 --  delay
      T_Fresh,                 --  fresh
      T_In,                    --  in
      T_Proceed,               --  proceed

      --  Numeric Literals

      T_Real_Literal,          --  real number
      T_Integer_Literal,       --  integer number
      T_Boolean_Literal,
      T_Character_Literal,

      --  Others

      T_String_Literal,        --  string
      T_Comment,               --  comment      (ignored)
      T_Raw_Text,              --  raw text (used for annex parsing)

      T_EOF                    --  end of file
     );

   First_Token_Pos : constant := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos  : constant := Token_Type'Pos (Token_Type'Last);

   type Token_List_Type is array (Positive range <>) of Token_Type;

   subtype Operator_Type is Token_Type
     range T_Less_Than .. T_Or;

   subtype Multiplying_Operator is Token_Type
     range T_Star .. T_Divide;

   subtype Unary_Adding_Operator is Token_Type
     range T_Plus .. T_Minus;

   subtype Binary_Adding_Operator is Token_Type
     range T_Plus .. T_Concat;

   subtype Boolean_Type is Token_Type
     range T_True .. T_False;

   subtype Opening_Delimiter is Token_Type
     range T_Left_Paren .. T_Left_Brace;

   subtype Closing_Delimiter is Token_Type
     range T_Right_Paren ..  T_Right_Brace;

   subtype Delimiter_Type is Token_Type
     range T_Comma .. T_Exclamation;

   subtype Numeric_Type is Token_Type
     range T_Real_Literal .. T_Integer_Literal;

   subtype Reserved_Word_Type is Token_Type
     range T_And .. T_Proceed;

   First_Reserved_Word_Pos : constant :=
     Reserved_Word_Type'Pos (Reserved_Word_Type'First);
   Last_Reserved_Word_Pos  : constant :=
     Reserved_Word_Type'Pos (Reserved_Word_Type'Last);

   Token_Image : array (Token_Type) of Name_Id;

   ---------------------------------------------------
   -- Global variables updated by the token scanner --
   ---------------------------------------------------

   Token                : Token_Type;
   Token_Location       : Location renames Ocarina.Files.Buffer_Location;
   Token_Name           : Name_Id;   --  for T_Identifier (Lower case)
   Token_Display_Name   : Name_Id;   --  for T_Identifier (Carbon copy)
   String_Literal_Value : Name_Id;   --  for T_String

   Language           : constant String := "ao4aadl_specification";
   AO4AADL_Language  : Name_Id;

   Max_Number_Of_Digits : constant Integer := 20;
   --  Number of digits of the biggest allowed integer, 2**64 have 20
   --  digits

   Integer_Literal_Value : Unsigned_Long_Long;
   --  for Tokens : T_Integer_Literal

   Real_Literal_Value   : Long_Long_Float;
   --  for Tokens : T_Real_Literal

   Numeric_Literal_Base  : Unsigned_Short_Short;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   Numeric_Literal_Exp   : Integer;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   function Image (T : Token_Type) return String;
   --  Return an image of token T

   procedure Init_Tokens;
   --  Set all the token strings into the name table.

   function Quoted_Image (T : Token_Type) return String;
   --  Return the quoted image of token T

end Ocarina.ME_AO4AADL.Tokens;
