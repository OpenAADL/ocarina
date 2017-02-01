------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . M E _ A A D L _ B A . T O K E N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

package Ocarina.ME_AADL_BA.Tokens is

   pragma Elaborate_Body (Tokens);

   type BA_Token_Type is
     (T_Error,
      T_Identifier,

      --  Special_Characters

      T_Quotation_Mark,        --  "    (for T_String_Literal)
      T_Number_Sign,           --  #
      T_Underline,             --  _

      T_Comma,                 --  ,     First Delimiter

      T_Dot,                   --  .
      T_Colon,                 --  :
      T_Semicolon,             --  ;
      T_Colon_Colon,           --  ::
      T_Assignment,            --  :=
      T_Interval,              --  ..

      T_Greater_Greater_Than,  --  >>
      T_Tick,                  --  '

      T_Left_Parenthesis,      --  (     First Opening Delimiter
      T_Left_Square_Bracket,   --  [
      T_Left_Curly_Bracket,    --  {
      T_Begin_Annex,           --  {**
      T_Left_Step_Bracket,     --  -[

      T_Right_Parenthesis,     --  )     First Closing Delimiter
      T_Right_Square_Bracket,  --  ]
      T_Right_Curly_Bracket,   --  }
      T_End_Annex,             --  **}
      T_Right_Step_Bracket,    --  ]->

      T_Interrogative,         --  ?
      T_Exclamation,           --  !
      T_Exclamation_Greater,   --  !>
      T_Exclamation_Lesser,    --  !<

      --  Relational operator

      T_Less_Than_Sign,        --  <
      T_Equals_Sign,           --  =
      T_Greater_Than_Sign,     --  >
      T_Greater_Or_Equal,      --  >=
      T_Less_Or_Equal,         --  <=
      T_Non_Equal,             --  !=

      --  Unary and binary adding operator

      T_Plus,                  --  +
      T_Minus,                 --  -

      T_Concat,                --  &

      --  Multiplying operator

      T_Multiply,              --  *
      T_Divide,                --  /
      T_Mod,                   --  mod
      T_Rem,                   --  rem

      --  Highest precedence operator

      T_Exponent,              --  **
      T_Abs,                   --  abs
      T_Not,                   --  not

      --  Logical operator

      T_And,                   --  and
      T_Or,                    --  or
      T_Xor,                   --  xor

      --  Structure

      T_If,                    --  if
      T_Elsif,                 --  elsif
      T_Else,                  --  else
      T_End,                   --  end
      T_For,                   --  for
      T_While,                 --  while
      T_Do,                    --  do
      T_Until,                 --  until
      T_Forall,                --  forall

      --  Boolean

      T_True,                  --  true
      T_False,                 --  false

      --  Reserved Words

      T_Abort,                 --  abort
      T_Any,                   --  any
      T_Binding,               --  binding
      T_Complete,              --  complete
      T_Computation,           --  computation
      T_Count,                 --  count
      T_Delay,                 --  delay
      T_Dispatch,              --  dispatch
      T_Final,                 --  final
      T_Fixed,                 --  fixed
      T_Fresh,                 --  fresh
      T_Frozen,                --  frozen
      T_In,                    --  in
      T_Initial,               --  initial
      T_Lower_Bound,           --  lower_bound
      T_Mode,                  --  mode
      T_Normal,                --  normal
      T_On,                    --  on
      T_Ormore,                --  ormore
      T_Orless,                --  orless
      T_Others,                --  others
      T_Otherwise,             --  otherwise
      T_Poisson,               --  poisson
      T_Random,                --  random
      T_Self,                  --  self
      T_State,                 --  state
      T_States,                --  states
      T_Stop,                  --  stop
      T_Then,                  --  then
      T_Timeout,               --  timeout
      T_Transition,            --  transition
      T_Transitions,           --  transitions
      T_Updated,               --  updated
      T_Upper_Bound,           --  upper_bound
      T_Variables,             --  variables

      T_None,                  --  none, not in BA draft

      --  Numeric Literals

      T_Real_Literal,          --  real number
      T_Integer_Literal,       --  integer number

      --  Others

      T_String_Literal,        --  string
      T_Comment,               --  comment      (ignored)
      T_Raw_Text,              --  raw text (used for annex parsing
                                 --  / printing)

      T_EOF                    --  end of file
     );

   type BA_Token_List_Type is array (Positive range <>) of BA_Token_Type;

   --  Sybtype definitions

   subtype BA_Logical_Operator_Type is BA_Token_Type
     range T_And .. T_Xor;

   subtype BA_Relational_Operator is BA_Token_Type
     range T_Less_Than_Sign .. T_Non_Equal;

   subtype BA_Unary_Adding_Operator is BA_Token_Type
     range T_Minus .. T_Minus;

   subtype BA_Binary_Adding_Operator is BA_Token_Type
     range T_Plus .. T_Minus;

   subtype BA_Multiplying_Operator is BA_Token_Type
     range T_Multiply .. T_Rem;

   subtype BA_Highest_Precedence_Operator is BA_Token_Type
     range T_Exponent .. T_Not;

   subtype BA_Boolean_Type is BA_Token_Type
     range T_True .. T_False;

   subtype BA_Opening_Delimiter is BA_Token_Type
     range T_Left_Parenthesis .. T_Left_Step_Bracket;

   subtype BA_Closing_Delimiter is BA_Token_Type
     range T_Right_Parenthesis .. T_Right_Step_Bracket;

   subtype BA_Delimiter_Type is BA_Token_Type
     range T_Comma ..  T_Exclamation;

   subtype BA_Numeric_Type is BA_Token_Type
     range T_Real_Literal .. T_Integer_Literal;

   subtype BA_Reserved_Word_Type is BA_Token_Type
     range  T_Mod .. T_None;

   BA_First_Reserved_Word_Pos : constant :=
     BA_Reserved_Word_Type'Pos (BA_Reserved_Word_Type'First);
   BA_Last_Reserved_Word_Pos  : constant :=
     BA_Reserved_Word_Type'Pos (BA_Reserved_Word_Type'Last);

   BA_Token_Image    : array (BA_Token_Type) of Name_Id;

   Language       : constant String := "behavior_specification";
   BA_Language  : Name_Id;

   Max_Number_Of_Digits : constant Integer := 20;
   --  Number of digits of the biggest allowed integer, 2**64 have 20
   --  digits

   Integer_Literal_Value : Unsigned_Long_Long;
   --  for Tokens : T_Integer_Literal

   Float_Literal_Value   : Long_Long_Float;
   --  for Tokens : T_Real_Literal

   Numeric_Literal_Base  : Unsigned_Short_Short;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   Numeric_Literal_Exp   : Integer;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   function Image (T : BA_Token_Type) return String;
   --  Return an image of token T

   procedure Init_Tokens;
   --  Set all the token strings into the name table.

   function Quoted_Image (T : BA_Token_Type) return String;
   --  Return the quoted image of token T

end Ocarina.ME_AADL_BA.Tokens;
