------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . M E _ A A D L _ E M A . E M A _ T O K E N S        --
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

--  Description of EMA tokens

with Ocarina.Types;
with Locations;
with Ocarina.Files;

package Ocarina.ME_AADL_EMA.EMA_Tokens is

   use Ocarina.Types;
   use Locations;

   Suffix : constant String := "%ema%";
   --  This string is used to avoid clashing between several language
   --  keywords when associating them to the corresponding positions
   --  in Token_Type.

   type Token_Type is
     (T_Error,
      --  error is a reserved word so T_R_Error
      T_Error_Property_Id,
      T_Property_Identifier,
      T_Identifier,
      T_Mode_Name,
      T_Error_Behavior_State_Id,
      T_Composite_State_Id,
      T_Propagation_Point_Id,
      T_Property_Name,
      T_Target_Error_State_Id,

      --  Special_Characters
      T_Quotation_Mark,        --  "    (for T_String_Literal)
      T_Underline,             --  _

      --  Binary Operator (used in properties)
      T_Minus,                 --  -
      T_Plus,                  --  +
      T_Multiply,              --  X

      T_Comma,                 --  ,     First Delimiter
      T_Dot,                   --  .
      T_Colon,                 --  :
      T_Semi_Colon,            --  ;
      T_Star,                  --  *
      T_Interval,              --  ..
      T_Colon_Colon,           --  ::

      T_Association,           --  =>
      T_Direct_Connection,     --  ->

      T_Left_Paren,            --  (     First Opening Delimiter
      T_Left_Bracket,          --  [
      T_Left_Brace,            --  {
      T_Left_Step_Bracket,     --  -[

      T_Right_Paren,           --  )     First Closing Delimiter
      T_Right_Bracket,         --  ]
      T_Right_Brace,           --  }
      T_Right_Step_Bracket,    --  ]->

      T_Exclamation,           --  !

      --  Relational operator

      T_And,                   --  and
      T_Or,                    --  or
      T_Not,                   --  not
      T_OrMore,                --  ormore
      T_OrLess,                --  orless

      --  Structure

      T_If,                    --  if
      T_When,                  --  when

      --  Reserved Words

      T_AADLReal,              --  aadlreal
      T_AADLString,            --  aadlstring
      T_AADLInteger,           --  aadlinteger
      T_Equivalence,           --  equivalence
      T_Mappings,              --  mappings
      T_Applies,               --  applies
      T_To,                    --  to
      T_Properties,            --  properties
      T_All,                   --  all
      T_Aame,                  --  same
      T_Recover,               --  recover
      T_Repair,                --  repair
      T_With,                  --  with
      T_In,                    --  in
      T_Out,                   --  out
      T_R_Error,               --  error
      T_Event,                 --  event
      T_Events,                --  events
      T_Same,                  --  same
      T_State,                 --  state
      T_States,                --  states
      T_Behavior,              --  behavior
      T_Type,                  --  type
      T_Types,                 --  types
      T_End,                   --  end
      T_Extends,               --  extends
      T_Use,                   --  use
      T_Initial,               --  initial
      T_Transitions,           --  transitions
      T_Others,                --  others
      T_Propagations,          --  propagation
      T_Propagation,           --  propagations
      T_Flows,                 --  flows
      T_Flow,                  --  flow
      T_Renames,               --  renames
      T_Set,                   --  set
      T_Noerror,               --  noerror
      T_Access,                --  access
      T_Processor,             --  processor
      T_Memory,                --  memory
      T_Connection,            --  connection
      T_Binding,               --  binding
      T_Bindings,              --  bindings
      T_Source,                --  source
      T_Sink,                  --  sink
      T_Path,                  --  path
      T_Paths,                 --  paths
      T_Point,                 --  point
      T_Transformations,       --  transformations
      T_Component,             --  component
      T_Detections,            --  detections
      T_Mode,                  --  mode
      T_Modes,                 --  modes
      T_Constant,              --  constant
      T_Composite,             --  composite

      --  Numeric Literals

      T_Real_Literal,          --  real number
      T_Integer_Literal,       --  integer number
      T_Numeric_Literal,       --  real or integer number
      --  : used in parser-errors

      --  Others

      T_String_Literal,        --  string
      T_Comment,               --  comment      (ignored)

      T_EOF                    --  end of file
     );

   First_Token_Pos : constant := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos  : constant := Token_Type'Pos (Token_Type'Last);

   type Token_List_Type is array (Positive range <>) of Token_Type;

   subtype Opening_Delimiter is Token_Type
     range T_Left_Paren .. T_Left_Step_Bracket;

   subtype Binary_Operator_Type is Token_Type
     range T_Minus .. T_Multiply;

   subtype Closing_Delimiter is Token_Type
     range T_Right_Paren ..  T_Right_Step_Bracket;

   subtype Delimiter_Type is Token_Type
     range T_Comma .. T_Exclamation;

   subtype Numeric_Type is Token_Type
     range T_Real_Literal .. T_Integer_Literal;

   subtype Reserved_Word_Type is Token_Type
     range T_And .. T_Composite;

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

   Language           : constant String := "emv2";
   EMA_Language  : Name_Id;

   Max_Number_Of_Digits : constant Integer := 20;
   --  Number of digits of the biggest allowed integer, 2**64 have 20
   --  digits

   Integer_Literal_Value : Unsigned_Long_Long;
   --  for Tokens : T_Integer_Literal

   Real_Literal_Value   : Long_Long_Float;
   --  for Tokens : T_Real_Literal

   Float_Literal_Value   : Long_Long_Float;
   --  for Tokens : T_Real_Literal

   Numeric_Literal_Base  : Unsigned_Short_Short;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   Numeric_Literal_Exp   : Integer;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   ----------------------------------------------------

   function Image (T : Token_Type) return String;
   --  Return an image of token T

   procedure Init_Tokens;
   --  Set all the token strings into the name table.

   function Quoted_Image (T : Token_Type) return String;
   --  Return the quoted image of token T

end Ocarina.ME_AADL_EMA.EMA_Tokens;
