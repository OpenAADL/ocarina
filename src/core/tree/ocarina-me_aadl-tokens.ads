------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . M E _ A A D L . T O K E N S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

package Ocarina.ME_AADL.Tokens is

   pragma Elaborate_Body (Tokens);

   type Token_Type is
     (T_Error,
      T_Identifier,

   --  Special_Characters

      T_Quotation_Mark,        --  "    (for T_String_Literal)
      T_Number_Sign,           --  #
      T_Underline,             --  _

      T_Comma,                 --  ,     First Delimiter

      T_Plus,                  --  +     First Binary Operator
      T_Minus,                 --  -
      T_Multiply,              --  *
      T_Divide,                --  /     Last Binary Operator

      T_Dot,                   --  .
      T_Colon,                 --  :
      T_Semicolon,             --  ;
      T_Less_Than_Sign,        --  <
      T_Equals_Sign,           --  =
      T_Greater_Than_Sign,     --  >

      T_Association,           --  =>
      T_Additive_Association,  --  +=>
      T_Direct_Connection,     --  ->    T_Sample_Connection in AADLv1
      T_Bidirect_Connection,   --  <->
      T_Delayed_Connection,    --  ->>   only in AADLv1
      T_Interval,              --  ..
      T_Colon_Colon,           --  ::

      T_Left_Parenthesis,      --  (     First Opening Delimiter
      T_Left_Square_Bracket,   --  [
      T_Left_Curly_Bracket,    --  {
      T_Left_Step_Bracket,     --  -[
      T_Begin_Annex,           --  {**   Last Opening Delimiter

      T_Right_Parenthesis,     --  )     First Closing Delimiter
      T_Right_Square_Bracket,  --  ]
      T_Right_Curly_Bracket,   --  }
      T_Right_Step_Bracket,    --  ]->
      T_End_Annex,             --  **}   Last Closing Delimiter, Last Delimiter

   --  Reserved Words

      T_AADLBoolean,           --  'aadlboolean'
      T_AADLInteger,           --  'aadlinteger'
      T_AADLReal,              --  'aadlreal'
      T_AADLString,            --  'aadlstring'
      T_Abstract,              --  'abstract'
      T_Access,                --  'access'
      T_All,                   --  'all'
      T_And,                   --  'and'
      T_Annex,                 --  'annex'
      T_Applies,               --  'applies'
      T_Binding,               --  'binding'
      T_Bus,                   --  'bus'
      T_Calls,                 --  'calls'
      T_Classifier,            --  'classifier'
      T_Compute,               --  'compute'
      T_Connections,           --  'connections'
      T_Constant,              --  'constant'
      T_Data,                  --  'data'
      T_Delta,                 --  'delta'
      T_Device,                --  'device'
      T_End,                   --  'end'
      T_Enumeration,           --  'enumeration'
      T_Event,                 --  'event'
      T_Extends,               --  'extends'
      T_False,                 --  'false'                Boolean Value
      T_True,                  --  'true'                 Boolean Value
      T_Feature,               --  'feature'
      T_Features,              --  'features'
      T_Flow,                  --  'flow'
      T_Flows,                 --  'flows'
      T_Group,                 --  'group'
      T_Implementation,        --  'implementaion'
      T_In,                    --  'in'
      T_Inherit,               --  'inherit'
      T_Initial,               --  'initial'
      T_Inverse,               --  'inverse'
      T_Is,                    --  'is'
      T_List,                  --  'list'
      T_Memory,                --  'memory'
      T_Mode,                  --  'mode'
      T_Modes,                 --  'modes'
      T_None,                  --  'none'
      T_Not,                   --  'not'
      T_Of,                    --  'of'
      T_Or,                    --  'or'
      T_Out,                   --  'out'
      T_Package,               --  'package'
      T_Parameter,             --  'parameter'
      T_Path,                  --  'path'
      T_Port,                  --  'port'
      T_Private,               --  'private'
      T_Process,               --  'process'
      T_Processor,             --  'processor'
      T_Properties,            --  'properties'
      T_Property,              --  'property'
      T_Prototypes,            --  'prototypes'
      T_Provides,              --  'provides'
      T_Public,                --  'public'
      T_Range,                 --  'range'
      T_Record,                --  'record'
      T_Reference,             --  'reference'
      T_Refined,               --  'refined'
      T_Refines,               --  'refines'
      T_Renames,               --  'renames'
      T_Requires,              --  'requires'
      T_Self,                  --  'self'
      T_Server,                --  'server'
      T_Set,                   --  'set'
      T_Sink,                  --  'sink'
      T_Source,                --  'source'
      T_Subcomponents,         --  'subcomponents'
      T_Subprogram,            --  'subprogram'
      T_System,                --  'system'
      T_Thread,                --  'thread'
      T_To,                    --  'to'
      T_Type,                  --  'type'
      T_Units,                 --  'units'
      T_Value,                 --  'value'
      T_Virtual,               --  'virtual'
      T_With,                  --  'with'

   --  Numeric Literals

      T_Real_Literal,          --  real number
      T_Integer_Literal,       --  integer number

   --  Others

      T_String_Literal,        --  string
      T_Comment,               --  comment      (ignored)
      T_Raw_Text,              --  raw text (used for annex parsing / printing)

      T_EOF                    --  end of file
      );

   First_Token_Pos : constant := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos  : constant := Token_Type'Pos (Token_Type'Last);

   type Token_List_Type is array (Positive range <>) of Token_Type;

   --  Sybtype definitions

   subtype Binary_Operator_Type is Token_Type range T_Plus .. T_Divide;

   subtype Boolean_Type is Token_Type range T_False .. T_True;

   subtype Opening_Delimiter is
     Token_Type range T_Left_Parenthesis .. T_Begin_Annex;

   subtype Closing_Delimiter is
     Token_Type range T_Right_Parenthesis .. T_End_Annex;

   subtype Delimiter_Type is Token_Type range T_Comma .. T_End_Annex;

   subtype Numeric_Type is
     Token_Type range T_Real_Literal .. T_Integer_Literal;

   subtype Reserved_Word_Type is Token_Type range T_AADLBoolean .. T_With;

   First_Reserved_Word_Pos : constant :=
     Reserved_Word_Type'Pos (Reserved_Word_Type'First);
   Last_Reserved_Word_Pos : constant :=
     Reserved_Word_Type'Pos (Reserved_Word_Type'Last);

   type Property_Owner_Token is
     (POT_Error,
      POT_Call,                --  'call'
      POT_Connection,          --  'connection'
      POT_Component,           --  'component'
      POT_Element,             --  'element'
      POT_Instance,            --  'instance'
      POT_Named,               --  'named'
      POT_Prototype,           --  'prototype'
      POT_Sequence,            --  'sequence'
      POT_Specification,       --  'specification'
      POT_Subcomponent,        --  'subcomponent'
      POT_Transition           --  'transition'
      );
   --  Elements defined in this enumeration must not be AADL keywords

   subtype Property_Owner_Type is
     Property_Owner_Token range POT_Call .. POT_Transition;

   First_PO_Type_Pos : constant :=
     Property_Owner_Type'Pos (Property_Owner_Type'First);
   Last_PO_Type_Pos : constant :=
     Property_Owner_Type'Pos (Property_Owner_Type'Last);

   Token_Image    : array (Token_Type) of Name_Id;
   Token_PO_Image : array (Property_Owner_Token) of Name_Id;
   --  Tables of actual token images

   Max_Number_Of_Digits : constant Integer := 20;
   --  Number of digits of the biggest allowed integer, 2**64 have 20
   --  digits

   Integer_Literal_Value : Unsigned_Long_Long;
   --  for Tokens : T_Integer_Literal

   Float_Literal_Value : Long_Long_Float;
   --  for Tokens : T_Real_Literal

   Numeric_Literal_Base : Unsigned_Short_Short;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   Numeric_Literal_Exp : Integer;
   --  for Tokens : T_Integer_Literal, T_Real_Literal

   function Image (T : Token_Type) return String;
   --  Return an image of token T

   procedure Init_Tokens;
   --  Set all the token strings into the name table.

   procedure Init_Property_Owner_Tokens;
   --  Set all the token string for property owner
   --  into the name table.

   function Quoted_Image (T : Token_Type) return String;
   --  Return the quoted image of token T

end Ocarina.ME_AADL.Tokens;
