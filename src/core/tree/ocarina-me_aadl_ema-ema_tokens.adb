------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . M E _ A A D L _ E M A . E M A _ T O K E N S        --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet;
with Charset;

package body Ocarina.ME_AADL_EMA.EMA_Tokens is

   use Ocarina.Namet;
   use Charset;

   procedure New_Token (Token : Token_Type; Image : String);
   --  Compute token image and store it in Token_Image table. When
   --  Token is a reserved word, embrace its image between double
   --  quotes. Enter the lower-case form of a reserved word image into
   --  the name table and set name table byte to its token position in
   --  order to resolve it easily.

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
   begin
      return Get_Name_String (Token_Image (T));
   end Image;

   -----------------
   -- Init_Tokens --
   -----------------

   procedure Init_Tokens is
   begin
      New_Token (T_Error, "[ERROR]");
      New_Token (T_Error_Property_Id, "[ERROR]");
      New_Token (T_Property_Identifier, "<property identifier>");
      New_Token (T_Identifier, "<identifier>");

      New_Token (T_Quotation_Mark, """");
      New_Token (T_Underline, "_");

      New_Token (T_Minus, "-");
      New_Token (T_Plus, "+");
      New_Token (T_Multiply, "X");

      New_Token (T_Comma, ",");
      New_Token (T_Dot, ".");
      New_Token (T_Colon, ":");
      New_Token (T_Star, "*");
      New_Token (T_Semi_Colon, ";");

      New_Token (T_Interval, "..");
      New_Token (T_Colon_Colon, "::");
      New_Token (T_Association, "=>");
      New_Token (T_Direct_Connection, "->");

      New_Token (T_Left_Paren, "(");
      New_Token (T_Right_Paren, ")");
      New_Token (T_Left_Bracket, "[");
      New_Token (T_Right_Bracket, "]");
      New_Token (T_Left_Brace, "{");
      New_Token (T_Right_Brace, "}");
      New_Token (T_Left_Step_Bracket, "-[");
      New_Token (T_Right_Step_Bracket, "]->");

      New_Token (T_Exclamation, "!");

      New_Token (T_And, "and");
      New_Token (T_Not, "not");
      New_Token (T_Or, "or");
      New_Token (T_OrMore, "ormore");
      New_Token (T_OrLess, "orless");

      New_Token (T_If, "if");
      New_Token (T_When, "when");

      New_Token (T_AADLReal, "aadlreal");
      New_Token (T_AADLString, "aadlstring");
      New_Token (T_AADLInteger, "aadlinteger");
      New_Token (T_Equivalence,  "equivalence");
      New_Token (T_Mappings,  "mappings");
      New_Token (T_Applies,  "applies");
      New_Token (T_To,  "to");
      New_Token (T_Properties,  "properties");
      New_Token (T_All,  "all");
      New_Token (T_Aame,  "same");
      New_Token (T_Recover,  "recover");
      New_Token (T_Repair,  "repair");
      New_Token (T_With,  "with");
      New_Token (T_In,  "in");
      New_Token (T_Out,  "out");
      New_Token (T_R_Error,  "error");
      New_Token (T_Event,  "event");
      New_Token (T_Events,  "events");
      New_Token (T_Same,  "same");
      New_Token (T_State,  "state");
      New_Token (T_States,  "states");
      New_Token (T_Behavior,  "behavior");
      New_Token (T_Type,  "type");
      New_Token (T_Types,  "types");
      New_Token (T_End,  "end");
      New_Token (T_Extends,  "extends");
      New_Token (T_Use,  "use");
      New_Token (T_Initial,  "initial");
      New_Token (T_Transitions,  "transitions");
      New_Token (T_Others,  "others");
      New_Token (T_Propagations,  "propagations");
      New_Token (T_Propagation,  "propagation");
      New_Token (T_Flows,  "flows");
      New_Token (T_Flow,  "flow");
      New_Token (T_Renames,  "renames");
      New_Token (T_Set,  "set");
      New_Token (T_Noerror,  "noerror");
      New_Token (T_Access,  "access");
      New_Token (T_Processor,  "processor");
      New_Token (T_Memory,  "memory");
      New_Token (T_Connection, "connection");
      New_Token (T_Binding,  "binding");
      New_Token (T_Bindings,  "bindings");
      New_Token (T_Source,  "source");
      New_Token (T_Sink,  "sink");
      New_Token (T_Path,  "path");
      New_Token (T_Paths,  "paths");
      New_Token (T_Point,  "point");
      New_Token (T_Transformations,  "transformations");
      New_Token (T_Component,  "component");
      New_Token (T_Detections,  "detections");
      New_Token (T_Mode,  "mode");
      New_Token (T_Modes,  "modes");
      New_Token (T_Constant,  "constant");
      New_Token (T_Composite,  "composite");

      New_Token (T_Real_Literal, "<real literal>");
      New_Token (T_Integer_Literal, "<integer literal>");
      New_Token (T_Numeric_Literal, "<numeric literal>");

      New_Token (T_String_Literal, "<string literal>");
      New_Token (T_Comment, "<comment>");

      New_Token (T_EOF, "[EOF]");

   end Init_Tokens;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token (Token : Token_Type; Image : String) is
   begin
      Set_Str_To_Name_Buffer (Image);
      Token_Image (Token) := Name_Find;

      if Token in Reserved_Word_Type then
         To_Lower (Name_Buffer (1 .. Name_Len));
         Add_Str_To_Name_Buffer (Suffix);
         Set_Name_Table_Byte (Name_Find, Byte (Token_Type'Pos (Token)));
      end if;
   end New_Token;

   ------------------
   -- Quoted_Image --
   ------------------

   function Quoted_Image (T : Token_Type) return String is
   begin
      case T is
         when T_Identifier
           | T_Integer_Literal | T_Real_Literal | T_String_Literal
           | T_Numeric_Literal =>
            return Image (T);

         when others =>
            return "'" & Image (T) & "'";
      end case;
   end Quoted_Image;

end Ocarina.ME_AADL_EMA.EMA_Tokens;
