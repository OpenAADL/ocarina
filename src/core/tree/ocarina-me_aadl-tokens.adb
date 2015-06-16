------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . M E _ A A D L . T O K E N S                --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet;
with Charset;

package body Ocarina.ME_AADL.Tokens is

   use Ocarina.Namet;
   use Charset;

   procedure New_Token (Token : Token_Type; Image : String);
   --  Compute token image and store it in Token_Image table. When
   --  Token is a reserved word, embrace its image between double
   --  quotes. Enter the lower-case form of a reserved word image into
   --  the name table and set name table byte to its token position in
   --  order to resolve it easily.

   procedure New_Property_Owner_Token
     (Token : Property_Owner_Token;
      Image : String);

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
      New_Token (T_Identifier, "<identifier>");

      New_Token (T_Quotation_Mark, """");
      New_Token (T_Number_Sign, "#");
      New_Token (T_Underline, "_");

      New_Token (T_Comma, ",");

      New_Token (T_Plus, "+");
      New_Token (T_Minus, "-");
      New_Token (T_Multiply, "*");
      New_Token (T_Divide, "/");

      New_Token (T_Dot, ".");
      New_Token (T_Colon, ":");
      New_Token (T_Semicolon, ";");
      New_Token (T_Less_Than_Sign, "<");
      New_Token (T_Equals_Sign, "=");
      New_Token (T_Greater_Than_Sign, ">");

      New_Token (T_Association, "=>");
      New_Token (T_Additive_Association, "+=>");
      New_Token (T_Direct_Connection, "->");
      New_Token (T_Interval, "..");
      New_Token (T_Colon_Colon, "::");

      New_Token (T_Left_Parenthesis, "(");
      New_Token (T_Right_Parenthesis, ")");
      New_Token (T_Left_Square_Bracket, "[");
      New_Token (T_Right_Square_Bracket, "]");
      New_Token (T_Left_Curly_Bracket, "{");
      New_Token (T_Right_Curly_Bracket, "}");
      New_Token (T_Left_Step_Bracket, "-[");
      New_Token (T_Right_Step_Bracket, "]->");
      New_Token (T_Begin_Annex, "{**");
      New_Token (T_End_Annex, "**}");

      New_Token (T_AADLBoolean, "aadlboolean");
      New_Token (T_AADLInteger, "aadlinteger");
      New_Token (T_AADLReal, "aadlreal");
      New_Token (T_AADLString, "aadlstring");
      New_Token (T_Access, "access");
      New_Token (T_All, "all");
      New_Token (T_And, "and");
      New_Token (T_Annex, "annex");
      New_Token (T_Applies, "applies");
      New_Token (T_Binding, "binding");
      New_Token (T_Bus, "bus");
      New_Token (T_Calls, "calls");
      New_Token (T_Classifier, "classifier");
      New_Token (T_Connections, "connections");
      New_Token (T_Constant, "constant");
      New_Token (T_Data, "data");
      New_Token (T_Delta, "delta");
      New_Token (T_Device, "device");
      New_Token (T_End, "end");
      New_Token (T_Enumeration, "enumeration");
      New_Token (T_Event, "event");
      New_Token (T_Extends, "extends");

      New_Token (T_False, "false");
      New_Token (T_True, "true");

      New_Token (T_Features, "features");
      New_Token (T_Flow, "flow");
      New_Token (T_Flows, "flows");
      New_Token (T_Group, "group");
      New_Token (T_Implementation, "implementation");
      New_Token (T_In, "in");
      New_Token (T_Inherit, "inherit");
      New_Token (T_Initial, "initial");
      New_Token (T_Inverse, "inverse");
      New_Token (T_Is, "is");
      New_Token (T_List, "list");
      New_Token (T_Memory, "memory");
      New_Token (T_Mode, "mode");
      New_Token (T_Modes, "modes");
      New_Token (T_None, "none");
      New_Token (T_Not, "not");
      New_Token (T_Of, "of");
      New_Token (T_Or, "or");
      New_Token (T_Out, "out");
      New_Token (T_Package, "package");
      New_Token (T_Parameter, "parameter");
      New_Token (T_Path, "path");
      New_Token (T_Port, "port");
      New_Token (T_Private, "private");
      New_Token (T_Process, "process");
      New_Token (T_Processor, "processor");
      New_Token (T_Properties, "properties");
      New_Token (T_Property, "property");
      New_Token (T_Provides, "provides");
      New_Token (T_Public, "public");
      New_Token (T_Range, "range");
      New_Token (T_Reference, "reference");
      New_Token (T_Refined, "refined");
      New_Token (T_Refines, "refines");
      New_Token (T_Requires, "requires");
      New_Token (T_Server, "server");
      New_Token (T_Set, "set");
      New_Token (T_Sink, "sink");
      New_Token (T_Source, "source");
      New_Token (T_Subcomponents, "subcomponents");
      New_Token (T_Subprogram, "subprogram");
      New_Token (T_System, "system");
      New_Token (T_Thread, "thread");
      New_Token (T_To, "to");
      New_Token (T_Type, "type");
      New_Token (T_Units, "units");

      New_Token (T_Real_Literal, "<real literal>");
      New_Token (T_Integer_Literal, "<integer literal>");

      New_Token (T_String_Literal, "<string literal>");
      New_Token (T_Comment, "<comment>");

      New_Token (T_EOF, "[EOF]");

      case AADL_Version is
         when AADL_V1 =>
            New_Token (T_Value, "value");
            New_Token (T_Server, "server");
            New_Token (T_Delayed_Connection, "->>");

         when AADL_V2 =>
            New_Token (T_Abstract, "abstract");
            New_Token (T_Compute, "compute");
            New_Token (T_Bidirect_Connection, "<->");
            New_Token (T_Feature, "feature");
            New_Token (T_Prototypes, "prototypes");
            New_Token (T_Record, "record");
            New_Token (T_Renames, "renames");
            New_Token (T_Self, "self");
            New_Token (T_Virtual, "virtual");
            New_Token (T_With, "with");
      end case;

   end Init_Tokens;

   --------------------------------
   -- Init_Property_Owner_Tokens --
   --------------------------------

   procedure Init_Property_Owner_Tokens is
   begin
      New_Property_Owner_Token (POT_Error, "[ERROR]");
      New_Property_Owner_Token (POT_Call, "call");
      New_Property_Owner_Token (POT_Connection, "connection");
      New_Property_Owner_Token (POT_Component, "component");
      New_Property_Owner_Token (POT_Element, "element");
      New_Property_Owner_Token (POT_Instance, "instance");
      New_Property_Owner_Token (POT_Named, "named");
      New_Property_Owner_Token (POT_Prototype, "prototype");
      New_Property_Owner_Token (POT_Sequence, "sequence");
      New_Property_Owner_Token (POT_Specification, "specification");
      New_Property_Owner_Token (POT_Subcomponent, "subcomponent");
      New_Property_Owner_Token (POT_Transition, "transition");
   end Init_Property_Owner_Tokens;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token (Token : Token_Type; Image : String) is
   begin
      Set_Str_To_Name_Buffer (Image);
      Token_Image (Token) := Name_Find;

      if Token in Reserved_Word_Type then
         To_Lower (Name_Buffer (1 .. Name_Len));
         Set_Name_Table_Byte (Name_Find, Byte (Token_Type'Pos (Token)));
      end if;
   end New_Token;

   ------------------------------
   -- New_Property_Owner_Token --
   ------------------------------

   procedure New_Property_Owner_Token
     (Token : Property_Owner_Token;
      Image : String)
   is
   begin
      Set_Str_To_Name_Buffer (Image);
      Token_PO_Image (Token) := Name_Find;

      if Token in Property_Owner_Type then
         To_Lower (Name_Buffer (1 .. Name_Len));
         Set_Name_Table_Byte
           (Name_Find,
            Byte (Property_Owner_Token'Pos (Token)));
      end if;
   end New_Property_Owner_Token;

   ------------------
   -- Quoted_Image --
   ------------------

   function Quoted_Image (T : Token_Type) return String is
   begin
      case T is
         when T_Identifier   |
           T_Integer_Literal |
           T_Real_Literal    |
           T_String_Literal  =>
            return Image (T);

         when others =>
            return "'" & Image (T) & "'";
      end case;
   end Quoted_Image;

end Ocarina.ME_AADL.Tokens;
