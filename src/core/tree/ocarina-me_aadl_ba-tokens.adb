------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . M E _ A A D L _ B A . T O K E N S             --
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

with Ocarina.Namet;
with Charset;

package body Ocarina.ME_AADL_BA.Tokens is

   use Ocarina.Namet;
   use Charset;

   Prefix : constant String := "%ba%";

   procedure New_Token (Token : BA_Token_Type; Image : String);
   --  Compute token image and store it in BA_Token_Image table. When
   --  Token is a reserved word, embrace its image between double
   --  quotes. Enter the lower-case form of a reserved word image into
   --  the name table and set name table byte to its token position in
   --  order to resolve it easily.

   -----------
   -- Image --
   -----------

   function Image (T : BA_Token_Type) return String is
      S : constant String := Get_Name_String (BA_Token_Image (T));
   begin
      return S (S'First + Prefix'Length .. S'Last);
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

      New_Token (T_Exponent, "**");
      New_Token (T_Concat, "&");

      New_Token (T_Dot, ".");
      New_Token (T_Colon, ":");
      New_Token (T_Semicolon, ";");
      New_Token (T_Less_Than_Sign, "<");
      New_Token (T_Equals_Sign, "=");
      New_Token (T_Greater_Than_Sign, ">");
      New_Token (T_Greater_Or_Equal, ">=");
      New_Token (T_Less_Or_Equal, "<=");
      New_Token (T_Non_Equal, "!=");
      New_Token (T_Colon_Colon, "::");
      New_Token (T_Assignment, ":=");
      New_Token (T_Interval, "..");

      New_Token (T_Greater_Greater_Than, ">>");
      New_Token (T_Tick, "'");

      New_Token (T_Left_Parenthesis, "(");
      New_Token (T_Right_Parenthesis, ")");
      New_Token (T_Left_Square_Bracket, "[");
      New_Token (T_Right_Square_Bracket, "]");
      New_Token (T_Left_Curly_Bracket, "{");
      New_Token (T_Begin_Annex, "{**");
      New_Token (T_End_Annex, "**}");
      New_Token (T_Right_Curly_Bracket, "}");
      New_Token (T_Left_Step_Bracket, "-[");
      New_Token (T_Right_Step_Bracket, "]->");

      New_Token (T_Interrogative, "?");
      New_Token (T_Exclamation, "!");

      New_Token (T_And, "and");
      New_Token (T_Or, "or");
      New_Token (T_Not, "not");
      New_Token (T_On, "on");
      New_Token (T_In, "in");
      New_Token (T_Ormore, "ormore");
      New_Token (T_Orless, "orless");
      New_Token (T_Not, "not");
      New_Token (T_Xor, "xor");
      New_Token (T_Cand, "cand");
      New_Token (T_Cor, "cor");

      New_Token (T_If, "if");
      New_Token (T_Elsif, "elsif");
      New_Token (T_Else, "else");
      New_Token (T_End, "end");
      New_Token (T_For, "for");
      New_Token (T_While, "while");

      New_Token (T_True, "true");
      New_Token (T_False, "false");

      New_Token (T_Abort, "abort");
      New_Token (T_Abs, "abs");
      New_Token (T_Any, "any");
      New_Token (T_Complete, "complete");
      New_Token (T_Computation, "computation");
      New_Token (T_Count, "count");
      New_Token (T_Delay, "delay");
      New_Token (T_Dispatch, "dispatch");
      New_Token (T_Final, "final");
      New_Token (T_Fixed, "fixed");
      New_Token (T_Fresh, "fresh");
      New_Token (T_Frozen, "frozen");
      New_Token (T_Initial, "initial");
      New_Token (T_Mod, "mod");
      New_Token (T_Mode, "mode");
      New_Token (T_Normal, "normal");
      New_Token (T_Others, "others");
      New_Token (T_Poisson, "poisson");
      New_Token (T_Random, "random");
      New_Token (T_Rem, "rem");
      New_Token (T_State, "state");
      New_Token (T_States, "states");
      New_Token (T_Stop, "stop");
      New_Token (T_Timeout, "timeout");
      New_Token (T_Transition, "transition");
      New_Token (T_Transitions, "transitions");
      New_Token (T_Variables, "variables");
      New_Token (T_None, "none");

      New_Token (T_Real_Literal, "<real literal>");
      New_Token (T_Integer_Literal, "<integer literal>");

      New_Token (T_String_Literal, "<string literal>");
      New_Token (T_Comment, "<comment>");

      New_Token (T_EOF, "[EOF]");

   end Init_Tokens;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token (Token : BA_Token_Type; Image : String) is
   begin
      Set_Str_To_Name_Buffer (Prefix & Image);
      BA_Token_Image (Token) := Name_Find;

      if Token in BA_Reserved_Word_Type then
         To_Lower (Name_Buffer (1 .. Name_Len));
         Set_Name_Table_Byte (Name_Find, Byte (BA_Token_Type'Pos (Token)));
      end if;
   end New_Token;

   ------------------
   -- Quoted_Image --
   ------------------

   function Quoted_Image (T : BA_Token_Type) return String is
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

end Ocarina.ME_AADL_BA.Tokens;
