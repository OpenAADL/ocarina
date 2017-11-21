------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . M E _ A O 4 A A D L . T O K E N S             --
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

package body Ocarina.ME_AO4AADL.Tokens is

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
      New_Token (T_Identifier, "<identifier>");

      New_Token (T_Quotation_Mark, """");
      New_Token (T_Underline, "_");

      New_Token (T_Comma, ",");

      New_Token (T_Plus, "+");
      New_Token (T_Minus, "-");
      New_Token (T_Star, "*");
      New_Token (T_Divide, "/");

      New_Token (T_Dot, ".");

      New_Token (T_Colon, ":");
      New_Token (T_Semi_Colon, ";");
      New_Token (T_Less_Than, "<");
      New_Token (T_Equal, "=");
      New_Token (T_Greater_Than, ">");
      New_Token (T_Greater_Or_Equal, ">=");
      New_Token (T_Less_Or_Equal, "<=");
      New_Token (T_Non_Equal, "!=");

      New_Token (T_Interval, "..");
      New_Token (T_Colon_Colon, "::");
      New_Token (T_Assignment, ":=");
      New_Token (T_Tick, "'");

      New_Token (T_Left_Paren, "(");
      New_Token (T_Right_Paren, ")");
      New_Token (T_Left_Bracket, "[");
      New_Token (T_Right_Bracket, "]");
      New_Token (T_Left_Brace, "{");
      New_Token (T_Right_Brace, "}");

      New_Token (T_Interrogative, "?");
      New_Token (T_Exclamation, "!");

      New_Token (T_And, "and");
      New_Token (T_Not, "not");
      New_Token (T_Or, "or");
      New_Token (T_Or_Logic, "||");
      New_Token (T_Concat, "&&");

      New_Token (T_True, "true");
      New_Token (T_False, "false");

      New_Token (T_Precedence, "precedence");
      New_Token (T_Aspect, "aspect");
      New_Token (T_Applied, "applied");
      New_Token (T_To, "to");
      New_Token (T_Thread, "thread");
      New_Token (T_Process, "process");
      New_Token (T_Subprogram, "subprogram");
      New_Token (T_System, "system");
      New_Token (T_Pointcut, "pointcut");
      New_Token (T_Call, "call");
      New_Token (T_Execution, "execution");
      New_Token (T_Args, "args");
      New_Token (T_Inport, "inport");
      New_Token (T_Outport, "outport");
      New_Token (T_InOutPort, "inoutport");
      New_Token (T_Advice, "advice");
      New_Token (T_Before, "before");
      New_Token (T_After, "after");
      New_Token (T_Around, "around");
      New_Token (T_Variables, "variables");
      New_Token (T_Initially, "initially");
      New_Token (T_If, "if");
      New_Token (T_Elsif, "elsif");
      New_Token (T_Else, "else");
      New_Token (T_For, "for");
      New_Token (T_While, "while");
      New_Token (T_In, "in");
      New_Token (T_Count, "count");
      New_Token (T_Fresh, "fresh");
      New_Token (T_Computation, "computation");
      New_Token (T_Delay, "delay");
      New_Token (T_Proceed, "proceed");

      New_Token (T_Real_Literal, "<real literal>");
      New_Token (T_Integer_Literal, "<integer literal>");

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
           | T_Integer_Literal | T_Real_Literal | T_String_Literal =>
            return Image (T);

         when others =>
            return "'" & Image (T) & "'";
      end case;
   end Quoted_Image;

end Ocarina.ME_AO4AADL.Tokens;
