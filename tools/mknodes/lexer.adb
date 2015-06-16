------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                L E X E R                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Charset;
with Errors;
with Namet;
with Output;
with Types;
use type Types.Text_Buffer_Ptr, Types.Text_Ptr;
use type Types.Int, Types.Byte;

package body Lexer is

   Buffer : Types.Text_Buffer_Ptr;
   --  Once preprocessed, the idl file is loaded in Buffer and
   --  Token_Location.Scan is used to scan the source file.

   Initialized : Boolean := False;

   type Token_Image_Array is array (Token_Type) of Types.Name_Id;
   Token_Image : Token_Image_Array;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
   begin
      return Namet.Get_Name_String (Token_Image (T));
   end Image;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (Source_File : GNAT.OS_Lib.File_Descriptor) is
      --# hide Load_File
      Len  : Integer;
      Code : Integer;

   begin
      --  Load source file in a buffer

      Len := Integer (GNAT.OS_Lib.File_Length (Source_File));
      if Buffer /= null then
         Types.Free (Buffer);
      end if;
      Buffer := new Types.Text_Buffer (1 .. Types.Text_Ptr (Len + 1));

      --  Force the last character to be EOF

      Buffer (Types.Text_Ptr (Len + 1)) := Types.EOF;

      Token_Location.Scan := 1;
      loop
         Code :=
           GNAT.OS_Lib.Read
             (Source_File,
              Buffer (Token_Location.Scan)'Address,
              Len);
         exit when Code = Len;
         if Code <= 0 then
            Errors.DE ("cannot read preprocessor output");
            GNAT.OS_Lib.OS_Exit (4);
         end if;
         Token_Location.Scan := Token_Location.Scan + Types.Text_Ptr (Code);
         Len                 := Len - Code;
      end loop;
      GNAT.OS_Lib.Close (Source_File);

   end Load_File;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Token_Location.Scan      := Token_Location.Scan + 1;
      Token_Location.First_Pos := Token_Location.Scan;
      Token_Location.Last_Pos  := Token_Location.Scan;
      Token_Location.Line      := Token_Location.Line + 1;
   end New_Line;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token (Token : Token_Type; Image : String) is
   begin
      Namet.Set_Str_To_Name_Buffer (Image);
      Token_Image (Token) := Namet.Name_Find;

      if Token in Keyword_Type then
         for I in Natural range 1 .. Namet.Name_Len loop
            Namet.Name_Buffer (I) := Charset.To_Lower (Namet.Name_Buffer (I));
         end loop;
         Namet.Set_Name_Table_Byte
           (Namet.Name_Find,
            Types.Byte (Token_Type'Pos (Token)));
      end if;
   end New_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token return Token_Type is
      Current_Token_Name     : Types.Name_Id;
      Current_Token          : Token_Type;
      Current_Token_Location : Locations.Location;
      Next_Token_Value       : Token_Type;
   begin
      Current_Token_Name     := Token_Name;
      Current_Token          := Token;
      Current_Token_Location := Token_Location;
      Scan_Token (Fatal => False);
      Next_Token_Value := Token;
      Token_Name       := Current_Token_Name;
      Token            := Current_Token;
      Token_Location   := Current_Token_Location;
      return Next_Token_Value;
   end Next_Token;

   -------------
   -- Process --
   -------------

   procedure Process
     (Source_File : GNAT.OS_Lib.File_Descriptor;
      Source_Name : Types.Name_Id)
   is
   begin

      if not Initialized then
         Initialized := True;

         --  Enter all the alphabetic keywords in the name table

         New_Token (T_Error, "<error>");
         New_Token (T_Attribute, "attribute");
         New_Token (T_Boolean, "boolean");
         New_Token (T_Interface, "interface");
         New_Token (T_Long, "long");
         New_Token (T_Module, "module");
         New_Token (T_Octet, "octet");
         New_Token (T_Short, "short");
         New_Token (T_Typedef, "typedef");
         New_Token (T_Semi_Colon, ";");
         New_Token (T_Left_Brace, "{");
         New_Token (T_Right_Brace, "}");
         New_Token (T_Colon, ":");
         New_Token (T_Comma, ",");
         New_Token (T_Colon_Colon, "::");
         New_Token (T_Left_Paren, "(");
         New_Token (T_Right_Paren, ")");
         New_Token (T_Left_Bracket, "[");
         New_Token (T_Right_Bracket, "]");
         New_Token (T_Identifier, "<identifier>");
         New_Token (T_EOF, "<end of file>");
      end if;

      --  Load source file in a buffer

      Load_File (Source_File);

      --  Reset at the beginning

      Token_Location.Scan      := 1;
      Token_Location.First_Pos := 1;
      Token_Location.Last_Pos  := 1;
      Token_Location.Line      := 1;
      Token_Location.Base_Name := Source_Name;

   end Process;

   ------------------
   -- Quoted_Image --
   ------------------

   function Quoted_Image (T : Token_Type) return String is
   begin
      if T in T_Interface .. T_Octet then
         Namet.Set_Char_To_Name_Buffer ('"');
         Namet.Add_Str_To_Name_Buffer
           (Namet.Get_Name_String (Token_Image (T)));
         Namet.Add_Char_To_Name_Buffer ('"');
         return Namet.Get_Name_String (Namet.Name_Find);
      end if;
      return Namet.Get_Name_String (Token_Image (T));
   end Quoted_Image;

   -------------------
   -- Restore_Lexer --
   -------------------

   procedure Restore_Lexer (State : Locations.Location) is
   begin
      Token_Location := State;
   end Restore_Lexer;

   ----------------
   -- Save_Lexer --
   ----------------

   procedure Save_Lexer (State : out Locations.Location) is
   begin
      State := Token_Location;
   end Save_Lexer;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   procedure Scan_Identifier (Fatal : Boolean) is
      Escaped : Boolean := False;
   begin

      --  Read escaped identifier

      if Buffer (Token_Location.Scan) = '_' then
         Escaped             := True;
         Token_Location.Scan := Token_Location.Scan + 1;
      end if;

      --  Read identifier

      Namet.Name_Len := 0;
      while Charset.Is_Identifier_Character (Buffer (Token_Location.Scan)) loop
         Namet.Name_Len                     := Namet.Name_Len + 1;
         Namet.Name_Buffer (Namet.Name_Len) := Buffer (Token_Location.Scan);
         Token_Location.Scan                := Token_Location.Scan + 1;
      end loop;

      if Namet.Name_Len = 0 then
         if Fatal then
            Errors.Error_Loc (1) := Token_Location;
            Errors.DE ("identifier must start with alphabetic character");
         end if;
         Namet.Name_Buffer (1) := ' ';
         Namet.Name_Len        := 1;

      else
         Token_Name := Namet.Name_Find;
         Token      := T_Identifier;

         --  Check whether it is a keyword or a pragma

         if not Escaped then
            Token := To_Token (Token_Name);
            if Token = T_Error then
               Token := T_Identifier;
            end if;
         end if;

         --  Check that identifier is well-formed

         if Token = T_Identifier then
            if not Charset.Is_Alphabetic_Character (Namet.Name_Buffer (1)) then
               if Escaped then
                  if Fatal then
                     Errors.Error_Loc (1) := Token_Location;
                     Errors.DE ("incorrect escaped identifier");
                  end if;

               else
                  if Fatal then
                     Errors.Error_Loc (1) := Token_Location;
                     Errors.DE
                       ("identifier must start with alphabetic character");
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Scan_Identifier;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (T : Token_Type) is
      Loc : Locations.Location := Token_Location;
   begin
      Scan_Token;
      if T /= Token then
         if T = T_Semi_Colon then
            Loc.Last_Pos := Loc.Scan;
            if Buffer (Loc.Last_Pos) = ASCII.LF
              or else Buffer (Loc.Last_Pos) = ASCII.FF
              or else Buffer (Loc.Last_Pos) = ASCII.CR
              or else Buffer (Loc.Last_Pos) = ASCII.VT
            then
               Loc.Last_Pos := Loc.Last_Pos - 1;
            end if;
         else
            Loc := Token_Location;
         end if;

         Errors.Error_Loc (1) := Loc;
         Errors.DE ("expected token " & Quoted_Image (T));
         Token := T_Error;
      end if;
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (L : Token_List_Type) is
      Found : Boolean := False;
   begin
      pragma Assert (L'Length > 1);
      Scan_Token;
      for Index in Natural range L'First .. L'Last loop
         if L (Index) = Token then
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Namet.Set_Str_To_Name_Buffer ("expected token");
         if L'Length > 1 then
            Namet.Add_Char_To_Name_Buffer ('s');
         end if;
         Namet.Add_Char_To_Name_Buffer (' ');
         Namet.Add_Str_To_Name_Buffer (Quoted_Image (L (L'First)));
         for Index in Natural range L'First + 1 .. L'Last loop
            Namet.Add_Str_To_Name_Buffer (" or ");
            Namet.Add_Str_To_Name_Buffer (Quoted_Image (L (Index)));
         end loop;

         Errors.Error_Loc (1) := Token_Location;
         Errors.DE (Namet.Get_Name_String (Namet.Name_Find));

         Token := T_Error;
      end if;
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token is
   begin
      Scan_Token (Fatal => True);
   end Scan_Token;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token (Fatal : Boolean) is
   begin
      if Token /= T_EOF then
         Token := T_Error;
         while Token = T_Error loop
            Skip_Spaces;

            Token_Location.Last_Pos := Token_Location.Scan;

            case Buffer (Token_Location.Scan) is
               when ASCII.LF | ASCII.FF | ASCII.CR | ASCII.VT =>
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
                  if Buffer (Token_Location.Scan) = ':' then
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token               := T_Colon_Colon;
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

               when '/' =>

                  --  Comment like //

                  if Buffer (Token_Location.Scan + 1) = '/' then
                     Skip_Line;

                  --  Comment like /* ... */ (not nested)

                  elsif Buffer (Token_Location.Scan + 1) = '*' then
                     Token_Location.Scan := Token_Location.Scan + 2;
                     while Buffer (Token_Location.Scan) /= Types.EOF
                       and then
                       (Buffer (Token_Location.Scan) /= '*'
                        or else Buffer (Token_Location.Scan + 1) /= '/')
                     loop
                        case Buffer (Token_Location.Scan) is
                           when ASCII.LF | ASCII.FF | ASCII.CR | ASCII.VT =>
                              New_Line;
                           when others =>
                              Token_Location.Scan := Token_Location.Scan + 1;
                        end case;
                     end loop;

                     if Buffer (Token_Location.Scan) = Types.EOF then
                        Errors.Error_Loc (1) := Token_Location;
                        Errors.DE ("unterminated comment");
                     end if;

                     --  Skip char sequence */
                     Token_Location.Scan := Token_Location.Scan + 2;

                  else
                     Errors.Error_Loc (1) := Token_Location;
                     Errors.DE ("invalid character");
                     Token_Location.Scan := Token_Location.Scan + 1;
                     Token               := T_Error;
                  end if;

               when '_' =>
                  Scan_Identifier (Fatal);

               when Types.EOF =>
                  Token_Location.Scan := Token_Location.Scan + 1;
                  Token               := T_EOF;

               when others =>
                  if Charset.Is_Alphabetic_Character
                      (Buffer (Token_Location.Scan))
                  then
                     Scan_Identifier (Fatal);

                  else
                     Errors.Error_Loc (1) := Token_Location;
                     Errors.DE ("invalid character");

                     --  Try to rescue parser and find the beginning of a
                     --  potential token

                     Token_Location.Scan := Token_Location.Scan + 1;
                     while Charset.Is_Alphabetic_Character
                         (Buffer (Token_Location.Scan))
                       or else Buffer (Token_Location.Scan) = '_'
                     loop
                        Token_Location.Scan := Token_Location.Scan + 1;
                     end loop;
                  end if;
            end case;
         end loop;
      end if;
   end Scan_Token;

   ----------------------
   -- Skip_Declaration --
   ----------------------

   procedure Skip_Declaration (Delimiter : Token_Type) is
      Braces : Integer := 0;
      State  : Locations.Location;
   begin
      loop
         Save_Lexer (State);
         Scan_Token (False);

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
            when ASCII.LF | ASCII.FF | ASCII.CR | ASCII.VT =>
               New_Line;
               exit;
            when others =>
               null;
         end case;
         Token_Location.Scan := Token_Location.Scan + 1;
      end loop;
   end Skip_Line;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
   begin
      loop
         case Buffer (Token_Location.Scan) is
            when ' ' | ASCII.HT =>
               Token_Location.Scan := Token_Location.Scan + 1;
            when ASCII.LF | ASCII.FF | ASCII.CR | ASCII.VT =>
               New_Line;
            when others =>
               exit;
         end case;
      end loop;
   end Skip_Spaces;

   --------------
   -- To_Token --
   --------------

   function To_Token (Name : Types.Name_Id) return Token_Type is
      B : Types.Byte;
   begin
      Namet.Get_Name_String (Name);
      for I in Natural range 1 .. Namet.Name_Len loop
         Namet.Name_Buffer (I) := Charset.To_Lower (Namet.Name_Buffer (I));
      end loop;
      B := Namet.Get_Name_Table_Byte (Namet.Name_Find);
      if B <= Last_Token_Pos then
         return Token_Type'Val (B);
      end if;
      return T_Error;
   end To_Token;

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Output.Write_Str (Image (T));
   end Write;

end Lexer;
