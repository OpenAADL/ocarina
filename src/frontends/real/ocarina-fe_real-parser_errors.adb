------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . F E _ R E A L . P A R S E R _ E R R O R S         --
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

with Locations;
with Charset;
with Ocarina.Output;
with Ada.Characters.Handling;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Ocarina.FE_REAL.Parser_Errors is

   use Ocarina.Output;
   use Locations;
   use Charset;

   procedure Display_Parsing_Code (Code : Parsing_Code);
   pragma Inline (Display_Parsing_Code);
   --  Display corresponding string of given parsing code

   --------------------------
   -- Display_Parsing_Code --
   --------------------------

   procedure Display_Parsing_Code (Code : Parsing_Code) is
   begin
      Write_Str (Image (Token_Location));
      Write_Str (": parsing ");
      Write_Str (Image (Code));
      Write_Str (", ");
   end Display_Parsing_Code;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error (Code : Parsing_Code) is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);

      Write_Str ("unexpected ");
      Write_Line (Image (Token));

      Set_Standard_Output;
   end Display_Parsing_Error;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code           : Parsing_Code;
      Expected_Token : Token_Type)
   is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);

      if Expected_Token = T_Identifier then
         Write_Str ("an identifier");
      else
         Write_Str ("token ");
         Write_Str (Quoted_Image (Expected_Token));
      end if;

      Write_Str (" is expected, found ");
      Write_Line (Image (Token));

      Set_Standard_Output;
   end Display_Parsing_Error;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code      : Parsing_Code;
      Error_Msg : Error_Message_Code)
   is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);
      Write_Line (Image (Error_Msg));
      Set_Standard_Output;
   end Display_Parsing_Error;

   -----------
   -- Image --
   -----------

   function Image (Code : Parsing_Code) return String is
      S       : String  := Parsing_Code'Image (Code);
      Capital : Boolean := False;
   begin
      case Code is

         when others =>
            To_Lower (S);
            for I in S'Range loop
               if S (I) = '_' then
                  Capital := True;
               else
                  if Capital then
                     S (I) := Ada.Characters.Handling.To_Upper (S (I));
                  end if;
                  Capital := False;
               end if;
            end loop;
            return S (4 .. S'Last);
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Code : Error_Message_Code) return String is
      S : String := Error_Message_Code'Image (Code);
   begin
      case Code is

         when EMC_Subtheorem_Parameter =>
            return "First sub-theorem parameter must be either " &
              "a set or a variable";

         when EMC_Variable_Declaration =>
            return "expected 'var' or 'global'";

         when EMC_Wrong_List_Connector =>
            return "wrong list connector, expected 'and' or <left " &
              "parenthesis>";

         when EMC_Declaration_Parameter =>
            return "expected a valid declaration parameter (either " &
              "var/global or an identifier), or testing keyword (either " &
              "return or check)";

         when EMC_Used_Keyword =>
            return "used a keyword instead of a valid identifier " &
              "or function name";

         when EMC_Testing_Token_Expected =>
            return "expected testing expression (either 'return' " &
              "or 'check' tokens";

         when others =>
            To_Lower (S);

            for I in S'Range loop
               if S (I) = '_' then
                  S (I) := ' ';
               end if;
            end loop;

            return S (5 .. S'Last);
      end case;
   end Image;

   ----------------------
   -- Display_And_Exit --
   ----------------------

   procedure Display_And_Exit (Str : String) is
   begin
      Write_Line (Str);
      OS_Exit (1);
   end Display_And_Exit;

end Ocarina.FE_REAL.Parser_Errors;
