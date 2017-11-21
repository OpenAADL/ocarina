------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . F E _ A A D L . P A R S E R _ E R R O R S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2016 ESA & ISAE.      --
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

with Ada.Characters.Handling;

with Locations;
with Charset;
with Ocarina.Namet;
with Ocarina.Output;
with Ocarina.FE_AADL.Lexer;

package body Ocarina.FE_AADL.Parser_Errors is

   use Ocarina.Output;
   use Ocarina.FE_AADL.Lexer;
   use Locations;
   use Ocarina.Namet;
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
      Write_Line (Current_Token_Image);

      Set_Standard_Output;
   end Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code       : Parsing_Code;
      Identifier : Name_Id)
   is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);

      Write_Str ("identifier '");
      Write_Name (Identifier);
      Write_Str ("' is expected, found ");
      Write_Line (Current_Token_Image);

      Set_Standard_Output;
   end Display_Parsing_Error;

   procedure Display_Parsing_Error
     (Code          : Parsing_Code;
      Error_Msg     : Error_Message_Code;
      Current_Token : Token_Type)
   is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);

      Write_Str ("token ");
      Write_Str (Quoted_Image (Current_Token));

      Write_Line (Image (Error_Msg));

      Set_Standard_Output;
   end Display_Parsing_Error;

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
      Write_Line (Current_Token_Image);

      Set_Standard_Output;
   end Display_Parsing_Error;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Expected_Tokens : Token_List_Type)
   is
      Index : Integer;
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code);
      Write_Str ("token ");

      Index := Expected_Tokens'First;

      while Index < Expected_Tokens'Last loop
         Write_Str (Quoted_Image (Expected_Tokens (Index)));
         Write_Str (" or ");
         Index := Index + 1;
      end loop;

      Write_Str (Quoted_Image (Expected_Tokens (Index)));
      Write_Str (" is expected, found ");
      Write_Line (Current_Token_Image);

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
         when PC_AADL_Declaration =>
            return "AADL_Declaration";

         when PC_AADL_Specification =>
            return "AADL_Specification";

         when PC_Items_List =>
            return "list of items";

         when PC_Mode_Or_Mode_Transition =>
            return "Mode or Mode_Transition";

         when PC_Port_Spec_Or_Feature_Group_Spec =>
            return "Port_Spec or Port_Group_Spec or Feature_Group_Spec";

         when PC_Port_Refinement_Or_Feature_Group_Refinement =>
            return "Port_Refinement," &
              "Port_Group_Refinement,Feature_Group Refinement";

         when PC_Property_Association_Or_Contained_Property_Association =>
            return "Property_Association or Contained_Property_Association";

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
         when EMC_Access_Property_Association_Is_Not_Allowed =>
            return "Access_Property_Association is not allowed";

         when EMC_Contained_Property_Association_Is_Not_Allowed =>
            return "Contained_Property_Association is not allowed";

         when EMC_Odd_Number_Of_Element_Expected =>
            return "Odd number of elements expected";

         when EMC_At_Least_Tow_Elements_Expected =>
            return "At least 2 elements are expected";

         when EMC_At_Least_Three_Elements_Expected =>
            return "At least 3 elements are expected";

         when EMC_Empty_Package =>
            return "Empty packages are not allowed";

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

end Ocarina.FE_AADL.Parser_Errors;
