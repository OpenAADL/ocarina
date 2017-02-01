------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L _ E M A . P A R S E R _ E R R O R S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2015-2016 ESA & ISAE.                    --
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
with Ocarina.Output;
with GNAT.OS_Lib;
with Ocarina.FE_AADL_EMA.Lexer;

package body Ocarina.FE_AADL_EMA.Parser_Errors is

   use Ocarina.Output;
   use GNAT.OS_Lib;
   use Ocarina.FE_AADL_EMA.Lexer;
   use Locations;
   use Charset;

   procedure Display_Parsing_Code
     (Code    : Parsing_Code;
      Fatal   : Boolean;
      Warning : Boolean);
   pragma Inline (Display_Parsing_Code);
   --  Display corresponding string of given parsing code

   --------------------------
   -- Display_Parsing_Code --
   --------------------------

   procedure Display_Parsing_Error
   is
   begin
      Write_Str ("Frontends : fatal error : misplaced annex");
   end Display_Parsing_Error;

   --------------------------
   -- Display_Parsing_Code --
   --------------------------

   procedure Display_Parsing_Code
     (Code    : Parsing_Code;
      Fatal   : Boolean;
      Warning : Boolean)
   is
   begin
      Write_Str ("Frontends : ");

      if Fatal then
         Write_Str ("fatal ");
      end if;

      if Warning then
         Write_Str ("warning : ");
      else
         Write_Str ("error : ");
      end if;

      Write_Str (Image (Token_Location));
      Write_Str (": parsing ");
      Write_Str (Image (Code));
      Write_Str (", ");
   end Display_Parsing_Code;

   --------------------------------------
   -- Display_Parsing_Error_Unexpected --
   --------------------------------------

   procedure Display_Parsing_Error_Unexpected
     (Code : Parsing_Code)
   is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code, False, False);

      Write_Str ("unexpected ");
      Write_Line (Current_Token_Image);

      Set_Standard_Output;
   end Display_Parsing_Error_Unexpected;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code             : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code;
      Expected_Token   : Token_Type)
   is
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, False, False);

      Write_Str ("node ");
      Write_Str (Image (Code_Expected_1));
      Write_Str (" or node ");
      Write_Str (Image (Code_Expected_2));

      if Expected_Token = T_Identifier then
         Write_Str (" or an identifier");
         --  Write_Name (Identifier)
         --  print here name of identifier
      else
         Write_Str (" or token ");
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
     (Code           : Parsing_Code;
      Code_Expected  : Parsing_Code)
   is
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, False, False);

      Write_Str ("missing node ");
      Write_Line (Image (Code_Expected));

      Set_Standard_Output;
   end Display_Parsing_Error;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Code_Expected   : Parsing_Code;
      Expected_Tokens : Token_List_Type)
   is
      Index : Integer;
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, False, False);

      Write_Str ("node ");
      Write_Str (Image (Code_Expected));
      Write_Line (" or ");

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
     (Code           : Parsing_Code;
      Code_Expected  : Parsing_Code;
      Expected_Token : Token_Type)
   is
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, False, False);

      Write_Str ("node ");
      Write_Str (Image (Code_Expected));

      if Expected_Token = T_Identifier then
         Write_Str (" or an identifier");
         --  Write_Name (Identifier)
         --  print here name of identifier
      else
         Write_Str (" or token ");
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
     (Code             : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code;
      Expected_Tokens  : Token_List_Type)
   is
      Index : Integer;
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, False, False);

      Write_Str ("node ");
      Write_Str (Image (Code_Expected_1));
      Write_Str (" or node ");
      Write_Str (Image (Code_Expected_2));
      Write_Line (" or ");

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
     (Code             : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code)
   is
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, False, False);

      Write_Str ("missing node ");
      Write_Str (Image (Code_Expected_1));
      Write_Str (" or node ");
      Write_Line (Image (Code_Expected_2));

      Set_Standard_Output;
   end Display_Parsing_Error;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code             : Parsing_Code;
      Code_Expected_1  : Parsing_Code;
      Code_Expected_2  : Parsing_Code;
      Code_Expected_3  : Parsing_Code)
   is
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, False, False);

      Write_Str ("missing node ");
      Write_Str (Image (Code_Expected_1));
      Write_Str (" or node ");
      Write_Str (Image (Code_Expected_2));
      Write_Str (" or node ");
      Write_Line (Image (Code_Expected_3));

      Set_Standard_Output;
   end Display_Parsing_Error;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code      : Parsing_Code;
      Error_Msg : Error_Message_Code;
      Msg_Add   : String;
      Fatal     : Boolean            := False;
      Warning   : Boolean            := False)
   is
   begin
      Set_Standard_Error;
      Display_Parsing_Code (Code, Fatal, Warning);
      Write_Line (Image (Error_Msg) & " " & Msg_Add);

      if Fatal then
         OS_Exit (2);
      end if;

      Set_Standard_Output;
   end Display_Parsing_Error;

   --------------------------
   -- Display_Parsing_Code --
   --------------------------

   procedure Display_Parsing_Error
     (Code           : Parsing_Code;
      Expected_Token : Token_Type;
      Fatal          : Boolean       := False;
      Warning        : Boolean       := False)
   is
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, Fatal, Warning);

      if Expected_Token = T_Identifier then
         Write_Str ("an identifier");
         --  Write_Name (Identifier)
         --  print here name of identifier
      elsif Expected_Token = T_Property_Identifier then
         Write_Str ("a property identifier");
      elsif Expected_Token = T_Mode_Name then
         Write_Str ("a mode name");
      elsif Expected_Token = T_Error_Behavior_State_Id then
         Write_Str ("an error_behavior_state identifier");
      elsif Expected_Token = T_Composite_State_Id then
         Write_Str ("a composite_state identifier");
      elsif Expected_Token = T_Propagation_Point_Id then
         Write_Str ("a propagation_point identifier");
      elsif Expected_Token = T_Property_Name then
         Write_Str ("a property name");
      elsif Expected_Token = T_Target_Error_State_Id then
         Write_Str ("a target_error_state identifier");
      else
         Write_Str ("token ");
         Write_Str (Quoted_Image (Expected_Token));
      end if;

      Write_Str (" is expected, found ");
      Write_Line (Current_Token_Image);

      if Fatal then
         OS_Exit (2);
      end if;

      Set_Standard_Output;
   end Display_Parsing_Error;

   ---------------------------
   -- Display_Parsing_Error --
   ---------------------------

   procedure Display_Parsing_Error
     (Code            : Parsing_Code;
      Expected_Tokens : Token_List_Type;
      Fatal           : Boolean            := False;
      Warning         : Boolean            := False)
   is
      Index : Integer;
   begin
      Set_Standard_Error;

      Display_Parsing_Code (Code, Fatal, Warning);

      Index := Expected_Tokens'First;

      while Index < Expected_Tokens'Last loop
         Write_Str (Quoted_Image (Expected_Tokens (Index)));
         Write_Str (" or ");
         Index := Index + 1;
      end loop;

      Write_Str (Quoted_Image (Expected_Tokens (Index)));
      Write_Str (" is expected, found ");
      Write_Line (Current_Token_Image);

      if Fatal then
         OS_Exit (2);
      end if;

      Set_Standard_Output;
   end Display_Parsing_Error;

   -----------
   -- Image --
   -----------

   function Image (Code : Parsing_Code) return String is
      S       : String := Parsing_Code'Image (Code);
      Capital : Boolean := False;
   begin
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
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Code : Error_Message_Code) return String is
      S : String := Error_Message_Code'Image (Code);
   begin
      case Code is
         when EMC_List_Is_Empty =>
            return "the list parsed is empty";

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

end Ocarina.FE_AADL_EMA.Parser_Errors;
