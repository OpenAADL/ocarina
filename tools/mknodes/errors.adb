------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                               E R R O R S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
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

with Ada.Command_Line;          use Ada.Command_Line;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Output;                    use Output;
with Namet;                     use Namet;

package body Errors is

   procedure Internal_Display_Message (S : String);

   procedure Check_Space;
   --  Check there is a trailing space in order to append a string to
   --  name buffer.

   -----------------
   -- Check_Space --
   -----------------

   procedure Check_Space is
   begin
      if Name_Len > 0 and then Name_Buffer (Name_Len) /= ' ' then
         Add_Char_To_Name_Buffer (' ');
      end if;
   end Check_Space;

   ------------------------------
   -- Internal_Display_Message --
   ------------------------------

   procedure Internal_Display_Message (S : String) is
      L : Natural := 1;
      I : Natural := 1;
      N : Natural := 1;
      M : Boolean := False; --  Meta-character
      J : Natural := S'First;

   begin
      if Error_Loc (L) = No_Location then
         Set_Str_To_Name_Buffer
           (Base_Name (Command_Name, Get_Executable_Suffix.all));
      else
         Set_Str_To_Name_Buffer (Image (Error_Loc (L)));
      end if;

      L := L + 1;
      Add_Str_To_Name_Buffer (": ");

      while J <= S'Last loop

         --  Escape meta-character

         if S (J) = '|' then
            if J < S'Last then
               J := J + 1;
            end if;
            Add_Char_To_Name_Buffer (S (J));

         elsif S (J) = '%' then
            Check_Space;
            Get_Name_String_And_Append (Error_Name (N));
            N := N + 1;
            M := True;

         elsif S (J) = '#' then
            Check_Space;
            Add_Char_To_Name_Buffer ('"'); -- "
            Get_Name_String_And_Append (Error_Name (N));
            Add_Char_To_Name_Buffer ('"'); -- "
            N := N + 1;
            M := True;

         elsif S (J) = '!' then
            if Error_Loc (1).Base_Name = Error_Loc (L).Base_Name then
               Check_Space;
               Add_Str_To_Name_Buffer ("at line ");
               Add_Nat_To_Name_Buffer (Error_Loc (L).Line);

            else
               Check_Space;
               Add_Str_To_Name_Buffer ("in ");
               Add_Str_To_Name_Buffer (Image (Error_Loc (L)));
            end if;
            L := L + 1;
            M := True;

         elsif S (J) = '$' then
            Add_Nat_To_Name_Buffer (Error_Int (I));
            I := I + 1;
            M := False;

         else
            if M then
               Add_Char_To_Name_Buffer (' ');
               M := False;
            end if;
            Add_Char_To_Name_Buffer (S (J));
         end if;

         J := J + 1;
      end loop;

      Set_Standard_Error;
      Write_Line (Name_Buffer (1 .. Name_Len));
      Set_Standard_Output;

      Initialize;
   end Internal_Display_Message;

   -------------------
   -- Display_Error --
   -------------------

   procedure Display_Error (S : String) is
   begin
      N_Errors := N_Errors + 1;
      Internal_Display_Message (S);
   end Display_Error;

   ---------------------
   -- Display_Message --
   ---------------------

   procedure Display_Message (S : String) is
   begin
      Internal_Display_Message (S);
   end Display_Message;

   ---------------------
   -- Display_Warning --
   ---------------------

   procedure Display_Warning (S : String) is
   begin
      N_Warnings := N_Warnings + 1;
      Internal_Display_Message ("warning: " & S);
   end Display_Warning;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Error_Loc  := (others => No_Location);
      Error_Name := (others => No_Name);
      Error_Int  := (others => 0);
   end Initialize;

   ---------------------
   -- Display_Bug_Box --
   ---------------------

   procedure Display_Bug_Box (E : Ada.Exceptions.Exception_Occurrence) is
      Exception_String : constant String := "| Detected exception: "
        & Ada.Exceptions.Exception_Name (E);
      Error_String : constant String :=
        "| Error: "
        & Ada.Exceptions.Exception_Message (E);
   begin
      Set_Standard_Error;

      Write_Line ("+========================== OCARINA BUG DETECTED"
                    & " =========================+");

      Write_Str (Exception_String);
      for J in Exception_String'Length .. 72 loop
         Write_Str (" ");
      end loop;
      Write_Str (" |");
      Write_Eol;

      Write_Str (Error_String);
      for J in Error_String'Length .. 72 loop
         Write_Str (" ");
      end loop;
      Write_Str (" |");
      Write_Eol;

      Write_Str ("| Please refer to the User's Guide for more details."
                   & "                      |");
      Write_Eol;
      Write_Line ("+============================================="
                    & "============================+");
      Write_Eol;
      Write_Line (Ada.Exceptions.Exception_Information (E));

      OS_Exit (3);
   end Display_Bug_Box;

   -------------------
   -- Exit_On_Error --
   -------------------

   procedure Exit_On_Error (Error : Boolean; Reason : String) is
   begin
      if Error then
         Set_Standard_Error;
         Write_Line (Reason);
         OS_Exit (1);
      end if;
   end Exit_On_Error;

end Errors;
