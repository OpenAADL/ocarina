------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                       O C A R I N A . O U T P U T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 1992-2009 Telecom ParisTech, 2010-2016 ESA & ISAE.      --
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

pragma License (Gpl);

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Ocarina.Output is

   Current_FD : File_Descriptor := Standout;
   --  File descriptor for current output

   -------------------------
   -- Line Buffer Control --
   -------------------------

   --  Note: the following buffer and column position are maintained by
   --  the subprograms defined in this package, and are not normally
   --  directly modified or accessed by a client. However, a client is
   --  permitted to modify these values, using the knowledge that only
   --  Write_Eol actually generates any output.

   Buffer_Max : constant := 8192;
   Buffer     : String (1 .. Buffer_Max + 1);
   --  Buffer used to build output line. We do line buffering because it
   --  is needed for the support of the debug-generated-code option (-gnatD).
   --  Historically it was first added because on VMS, line buffering is
   --  needed with certain file formats. So in any case line buffering must
   --  be retained for this purpose, even if other reasons disappear. Note
   --  any attempt to write more output to a line than can fit in the buffer
   --  will be silently ignored.

   Next_Column : Pos range 1 .. Buffer'Length + 1 := 1;
   --  Column about to be written.

   -----------------------
   -- Local_Subprograms --
   -----------------------

   procedure Flush_Buffer;
   --  Flush buffer if non-empty and reset column counter

   ------------
   -- Column --
   ------------

   function Column return Nat is
   begin
      return Next_Column;
   end Column;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output return GNAT.OS_Lib.File_Descriptor is
   begin
      return Current_FD;
   end Current_Output;

   ---------------------------
   -- Decrement_Indentation --
   ---------------------------

   procedure Decrement_Indentation is
   begin
      N_Space := N_Space - Space_Increment;
   end Decrement_Indentation;

   ------------------
   -- Flush_Buffer --
   ------------------

   procedure Flush_Buffer is
      Len : constant Natural := Natural (Next_Column - 1);

   begin
      if Len /= 0 then
         if Len /= Write (Current_FD, Buffer'Address, Len) then

            --  If there are errors with standard error, just quit

            if Current_FD = Standerr then
               OS_Exit (2);

            --  Otherwise, set the output to standard error before
            --  reporting a failure and quitting.

            else
               Current_FD  := Standerr;
               Next_Column := 1;
               Write_Line ("fatal error: disk full");
               OS_Exit (2);
            end if;
         end if;

         --  Buffer is now empty

         Next_Column := 1;
      end if;
   end Flush_Buffer;

   ---------------------------
   -- Increment_Indentation --
   ---------------------------

   procedure Increment_Indentation is
   begin
      N_Space := N_Space + Space_Increment;
   end Increment_Indentation;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (New_Output : File_Descriptor) is
   begin
      Flush_Buffer;
      Next_Column := 1;
      Current_FD  := New_Output;
   end Set_Output;

   -------------------------
   -- Set_Space_Increment --
   -------------------------

   procedure Set_Space_Increment (Value : Natural) is
   begin
      Space_Increment := Value;
   end Set_Space_Increment;

   ------------------------
   -- Set_Standard_Error --
   ------------------------

   procedure Set_Standard_Error is
   begin
      Flush_Buffer;
      Next_Column := 1;
      Current_FD  := Standerr;
   end Set_Standard_Error;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   procedure Set_Standard_Output is
   begin
      Flush_Buffer;
      Next_Column := 1;
      Current_FD  := Standout;
   end Set_Standard_Output;

   ----------------
   -- Write_Char --
   ----------------

   procedure Write_Char (C : Character) is
   begin
      if Next_Column < Buffer'Length then
         Buffer (Natural (Next_Column)) := C;
         Next_Column                    := Next_Column + 1;
      end if;
   end Write_Char;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol (N : Natural := 1) is
   begin
      for J in 1 .. N loop
         Buffer (Natural (Next_Column)) := ASCII.LF;
         Next_Column                    := Next_Column + 1;
         Flush_Buffer;
      end loop;
   end Write_Eol;

   -----------------------
   -- Write_Indentation --
   -----------------------

   procedure Write_Indentation (Offset : Integer := 0) is
   begin
      for I in 1 .. N_Space + Offset loop
         Write_Char (' ');
      end loop;
   end Write_Indentation;

   ---------------
   -- Write_Int --
   ---------------

   procedure Write_Int (Val : Int) is
   begin
      if Val < 0 then
         Write_Char ('-');
         Write_Int (-Val);

      else
         if Val > 9 then
            Write_Int (Val / 10);
         end if;

         Write_Char (Character'Val ((Val mod 10) + Character'Pos ('0')));
      end if;
   end Write_Int;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (S : String) is
   begin
      Write_Str (S);
      Write_Eol;
   end Write_Line;

   -----------------
   -- Write_Space --
   -----------------

   procedure Write_Space is
   begin
      Write_Char (' ');
   end Write_Space;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : String) is
   begin
      if Next_Column + S'Length > Buffer'Length then
         --  XXX The following will not work for the Ada generator.
         --  Note this should not be a problem since Buffer'Length is
         --  8192, and Ada code generator does not allow long lines.

         Write_Char ('\');
         Write_Eol;
      end if;

      for J in S'Range loop
         Write_Char (S (J));
      end loop;
   end Write_Str;

   ----------------------
   -- Write_Quoted_Str --
   ----------------------

   procedure Write_Quoted_Str (S : String) is
   begin
      Write_Str ('"' & S & '"');
   end Write_Quoted_Str;

end Ocarina.Output;
