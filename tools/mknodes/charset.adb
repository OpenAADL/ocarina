------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                              C H A R S E T                               --
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Charset is

   type Translate_Table is array (Character) of Character;
   --  Type used to describe translate tables

   Fold_Lower : constant Translate_Table :=
     (
      'A'                      => LC_A,
      'B'                      => LC_B,
      'C'                      => LC_C,
      'D'                      => LC_D,
      'E'                      => LC_E,
      'F'                      => LC_F,
      'G'                      => LC_G,
      'H'                      => LC_H,
      'I'                      => LC_I,
      'J'                      => LC_J,
      'K'                      => LC_K,
      'L'                      => LC_L,
      'M'                      => LC_M,
      'N'                      => LC_N,
      'O'                      => LC_O,
      'P'                      => LC_P,
      'Q'                      => LC_Q,
      'R'                      => LC_R,
      'S'                      => LC_S,
      'T'                      => LC_T,
      'U'                      => LC_U,
      'V'                      => LC_V,
      'W'                      => LC_W,
      'X'                      => LC_X,
      'Y'                      => LC_Y,
      'Z'                      => LC_Z,
      LC_A                     => LC_A,
      LC_B                     => LC_B,
      LC_C                     => LC_C,
      LC_D                     => LC_D,
      LC_E                     => LC_E,
      LC_F                     => LC_F,
      LC_G                     => LC_G,
      LC_H                     => LC_H,
      LC_I                     => LC_I,
      LC_J                     => LC_J,
      LC_K                     => LC_K,
      LC_L                     => LC_L,
      LC_M                     => LC_M,
      LC_N                     => LC_N,
      LC_O                     => LC_O,
      LC_P                     => LC_P,
      LC_Q                     => LC_Q,
      LC_R                     => LC_R,
      LC_S                     => LC_S,
      LC_T                     => LC_T,
      LC_U                     => LC_U,
      LC_V                     => LC_V,
      LC_W                     => LC_W,
      LC_X                     => LC_X,
      LC_Y                     => LC_Y,
      LC_Z                     => LC_Z,
      UC_A_Grave               => LC_A_Grave,
      UC_A_Acute               => LC_A_Acute,
      UC_A_Circumflex          => LC_A_Circumflex,
      UC_A_Tilde               => LC_A_Tilde,
      UC_A_Diaeresis           => LC_A_Diaeresis,
      UC_A_Ring                => LC_A_Ring,
      UC_AE_Diphthong          => LC_AE_Diphthong,
      UC_C_Cedilla             => LC_C_Cedilla,
      UC_E_Grave               => LC_E_Grave,
      UC_E_Acute               => LC_E_Acute,
      UC_E_Circumflex          => LC_E_Circumflex,
      UC_E_Diaeresis           => LC_E_Diaeresis,
      UC_I_Grave               => LC_I_Grave,
      UC_I_Acute               => LC_I_Acute,
      UC_I_Circumflex          => LC_I_Circumflex,
      UC_I_Diaeresis           => LC_I_Diaeresis,
      UC_Icelandic_Eth         => LC_Icelandic_Eth,
      UC_N_Tilde               => LC_N_Tilde,
      UC_O_Grave               => LC_O_Grave,
      UC_O_Acute               => LC_O_Acute,
      UC_O_Circumflex          => LC_O_Circumflex,
      UC_O_Tilde               => LC_O_Tilde,
      UC_O_Diaeresis           => LC_O_Diaeresis,
      UC_O_Oblique_Stroke      => LC_O_Oblique_Stroke,
      UC_U_Grave               => LC_U_Grave,
      UC_U_Acute               => LC_U_Acute,
      UC_U_Circumflex          => LC_U_Circumflex,
      UC_U_Diaeresis           => LC_U_Diaeresis,
      UC_Y_Acute               => LC_Y_Acute,
      UC_Icelandic_Thorn       => LC_Icelandic_Thorn,
      LC_German_Sharp_S        => LC_German_Sharp_S,
      LC_A_Grave               => LC_A_Grave,
      LC_A_Acute               => LC_A_Acute,
      LC_A_Circumflex          => LC_A_Circumflex,
      LC_A_Tilde               => LC_A_Tilde,
      LC_A_Diaeresis           => LC_A_Diaeresis,
      LC_A_Ring                => LC_A_Ring,
      LC_AE_Diphthong          => LC_AE_Diphthong,
      LC_C_Cedilla             => LC_C_Cedilla,
      LC_E_Grave               => LC_E_Grave,
      LC_E_Acute               => LC_E_Acute,
      LC_E_Circumflex          => LC_E_Circumflex,
      LC_E_Diaeresis           => LC_E_Diaeresis,
      LC_I_Grave               => LC_I_Grave,
      LC_I_Acute               => LC_I_Acute,
      LC_I_Circumflex          => LC_I_Circumflex,
      LC_I_Diaeresis           => LC_I_Diaeresis,
      LC_Icelandic_Eth         => LC_Icelandic_Eth,
      LC_N_Tilde               => LC_N_Tilde,
      LC_O_Grave               => LC_O_Grave,
      LC_O_Acute               => LC_O_Acute,
      LC_O_Circumflex          => LC_O_Circumflex,
      LC_O_Tilde               => LC_O_Tilde,
      LC_O_Diaeresis           => LC_O_Diaeresis,
      LC_O_Oblique_Stroke      => LC_O_Oblique_Stroke,
      LC_U_Grave               => LC_U_Grave,
      LC_U_Acute               => LC_U_Acute,
      LC_U_Circumflex          => LC_U_Circumflex,
      LC_U_Diaeresis           => LC_U_Diaeresis,
      LC_Y_Acute               => LC_Y_Acute,
      LC_Icelandic_Thorn       => LC_Icelandic_Thorn,
      LC_Y_Diaeresis           => LC_Y_Diaeresis,
      others                   => ' ');

      Fold_Upper : constant Translate_Table :=
     (
      LC_A                     => 'A',
      LC_B                     => 'B',
      LC_C                     => 'C',
      LC_D                     => 'D',
      LC_E                     => 'E',
      LC_F                     => 'F',
      LC_G                     => 'G',
      LC_H                     => 'H',
      LC_I                     => 'I',
      LC_J                     => 'J',
      LC_K                     => 'K',
      LC_L                     => 'L',
      LC_M                     => 'M',
      LC_N                     => 'N',
      LC_O                     => 'O',
      LC_P                     => 'P',
      LC_Q                     => 'Q',
      LC_R                     => 'R',
      LC_S                     => 'S',
      LC_T                     => 'T',
      LC_U                     => 'U',
      LC_V                     => 'V',
      LC_W                     => 'W',
      LC_X                     => 'X',
      LC_Y                     => 'Y',
      LC_Z                     => 'Z',
      'A'                      => 'A',
      'B'                      => 'B',
      'C'                      => 'C',
      'D'                      => 'D',
      'E'                      => 'E',
      'F'                      => 'F',
      'G'                      => 'G',
      'H'                      => 'H',
      'I'                      => 'I',
      'J'                      => 'J',
      'K'                      => 'K',
      'L'                      => 'L',
      'M'                      => 'M',
      'N'                      => 'N',
      'O'                      => 'O',
      'P'                      => 'P',
      'Q'                      => 'Q',
      'R'                      => 'R',
      'S'                      => 'S',
      'T'                      => 'T',
      'U'                      => 'U',
      'V'                      => 'V',
      'W'                      => 'W',
      'X'                      => 'X',
      'Y'                      => 'Y',
      'Z'                      => 'Z',
      UC_A_Grave               => UC_A_Grave,
      UC_A_Acute               => UC_A_Acute,
      UC_A_Circumflex          => UC_A_Circumflex,
      UC_A_Tilde               => UC_A_Tilde,
      UC_A_Diaeresis           => UC_A_Diaeresis,
      UC_A_Ring                => UC_A_Ring,
      UC_AE_Diphthong          => UC_AE_Diphthong,
      UC_C_Cedilla             => UC_C_Cedilla,
      UC_E_Grave               => UC_E_Grave,
      UC_E_Acute               => UC_E_Acute,
      UC_E_Circumflex          => UC_E_Circumflex,
      UC_E_Diaeresis           => UC_E_Diaeresis,
      UC_I_Grave               => UC_I_Grave,
      UC_I_Acute               => UC_I_Acute,
      UC_I_Circumflex          => UC_I_Circumflex,
      UC_I_Diaeresis           => UC_I_Diaeresis,
      UC_Icelandic_Eth         => UC_Icelandic_Eth,
      UC_N_Tilde               => UC_N_Tilde,
      UC_O_Grave               => UC_O_Grave,
      UC_O_Acute               => UC_O_Acute,
      UC_O_Circumflex          => UC_O_Circumflex,
      UC_O_Tilde               => UC_O_Tilde,
      UC_O_Diaeresis           => UC_O_Diaeresis,
      UC_O_Oblique_Stroke      => UC_O_Oblique_Stroke,
      UC_U_Grave               => UC_U_Grave,
      UC_U_Acute               => UC_U_Acute,
      UC_U_Circumflex          => UC_U_Circumflex,
      UC_U_Diaeresis           => UC_U_Diaeresis,
      UC_Y_Acute               => UC_Y_Acute,
      UC_Icelandic_Thorn       => UC_Icelandic_Thorn,
      LC_German_Sharp_S        => LC_German_Sharp_S,
      LC_A_Grave               => UC_A_Grave,
      LC_A_Acute               => UC_A_Acute,
      LC_A_Circumflex          => UC_A_Circumflex,
      LC_A_Tilde               => UC_A_Tilde,
      LC_A_Diaeresis           => UC_A_Diaeresis,
      LC_A_Ring                => UC_A_Ring,
      LC_AE_Diphthong          => UC_AE_Diphthong,
      LC_C_Cedilla             => UC_C_Cedilla,
      LC_E_Grave               => UC_E_Grave,
      LC_E_Acute               => UC_E_Acute,
      LC_E_Circumflex          => UC_E_Circumflex,
      LC_E_Diaeresis           => UC_E_Diaeresis,
      LC_I_Grave               => UC_I_Grave,
      LC_I_Acute               => UC_I_Acute,
      LC_I_Circumflex          => UC_I_Circumflex,
      LC_I_Diaeresis           => UC_I_Diaeresis,
      LC_Icelandic_Eth         => UC_Icelandic_Eth,
      LC_N_Tilde               => UC_N_Tilde,
      LC_O_Grave               => UC_O_Grave,
      LC_O_Acute               => UC_O_Acute,
      LC_O_Circumflex          => UC_O_Circumflex,
      LC_O_Tilde               => UC_O_Tilde,
      LC_O_Diaeresis           => UC_O_Diaeresis,
      LC_O_Oblique_Stroke      => UC_O_Oblique_Stroke,
      LC_U_Grave               => UC_U_Grave,
      LC_U_Acute               => UC_U_Acute,
      LC_U_Circumflex          => UC_U_Circumflex,
      LC_U_Diaeresis           => UC_U_Diaeresis,
      LC_Y_Acute               => UC_Y_Acute,
      LC_Icelandic_Thorn       => UC_Icelandic_Thorn,
      LC_Y_Diaeresis           => LC_Y_Diaeresis,
      others                   => ' ');

   -----------------------------
   -- Is_Alphabetic_Character --
   -----------------------------

   function Is_Alphabetic_Character (C : Character) return Boolean is
   begin
      return Fold_Lower (C) /= ' ';
   end Is_Alphabetic_Character;

   -----------------------------
   -- Is_Identifier_Character --
   -----------------------------

   function Is_Identifier_Character (C : Character) return Boolean is
   begin
      return C = '_'
        or else C in '0' .. '9'
        or else Fold_Lower (C) /= ' ';
   end Is_Identifier_Character;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      for I in S'Range loop
         if Fold_Lower (S (I)) /= ' ' then
            S (I) := Fold_Lower (S (I));
         end if;
      end loop;
   end To_Lower;

   function To_Lower (C : Character) return Character is
   begin
      if Fold_Lower (C) /= ' ' then
         return Fold_Lower (C);
      else
         return C;
      end if;
   end To_Lower;

   function To_Lower (S : String) return String is
      LS : String := S;
   begin
      To_Lower (LS);
      return LS;
   end To_Lower;

   --------------
   -- To_Upper --
   --------------

   procedure To_Upper (S : in out String) is
   begin
      for I in S'Range loop
         if Fold_Lower (S (I)) /= ' ' then
            S (I) := Fold_Upper (S (I));
         end if;
      end loop;
   end To_Upper;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (C : Character) return Character is
   begin
      if Fold_Lower (C) /= ' ' then
         return Fold_Upper (C);
      else
         return C;
      end if;
   end To_Upper;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (S : String) return String is
      LS : String := S;
   begin
      To_Upper (LS);
      return LS;
   end To_Upper;

end Charset;
