------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                             T E S T _ 1 1 1                              --
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

with Ocarina.Types;
with Ocarina.Namet;
with Locations;
with Ocarina.Output;

with Ocarina;
with Ocarina.Configuration;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Lexer;
with Ocarina.FE_AADL.Parser;
with Ocarina.ME_AO4AADL.Tokens;
with Ocarina.FE_AO4AADL.Lexer;
with Ocarina.FE_AO4AADL.Parser;
with Ocarina.ME_AO4AADL.AO4AADL_TREE.Debug;

with Ada.Command_Line;

procedure test_111 is
   use Ocarina.Types;
   use Ocarina.Namet;
   use Locations;
   use Ocarina.Output;
   use Ocarina;
   use Ada.Command_Line;
   use Ocarina.ME_AO4AADL.AO4AADL_TREE.Debug;
   use Ocarina.FE_AO4AADL.Parser;

   package AADL_T renames Ocarina.ME_AADL.Tokens;
   package AADL_L renames Ocarina.FE_AADL.Lexer;
   package AADL_P renames Ocarina.FE_AADL.Parser;
   package AO4AADL_T renames Ocarina.ME_AO4AADL.Tokens;
   package AO4AADL_L renames Ocarina.FE_AO4AADL.Lexer;
   package AO4AADL_P renames Ocarina.FE_AO4AADL.Parser;

   use type AADL_T.Token_Type;
   use type AO4AADL_T.Token_Type;

   AO4AADL_Name    : Name_Id;
   AO4AADL_Present : Boolean := False;

   L : Location;

begin
   if Argument_Count /= 1 then
      Write_Line ("Usage test_000 <ocarina_source_dir>");
      return;
   end if;

   Ocarina.Initialize;
   Default_AADL_Version := AADL_V1;
   AADL_Version         := Default_AADL_Version;
   Ocarina.Configuration.Init_Modules;

   AO4AADL_Name := Get_String_Name ("ao4aadl");

   declare
      AADL_File  : constant String
        := Argument (1) & "/tests/ao4aadl/Test.aadl";
   begin
      L := AADL_L.Load_File (Get_String_Name (AADL_File));
   end;

   --  We skip the AADL tokens until we find the AO4AADL annex

   AADL_L.Restore_Lexer (L);

   AADL_L.Scan_Token;

   while  AADL_L.Token /= AADL_T.T_EOF loop
      --  We exit the loop if the current token is T_Annex and the
      --  next is AO4AADL

      if AADL_L.Token = AADL_T.T_Annex then
         declare
            S : Location;
         begin
            AADL_L.Save_Lexer (S);
            AADL_L.Scan_Token;

            if AADL_L.Token = AADL_T.T_Identifier
              and then AADL_L.Token_Name = AO4AADL_Name
            then
               AO4AADL_Present := True;
               exit;
            end if;

            AADL_L.Restore_Lexer (S);
         end;
      end if;

      AADL_L.Scan_Token;
   end loop;

   if AO4AADL_Present then

      --  Skip {**
      AADL_L.Scan_Token;

      L := AADL_L.Token_Location;

      AO4AADL_L.Restore_Lexer (L);

      W_Node_Id (AO4AADL_P.Process(No_Node, L));
   end if;

   Ocarina.Configuration.Reset_Modules;
   Ocarina.Reset;
end test_111;
