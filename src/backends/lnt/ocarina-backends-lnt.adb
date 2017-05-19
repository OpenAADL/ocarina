------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 O C A R I N A . B A C K E N D S . L N T                  --
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
with Ocarina.Output;
with Utils;
with GNAT.Command_Line; use GNAT.Command_Line;

with Ocarina.Instances;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Utils;
with Ocarina.Backends.LNT.Nodes;
with Ocarina.Backends.LNT.Nutils;
with Ocarina.Backends.LNT.Tree_generator;
with Ocarina.Backends.LNT.SVL_generator;
with Ocarina.Backends.LNT.Tree_generator_Types;
with Ocarina.Backends.LNT.Tree_generator_Thread;
with Ocarina.Backends.LNT.Tree_generator_Processor;
with Ocarina.Backends.LNT.Tree_generator_Port;
with Ocarina.Backends.LNT.Tree_generator_Main;
with Ocarina.Backends.LNT.Printer;
with Ocarina.ME_AADL.AADL_Instances.Nodes;

use Ocarina.Namet;
use Ocarina.Output;
use Utils;
use Ocarina.Instances;
use Ocarina.Backends.Messages;
use Ocarina.Backends.Utils;
use Ocarina.Backends.Expander;

use Ocarina.Backends.LNT.Nutils;
use Ocarina.Backends.LNT.Tree_generator;
use Ocarina.Backends.LNT.SVL_generator;
use Ocarina.Backends.LNT.Tree_generator_Thread;
use Ocarina.Backends.LNT.Tree_generator_Types;
use Ocarina.Backends.LNT.Tree_generator_Processor;
use Ocarina.Backends.LNT.Tree_generator_Port;
use Ocarina.Backends.LNT.Tree_generator_Main;
use Ocarina.Backends.LNT.Printer;
use Ocarina.Backends.LNT.Nodes;

package body Ocarina.Backends.LNT is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   use AIN;

   Generated_Sources_Directory : Name_Id := No_Name;
   --------------
   -- Generate --
   --------------
   procedure Generate (AADL_Root : Types.Node_Id) is
      Instance_Root : Node_Id;
      --  pragma Warnings (Off, LNT_Generated);

   begin
      Write_Line ("------------------------------------------");
      Write_Line ("---------- Ocarina LNT Generator ---------");
      Write_Line ("------------------------------------------");
      Write_Line ("");
      Instance_Root := Instantiate_Model (AADL_Root);
      Expand (Instance_Root);

      if Instance_Root /= No_Node then
         System_Name := To_Upper (
             AIN.Name (
             AIN.Identifier (Namespace (
             Root_System (Instance_Root)))));

         Get_N_Thread (Root_System (Instance_Root),
                       Thread_Number,
                       Not_Periodic_Thread_Number);
         LNT_Thread_Instance_List := New_List;
         LNT_Threads := Generate_LNT_Thread (Instance_Root);
         LNT_Processor := Generate_LNT_Processor (Instance_Root);
         LNT_Types := Generate_LNT_Types;
         LNT_Main := Generate_LNT_Main (Instance_Root);
      end if;
      if No (LNT_Threads) then
         Display_Error ("Code generation failed", Fatal => True);
      end if;
      Enter_Directory (Generated_Sources_Directory);
      --  Print_LNT_Generated (SVL);
      Make_Svl_File;
      Generate_LNT_Port;
      Print_LNT_Generated (LNT_Threads);
      Print_LNT_Generated (LNT_Types);
      Print_LNT_Generated (LNT_Main);
      Print_LNT_Generated (LNT_Processor);
   end Generate;

   ----------
   -- Init --
   ----------
   procedure Init is
   begin
      Generated_Sources_Directory := Get_String_Name (".");
      Initialize_Option_Scan;
      Register_Backend ("lnt", Generate'Access, Lnt_S);

      --  LNT Keywords
      for J in Keyword_Type loop
         New_Token (J);
      end loop;

      --  Graphic Characters
      New_Token (Tok_Double_Asterisk, "**");
      New_Token (Tok_Ampersand, "&");
      New_Token (Tok_Minus, "-");
      New_Token (Tok_Plus, "+");
      New_Token (Tok_Asterisk, "*");
      New_Token (Tok_Slash, "/");
      New_Token (Tok_Dot, ".");
      New_Token (Tok_Apostrophe, "'");
      New_Token (Tok_Left_Paren, "(");
      New_Token (Tok_Right_Paren, ")");
      New_Token (Tok_Comma, ",");
      New_Token (Tok_Less, "<");
      New_Token (Tok_Equal, "=");
      New_Token (Tok_Greater, ">");
      New_Token (Tok_Not_Equal, "/=");
      New_Token (Tok_Greater_Equal, ">=");
      New_Token (Tok_Less_Equal, "<=");
      New_Token (Tok_Box, "<>");
      New_Token (Tok_Colon_Equal, ":=");
      New_Token (Tok_Colon, ":");
      New_Token (Tok_Greater_Greater, ">>");
      New_Token (Tok_Less_Less, "<<");
      New_Token (Tok_Semicolon, ";");
      New_Token (Tok_Arrow, "->");
      New_Token (Tok_Arrow_Double, "=>");
      New_Token (Tok_Vertical_Bar, "|");
      New_Token (Tok_Dot_Dot, "..");
      New_Token (Tok_Minus_Minus, "--");

   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ocarina.Backends.LNT.Nodes.Entries.Init;
   end Reset;

end Ocarina.Backends.LNT;
