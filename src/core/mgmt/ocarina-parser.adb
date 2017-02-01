------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                       O C A R I N A . P A R S E R                        --
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

with Charset; use Charset;
with Errors;  use Errors;
with Ocarina.Namet;   use Ocarina.Namet;
with Ocarina.Output;  use Ocarina.Output;

with Ocarina.Options;       use Ocarina.Options;
with Ocarina.Property_Sets; use Ocarina.Property_Sets;

with GNAT.Table;

package body Ocarina.Parser is

   type Parser_Record is record
      Language : Name_Id;
      Parser   : Parser_Subprogram;
   end record;

   package Parsers is new GNAT.Table (Parser_Record, Nat, 1, 5, 20);

   -----------
   -- Parse --
   -----------

   function Parse
     (Language  : Name_Id;
      AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location;
      Container : Node_Id  := No_Node) return Node_Id
   is
   begin
      for P in Parsers.First .. Parsers.Last loop
         if Language = Parsers.Table (P).Language then
            return Parsers.Table (P).Parser (AADL_Root, From, To, Container);
         end if;
      end loop;

      Error_Loc (1)  := From;
      Error_Name (1) := Language;
      Display_Warning ("no support provided for annex language %");

      return AADL_Root;
   end Parse;

   ------------------
   -- Init_Parsers --
   ------------------

   procedure Init_Parsers is
   begin
      Ocarina.Property_Sets.Initialize;
   end Init_Parsers;

   ---------------------
   -- Register_Parser --
   ---------------------

   procedure Register_Parser (Language : String; Parser : Parser_Subprogram) is
      N : Name_Id;
   begin
      N := Get_String_Name (To_Lower (Language));
      for B in Parsers.First .. Parsers.Last loop
         if Parsers.Table (B).Language = N then
            Set_Standard_Error;
            Write_Line ("Cannot register twice parser " & Language);
            Set_Standard_Output;
         end if;
      end loop;

      if Language'Length > 0 then
         Parsers.Increment_Last;
         Set_Str_To_Name_Buffer (Language);
         Parsers.Table (Parsers.Last).Language := Name_Find;
         Parsers.Table (Parsers.Last).Parser   := Parser;
      end if;
   end Register_Parser;

   -------------------
   -- Reset_Parsers --
   -------------------

   procedure Reset_Parsers is
   begin
      Parsers.Init;
      Library_Paths.Init;
   end Reset_Parsers;

end Ocarina.Parser;
