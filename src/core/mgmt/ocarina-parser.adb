------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                       O C A R I N A . P A R S E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Charset;   use Charset;
with Namet;     use Namet;
with Output;    use Output;

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
      To        : Location := No_Location)
     return Node_Id
   is
   begin
      for P in Parsers.First .. Parsers.Last loop
         if Language = Parsers.Table (P).Language then
            return Parsers.Table (P).Parser (AADL_Root, From, To);
         end if;
      end loop;

      Set_Standard_Error;
      Write_Line ("Cannot find language " & Get_Name_String (Language));
      Set_Standard_Output;
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

   procedure Register_Parser
     (Language : String;
      Parser   : Parser_Subprogram)
   is
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
