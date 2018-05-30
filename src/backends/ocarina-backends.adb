------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                     O C A R I N A . B A C K E N D S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Table;

with Charset;        use Charset;
with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;
with Errors;         use Errors;

with Ocarina.Backends.Build_Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.PN;

with Ocarina.Backends.ARINC653_Conf;
with Ocarina.Backends.Deos_Conf;
with Ocarina.Backends.Vxworks653_Conf;
with Ocarina.Backends.PO_HI_Ada;
with Ocarina.Backends.ASN1;
with Ocarina.Backends.PO_HI_C;
with Ocarina.Backends.Stats;
with Ocarina.Backends.Subprograms;
with Ocarina.Backends.Cheddar;
with Ocarina.Backends.Connection_Matrix;
with Ocarina.Backends.Functions_Matrix;
with Ocarina.Backends.MAST;
with Ocarina.Backends.MAST_Values;
with Ocarina.Backends.MAST_Tree.Nodes;
with Ocarina.Backends.MAST_Tree.Nutils;
with Ocarina.Backends.POK_C;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Values;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.BoundT;
with Ocarina.Backends.REAL;
with Ocarina.Backends.LNT;
with Ocarina.Backends.Replication_Expander;
with Ocarina.Backends.Xtratum_Conf;
with Ocarina.Backends.AIR_Conf;
with Ocarina.Backends.ASN1_Tree.Nodes;
with Ocarina.Backends.ASN1_Tree.Nutils;
with Ocarina.Backends.ASN1_Values;
with Ocarina.Backends.AADL_XML;
with Ocarina.Backends.Alloy;

with Ocarina.Options; use Ocarina.Options;

package body Ocarina.Backends is

   use Ocarina.Backends.Messages;

   Current_Backend_Name : Name_Id      := No_Name;
   Current_Backend_Kind : Backend_Kind := Invalid_Backend;

   type Backend_Record is record
      Name    : Name_Id;
      Process : Backend_Subprogram;
      Kind    : Backend_Kind;
   end record;

   package Backend_Table is new GNAT.Table
     (Backend_Record,
      Natural,
      1,
      10,
      10);

   -------------------
   -- Generate_Code --
   -------------------

   procedure Generate_Code
     (Root         : Node_Id;
      Backend_Name : Name_Id := No_Name)
   is
      Current_Backend : Natural := 0;

   begin

      --  FIXME: Select the code generator according to information
      --  given in the instance root system.

      if Backend_Name /= No_Name then
         Current_Backend :=  Get_Backend (Backend_Name);

         if Current_Backend /= 0 then
            Current_Backend_Kind := Backend_Table.Table (Current_Backend).Kind;
         end if;

      elsif Current_Backend_Name = No_Name then
         Display_Error ("No backend name specified", Fatal => True);

      else
         Current_Backend :=  Get_Backend (Current_Backend_Name);

         if Current_Backend /= 0 then
            Current_Backend_Kind := Backend_Table.Table (Current_Backend).Kind;
         end if;
      end if;

      if Current_Backend = 0 then
         Ocarina.Backends.Messages.Display_Error
           ("Cannot find backend " & Get_Name_String (Current_Backend_Name),
            Fatal => True);
      end if;

      --  Call the current generator entry point

      Backend_Table.Table (Current_Backend).Process (Root);

   exception
      when E : others =>
         Errors.Display_Bug_Box (E);
   end Generate_Code;

   -----------------
   -- Get_Backend --
   -----------------

   function Get_Backend (Backend_Name : Name_Id := No_Name) return Natural is
      Current_Backend : Natural := 0;
   begin
      if Backend_Name /= No_Name then
         for B in Backend_Table.First .. Backend_Table.Last loop

            if Backend_Table.Table (B).Name = Backend_Name then
               Current_Backend      := B;
               exit;
            end if;
         end loop;
      end if;

      return Current_Backend;
   end Get_Backend;

   ------------------------------
   -- Get_Current_Backend_Kind --
   ------------------------------

   function Get_Current_Backend_Kind return Backend_Kind is
   begin
      return Current_Backend_Kind;
   end Get_Current_Backend_Kind;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Backend_Table.Init;
      Properties.Init;
      Ada_Tree.Nutils.Initialize;
      C_Tree.Nutils.Initialize;
      XML_Tree.Nutils.Initialize;
      MAST_Tree.Nutils.Initialize;
      ASN1_Tree.Nutils.Initialize;

      if Generated_Sources_Directory = No_Name then
         Generated_Sources_Directory := Get_String_Name (".");
      end if;

      Compile_Generated_Sources :=
        Compile_Generated_Sources
        or else Do_Regression_Test
        or else Do_Coverage_Test;

      --  Register the code generators

      Ocarina.Backends.ARINC653_Conf.Init;
      Ocarina.Backends.Vxworks653_Conf.Init;
      Ocarina.Backends.Deos_Conf.Init;
      PN.Init;
      BoundT.Init;
      MAST.Init;
      PO_HI_Ada.Init;
      PO_HI_C.Init;
      POK_C.Init;
      Xtratum_Conf.Init;
      Stats.Init;
      Subprograms.Init;
      REAL.Init;
      ASN1.Init;
      Cheddar.Init;
      LNT.Init;
      Replication_Expander.Init;
      Connection_Matrix.Init;
      Functions_Matrix.Init;
      AADL_XML.Init;
      Alloy.Init;
      AIR_Conf.Init;
   end Init;

   ----------------------
   -- Register_Backend --
   ----------------------

   procedure Register_Backend
     (Name    : String;
      Process : Backend_Subprogram;
      Kind    : Backend_Kind)
   is
      N : Name_Id;

   begin
      --  If the installation directory is unknown, we do not register
      --  the backend to avoid any future error.

      if Installation_Directory = No_Name then
         return;
      end if;

      Get_Name_String (Installation_Directory);
      Add_Str_To_Name_Buffer ("include" & Directory_Separator);
      Add_Str_To_Name_Buffer ("ocarina" & Directory_Separator);
      Add_Str_To_Name_Buffer ("runtime" & Directory_Separator);

      --  If the runtime is not installed, we do not register the
      --  backend to avoid any future error.

      if not Is_Directory (Name_Buffer (1 .. Name_Len)) then
         return;
      end if;

      N := Get_String_Name (To_Lower (Name));
      for B in Backend_Table.First .. Backend_Table.Last loop
         if Backend_Table.Table (B).Name = N then
            Display_Error
              ("Cannot register twice backend " & Name,
               Fatal => True);
         end if;
      end loop;

      Backend_Table.Increment_Last;
      Backend_Table.Table (Backend_Table.Last).Name    := N;
      Backend_Table.Table (Backend_Table.Last).Process := Process;
      Backend_Table.Table (Backend_Table.Last).Kind    := Kind;
   end Register_Backend;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      PO_HI_Ada.Reset;
      PO_HI_C.Reset;
      POK_C.Reset;
      Xtratum_Conf.Reset;
      Stats.Reset;
      Connection_Matrix.Reset;
      Functions_Matrix.Reset;

      Ada_Tree.Nutils.Reset;
      C_Tree.Nutils.Reset;
      ASN1_Tree.Nutils.Reset;
      MAST_Tree.Nutils.Reset;

      Ada_Tree.Nodes.Entries.Free;
      Ada_Tree.Nodes.Entries.Init;
      Ada_Values.Reset;

      C_Tree.Nodes.Entries.Free;
      C_Tree.Nodes.Entries.Init;
      C_Values.Reset;

      MAST_Tree.Nodes.Entries.Free;
      MAST_Tree.Nodes.Entries.Init;
      MAST_Values.Reset;

      XML_Values.Reset;
      BoundT.Reset;
      REAL.Reset;
      LNT.Reset;

      ASN1_Tree.Nodes.Entries.Free;
      ASN1_Tree.Nodes.Entries.Init;
      ASN1_Values.Reset;

      MAST_Tree.Nodes.Entries.Free;
      MAST_Tree.Nodes.Entries.Init;
      MAST_Values.Reset;

      Alloy.Reset;

      Build_Utils.Reset;
   end Reset;

   ------------------------------
   -- Set_Current_Backend_Name --
   ------------------------------

   procedure Set_Current_Backend_Name (Name : String) is
   begin
      Current_Backend_Name := Get_String_Name (To_Lower (Name));
   end Set_Current_Backend_Name;

   ------------------------------
   -- Get_Current_Backend_Name --
   ------------------------------

   function Get_Current_Backend_Name return Name_Id is
   begin
      return Current_Backend_Name;
   end Get_Current_Backend_Name;

   --------------------
   -- Write_Backends --
   --------------------

   procedure Write_Backends (Indent : Natural) is
   begin
      for Index in 1 .. Indent loop
         Write_Char (' ');
      end loop;

      Write_Line ("Registered backends:");

      for B in Backend_Table.First .. Backend_Table.Last loop
         if Backend_Table.Table (B).Name /= No_Name then
            for Index in 1 .. Indent + 1 loop
               Write_Char (' ');
            end loop;

            Write_Name (Backend_Table.Table (B).Name);
            Write_Eol;
         end if;
      end loop;
   end Write_Backends;

end Ocarina.Backends;
