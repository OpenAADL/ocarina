------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ H I _ R T S J . R U N T I M E   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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
with GNAT.Case_Util;

with Utils;   use Utils;
with Charset; use Charset;
with Ocarina.Namet;   use Ocarina.Namet;

with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.RTSJ_Tree.Nutils;

package body Ocarina.Backends.PO_HI_RTSJ.Runtime is

   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;

   Initialized : Boolean := False;

   --  Arrays of runtime entities and unit designators
   RED : array (RE_Id) of Node_Id := (RE_Id'Range => No_Node);
   RHD : array (RH_Id) of Node_Id := (RH_Id'Range => No_Node);

   type Casing_Rule is record
      Size : Natural;
      From : String_Access;
      Into : String_Access;
   end record;

   Rules      : array (1 .. 64) of Casing_Rule;
   Rules_Last : Natural := 0;

   procedure Apply_Casing_Rules (S : in out String);
   --  Apply the registered casing rule on the String S

   procedure Register_Casing_Rule (S : String);
   --  Register a custom casing rule

   ------------------------
   -- Apply_Casing_Rules --
   ------------------------
   procedure Apply_Casing_Rules (S : in out String) is
      New_Word : Boolean         := True;
      Length   : Natural         := S'Length;
      S1       : constant String := To_Lower (S);
   begin
      GNAT.Case_Util.To_Mixed (S);

      for I in S'Range loop
         if New_Word then
            New_Word := False;
            for J in 1 .. Rules_Last loop
               if Rules (J).Size <= Length
                 and then S1 (I .. I + Rules (J).Size - 1) = Rules (J).From.all
               then
                  S (I .. I + Rules (J).Size - 1) := Rules (J).Into.all;
               end if;
            end loop;
         end if;

         if S (I) = '_' then
            New_Word := True;
            for J in 1 .. Rules_Last loop
               if Rules (J).Size <= Length
                 and then S1 (I .. I + Rules (J).Size - 1) = Rules (J).From.all
               then
                  S (I .. I + Rules (J).Size - 1) := Rules (J).Into.all;
               end if;
            end loop;
         end if;
         Length := Length - 1;
      end loop;
   end Apply_Casing_Rules;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize is
      Name : Name_Id;
      N    : Node_Id;
   begin
      --  Initialize the runtime only once
      if Initialized then
         return;
      end if;

      Initialized := True;

      Register_Casing_Rule ("AADL");

      for E in RM_Id loop
         Set_Str_To_Name_Buffer (RM_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         Name :=
           To_RTSJ_Conventional_Name
             (Get_String_Name (Name_Buffer (1 .. Name_Len)),
              True);
         Name :=
           Add_Prefix_To_Name ("fr.enst.ocarina.polyORB_HI_runtime.", Name);

         N := New_Node (K_Defining_Identifier);
         Set_Name (N, Name);

         RHD (E) := Make_Import_Statement (N);
      end loop;

      for E in RN_Id loop
         Set_Str_To_Name_Buffer (RN_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         Name :=
           To_RTSJ_Conventional_Name
             (Get_String_Name (Name_Buffer (1 .. Name_Len)),
              True);
         Name := Add_Prefix_To_Name ("javax.realtime.", Name);

         N := New_Node (K_Defining_Identifier);
         Set_Name (N, Name);

         RHD (E) := Make_Import_Statement (N);
      end loop;

      for E in RC_Id loop
         Set_Str_To_Name_Buffer (RC_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         Name    := To_Upper (Name_Find);
         RED (E) := New_Node (K_Defining_Identifier);
         Set_Name (RED (E), Name);
      end loop;

      for E in RV_Id loop
         Set_Str_To_Name_Buffer (RV_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         Name :=
           To_RTSJ_Conventional_Name
             (Get_String_Name (Name_Buffer (1 .. Name_Len)),
              False);
         RED (E) := New_Node (K_Defining_Identifier);
         Set_Name (RED (E), Name);
      end loop;

      for E in RT_Id loop
         Set_Str_To_Name_Buffer (RT_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         Name :=
           To_RTSJ_Conventional_Name
             (Get_String_Name (Name_Buffer (1 .. Name_Len)),
              True);
         RED (E) := New_Node (K_Defining_Identifier);
         Set_Name (RED (E), Name);
      end loop;

   end Initialize;

   -----------
   -- Reset --
   -----------
   procedure Reset is
   begin
      RED        := (RE_Id'Range => No_Node);
      RHD        := (RH_Id'Range => No_Node);
      Rules_Last := 0;

      Initialized := False;
   end Reset;

   --------
   -- RE --
   --------
   function RE (Id : RE_Id) return Node_Id is
   begin
      if RE_Header_Table (Id) /= RH_Null then
         Add_Import (RH (RE_Header_Table (Id)));
      end if;
      return Copy_Node (RED (Id));
   end RE;

   --------
   -- RH --
   --------
   function RH (Id : RH_Id) return Node_Id is
   begin
      return Copy_Node (RHD (Id));
   end RH;

   --------------------------
   -- Register_Casing_Rule --
   --------------------------
   procedure Register_Casing_Rule (S : String) is
   begin
      Rules_Last              := Rules_Last + 1;
      Rules (Rules_Last).Size := S'Length;
      Rules (Rules_Last).Into := new String'(S);
      Rules (Rules_Last).From := new String'(S);
      To_Lower (Rules (Rules_Last).From.all);
   end Register_Casing_Rule;

end Ocarina.Backends.PO_HI_RTSJ.Runtime;
