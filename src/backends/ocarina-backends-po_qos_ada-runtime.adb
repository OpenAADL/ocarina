------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A . R U N T I M E   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2006-2008, GET-Telecom Paris.                --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Case_Util;

with Charset; use Charset;
with Namet;   use Namet;
with Utils;   use Utils;

with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Tree.Nutils;

package body Ocarina.Backends.PO_QoS_Ada.Runtime is

   use Ocarina.Backends.Ada_Tree.Nodes;
   use Ocarina.Backends.Ada_Tree.Nutils;

   Initialized : Boolean := False;

   RUD : array (RU_Id) of Node_Id := (RU_Id'Range => No_Node);
   RED : array (RE_Id) of Node_Id := (RE_Id'Range => No_Node);
   --  Arrays of run-time entity and unit designators

   type Casing_Rule is record
      Size : Natural;
      From : String_Access;
      Into : String_Access;
   end record;

   Rules : array (1 .. 64) of Casing_Rule;
   Rules_Last : Natural := 0;

   procedure Apply_Casing_Rules (S : in out String);
   --  Apply the registered casing rules on the string S

   procedure Register_Casing_Rule (S : String);
   --  Register a custom casing rule

   procedure Declare_Subunit (N : Node_Id);
   --  Declare the Unit corresponding to the node N as being nested

   function Get_Unit_Internal_Name (U_Name : Name_Id) return Name_Id;
   function Get_Unit_Position (U : Name_Id) return Int;
   procedure Set_Unit_Position (U : Name_Id; Pos : Int);
   --  The three routines below iensure the absence of conflict
   --  amongst different runtimes. The Get_Unit_Internal_Name does not
   --  affect globally the content of the name buffer.

   ----------------------------
   -- Get_Unit_Internal_Name --
   ----------------------------

   function Get_Unit_Internal_Name (U_Name : Name_Id) return Name_Id is
      Old_Name_Len    : constant Integer := Name_Len;
      Old_Name_Buffer : constant String := Name_Buffer;
      Result          : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("PO_QoS_Ada%RU%");
      Get_Name_String_And_Append (U_Name);
      Result := Name_Find;

      --  Restore the name buffer

      Name_Len := Old_Name_Len;
      Name_Buffer := Old_Name_Buffer;

      return Result;
   end Get_Unit_Internal_Name;

   -----------------------
   -- Get_Unit_Position --
   -----------------------

   function Get_Unit_Position (U : Name_Id) return Int is
      U_Name : constant Name_Id := Get_Unit_Internal_Name  (U);
   begin
      return Get_Name_Table_Info (U_Name);
   end Get_Unit_Position;

   -----------------------
   -- Set_Unit_Position --
   -----------------------

   procedure Set_Unit_Position (U : Name_Id; Pos : Int) is
      U_Name : constant Name_Id := Get_Unit_Internal_Name  (U);
   begin
      Set_Name_Table_Info (U_Name, Pos);
   end Set_Unit_Position;

   ------------------------
   -- Apply_Casing_Rules --
   ------------------------

   procedure Apply_Casing_Rules (S : in out String) is
      New_Word : Boolean := True;
      Length   : Natural := S'Length;
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

   ---------------------
   -- Declare_Subunit --
   ---------------------

   procedure Declare_Subunit (N : Node_Id) is
      S : Node_Id;

   begin
      pragma Assert (Kind (N) = K_Designator);
      S := Corresponding_Node (Defining_Identifier (N));
      pragma Assert (Kind (S) = K_Package_Specification);
      Set_Is_Subunit_Package (S, True);
   end Declare_Subunit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Position   : Integer;
      Name       : Name_Id;
      Identifier : Node_Id;
      Length     : Natural;
      Pkg_Spec   : Node_Id;

   begin
      --  Initialize the runtime only once

      if Initialized then
         return;
      end if;

      Initialized := True;

      Register_Casing_Rule ("AADL");
      Register_Casing_Rule ("ARAO");
      Register_Casing_Rule ("PolyORB");
      Register_Casing_Rule ("ExceptionList");
      Register_Casing_Rule ("NVList");
      Register_Casing_Rule ("NV_");
      Register_Casing_Rule ("TypeCode");
      Register_Casing_Rule ("ORB");
      Register_Casing_Rule ("IIOP");
      Register_Casing_Rule ("DIOP");
      Register_Casing_Rule ("SOAP");
      Register_Casing_Rule ("_OA");
      Register_Casing_Rule ("OA");
      Register_Casing_Rule ("RT");
      Register_Casing_Rule ("ARG_INOUT");
      Register_Casing_Rule ("ARG_IN");
      Register_Casing_Rule ("ARG_OUT");
      Register_Casing_Rule ("TC_");
      Register_Casing_Rule ("_TC");

      for U in RU_Id'Succ (RU_Id'First) .. RU_Id'Last loop
         Set_Str_To_Name_Buffer (RU_Id'Image (U));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));

         RUD (U) := New_Node (K_Designator);

         Position := 0;
         Name     := Name_Find;
         Length   := Name_Len;
         Set_Unit_Position (Name, RU_Id'Pos (U));

         while Name_Len > 0 loop
            if Name_Buffer (Name_Len) = '_' then
               Name_Len := Name_Len - 1;
               Position := Integer (Get_Unit_Position (Name_Find));
               exit when Position > 0;

            else
               Name_Len := Name_Len - 1;
            end if;
         end loop;

         --  When there is a parent, remove parent unit name from
         --  unit name to get real identifier.

         if Position > 0 then
            Set_Str_To_Name_Buffer
              (Name_Buffer (Name_Len + 2 .. Length));
            Name := Name_Find;
            Set_Homogeneous_Parent_Unit_Name
              (RUD (U), RUD (RU_Id'Val (Position)));
         end if;

         Get_Name_String (Name);
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));
         Identifier := Make_Defining_Identifier (Name_Find);
         Set_Defining_Identifier (RUD (U), Identifier);
         Pkg_Spec := New_Node (K_Package_Specification);
         Set_Is_Runtime_Package (Pkg_Spec, True);
         Set_Corresponding_Node (Identifier, Pkg_Spec);

         if Position > 0 then
            Set_Homogeneous_Parent_Unit_Name
              (Identifier,
               Defining_Identifier (Parent_Unit_Name (RUD (U))));
         end if;

      end loop;

      Declare_Subunit
        (RUD (RU_Ada_Strings_Wide_Bounded_Generic_Bounded_Length));
      Declare_Subunit (RUD (RU_Ada_Strings_Bounded_Generic_Bounded_Length));
      Declare_Subunit (RUD (RU_PolyORB_Any_NVList_Internals));
      Declare_Subunit (RUD (RU_PolyORB_Any_NVList_Internals_NV_Lists));
      Declare_Subunit (RUD (RU_PolyORB_Any_TypeCode));

      --  Package Standard is not a subunit but it has to be handled
      --  in a specific way as well as subunit.

      Declare_Subunit (RUD (RU_Standard));

      for E in RE_Id loop
         Set_Str_To_Name_Buffer (RE_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         while Name_Buffer (Name_Len) in '0' .. '9'
           or else Name_Buffer (Name_Len) = '_'
         loop
            Name_Len := Name_Len - 1;
         end loop;

         if E = RE_Add then
            Set_Str_To_Name_Buffer (Quoted ("+"));
         end if;

         if E = RE_And then
            Set_Str_To_Name_Buffer (Quoted ("&"));
         end if;

         Name := Name_Find;

         RED (E) := New_Node (K_Designator);
         Set_Defining_Identifier
           (RED (E), Make_Defining_Identifier (Name));
         Set_Homogeneous_Parent_Unit_Name (RED (E), RUD (RE_Unit_Table (E)));
      end loop;
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      RUD := (RU_Id'Range => No_Node);
      RED := (RE_Id'Range => No_Node);
      Rules_Last := 0;

      Initialized := False;
   end Reset;

   --------
   -- RE --
   --------

   function RE (Id : RE_Id; Withed : Boolean := True) return Node_Id is
   begin
      return Copy_Designator (RED (Id), Withed);
   end RE;

   --------------------------
   -- Register_Casing_Rule --
   --------------------------

   procedure Register_Casing_Rule (S : String) is
   begin
      Rules_Last := Rules_Last + 1;
      Rules (Rules_Last).Size := S'Length;
      Rules (Rules_Last).Into := new String'(S);
      Rules (Rules_Last).From := new String'(S);
      To_Lower (Rules (Rules_Last).From.all);
   end Register_Casing_Rule;

   --------
   -- RU --
   --------

   function RU (Id : RU_Id; Withed : Boolean := True) return Node_Id is
      Result : Node_Id;
   begin
      --  This is a runtime unit and not a runtime entity, so it's
      --  parent unit does not have to be "withed"

      Result := Copy_Designator (RUD (Id), False);
      if Withed then
         Add_With_Package (Result);
      end if;
      return Result;
   end RU;

end Ocarina.Backends.PO_QoS_Ada.Runtime;
