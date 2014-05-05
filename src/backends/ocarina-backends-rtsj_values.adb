------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . R T S J _ V A L U E S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

with Ocarina.Namet;  use Ocarina.Namet;

with GNAT.Table;

with Ocarina.AADL_Values;

package body Ocarina.Backends.RTSJ_Values is

   package OV renames Ocarina.AADL_Values;
   package VT is
      new GNAT.Table (Value_Type, Value_Id, No_Value + 1, 10, 10);

   Hex : constant String := "0123456789ABCDEF";
   subtype ULL is Unsigned_Long_Long;

   -----------
   -- Reset --
   -----------
   procedure Reset is
   begin
      VT.Init;
   end Reset;

   ----------------------------
   -- Add_ULL_To_Name_Buffer --
   ----------------------------
   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL; S : Integer := 1)
   is
      Q : constant ULL := U / B;
      R : constant ULL := U mod B;
   begin
      if Q /= 0 or else S > 1 then
         Add_ULL_To_Name_Buffer (Q, B, S - 1);
      end if;
      Add_Char_To_Name_Buffer (Hex (Hex'First + Natural (R)));
   end Add_ULL_To_Name_Buffer;

   -----------
   -- Image --
   -----------
   function Image (Value : Value_Id) return String is
      V : Value_Type;
   begin
      if Value = No_Value then
         return "<>";
      end if;

      V := VT.Table (Value);
      Name_Len := 0;
      case V.K is
         when K_Int =>
            if (V.Sign < 0) then
               Add_Char_To_Name_Buffer ('-');
            end if;

            Add_ULL_To_Name_Buffer (V.IVal, ULL (V.Base));

         when K_String =>
            if V.PCVal = No_Name then
               return '"' & '"';
            end if;
            Add_Char_To_Name_Buffer ('"');
            Get_Name_String_And_Append (V.PCVal);
            Add_Char_To_Name_Buffer ('"');

         when others =>
            raise Program_Error;
      end case;

      return Name_Buffer (1 .. Name_Len);
   end Image;

   ---------------
   -- New_Value --
   ---------------
   function New_Value (Value : Value_Type) return Value_Id is
      V : Value_Id;
   begin
      VT.Increment_Last;
      V := VT.Last;
      VT.Table (V) := Value;
      return V;
   end New_Value;

   -----------------------
   -- New_Int_Value --
   -----------------------
   function New_Int_Value
     (Value : Unsigned_Long_Long;
      Sign : Short_Short;
      Base : Unsigned_Short_Short)
      return Value_Id
   is
   begin
      return New_Value (Value_Type'(K_Int, Value, Sign, Base));
   end New_Int_Value;

   ----------------------
   -- New_String_Value --
   ----------------------
   function New_String_Value (Value : Name_Id) return Value_Id is
   begin
      return New_Value (Value_Type'(K_String, Value));
   end New_String_Value;

   -------------------
   -- To_RTSJ_Value --
   -------------------
   function To_RTSJ_Value (V : Value_Id) return Value_Id is
      VT     : constant OV.Value_Type := OV.Value (V);
      Result : Value_Id;
   begin
      case VT.T is
         when OV.LT_Integer =>
            if VT.ISign then
               Result := New_Int_Value (VT.IVal, -1, VT.IBase);
            else
               Result := New_Int_Value (VT.IVal, 1, VT.IBase);
            end if;

         when OV.LT_String =>
            Result := New_String_Value (VT.Sval);

         when others =>
            raise Constraint_Error;
      end case;

      return Result;
   end To_RTSJ_Value;

   ---------------
   -- Get_Value --
   ---------------
   function Get_Value (V : Value_Id) return Value_Type is
   begin
      return VT.Table (V);
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------
   procedure Set_Value
     (V : Value_Id;
      X : Value_Type) is
   begin
      VT.Table (V) := X;
   end Set_Value;

end Ocarina.Backends.RTSJ_Values;
