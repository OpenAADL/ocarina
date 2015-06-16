------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . A D A _ V A L U E S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.AADL_Values;

with GNAT.Table;

package body Ocarina.Backends.Ada_Values is

   package OV renames Ocarina.AADL_Values;

   Hex : constant String := "0123456789ABCDEF";

   package VT is new GNAT.Table (Value_Type, Value_Id, No_Value + 1, 10, 10);

   subtype ULL is Unsigned_Long_Long;

   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL; S : Integer := 1);

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      VT.Init;
   end Reset;

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Value_Type) return Value_Type is
      V : Value_Type := L;
   begin
      case V.K is
         when K_Integer =>
            if L.Base = R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal * R.IVal;

         when K_Float =>
            V.FVal := L.FVal * R.FVal;

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Value_Type) return Value_Type is
      V : Value_Type := R;
   begin
      case R.K is
         when K_Integer =>
            if L.Base /= R.Base then
               V.Base := 10;
            end if;
            if L.Sign = R.Sign then
               V.IVal := L.IVal + R.IVal;
            elsif R.IVal <= L.IVal then
               V.Sign := L.Sign;
               V.IVal := L.IVal - R.IVal;
            else
               V.Sign := -L.Sign;
               V.IVal := R.IVal - L.IVal;
            end if;

         when K_Float =>
            V.FVal := L.FVal + R.FVal;

         when others =>
            return Bad_Value;

      end case;
      return V;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (R : Value_Type) return Value_Type is
      V : Value_Type := R;
   begin
      case R.K is
         when K_Integer =>
            V.Sign := -V.Sign;

         when K_Float =>
            V.FVal := -V.FVal;

         when others =>
            return Bad_Value;

      end case;
      return V;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Value_Type) return Value_Type is
      V : Value_Type := R;
   begin
      case R.K is
         when K_Integer =>
            V.Sign := -V.Sign;

         when K_Float =>
            V.FVal := -V.FVal;

         when others =>
            return Bad_Value;

      end case;
      return L + V;
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Value_Type) return Value_Type is
      V : Value_Type := L;
   begin
      case V.K is
         when K_Integer =>
            if L.Base = R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal / R.IVal;

         when K_Float =>
            V.FVal := L.FVal / R.FVal;

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "/";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Value_Type) return Boolean is
   begin
      case R.K is
         when K_Integer =>
            if L.Sign > 0 then
               if R.Sign > 0 then
                  return L.IVal < R.IVal;
               else
                  return False;
               end if;
            elsif R.Sign > 0 then
               return True;
            else
               return L.IVal > R.IVal;
            end if;

         when K_Float =>
            return L.FVal < R.FVal;

         when others =>
            return False;

      end case;
   end "<";

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Value_Type) return Value_Type is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case L.K is
         when K_Integer =>
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            if LV.Sign < 0 then
               LV.IVal := LULL - LV.IVal;
            end if;
            if RV.Sign < 0 then
               RV.IVal := LULL - RV.IVal;
            end if;
            LV.IVal := LV.IVal and RV.IVal;
            LV.Sign := 1;

         when K_Boolean =>
            LV.IVal := LV.IVal and RV.IVal;

         when others =>
            return Bad_Value;
      end case;
      return LV;
   end "and";

   -----------
   -- "mod" --
   -----------

   function "mod" (L, R : Value_Type) return Value_Type is
      V : Value_Type := L;
   begin
      case L.K is
         when K_Integer =>
            if L.Base /= R.Base then
               V.Base := 10;
            end if;
            V.Sign := L.Sign * R.Sign;
            V.IVal := L.IVal mod R.IVal;

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "mod";

   -----------
   -- "not" --
   -----------

   function "not" (R : Value_Type) return Value_Type is
      V : Value_Type := R;
   begin
      case V.K is
         when K_Integer =>
            V.IVal := Unsigned_Long_Long (not Unsigned_Long (V.IVal));

         when K_Boolean =>
            V.IVal := 1 - V.IVal;

         when others =>
            return Bad_Value;
      end case;
      return V;
   end "not";

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Value_Type) return Value_Type is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case L.K is
         when K_Integer =>
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            if LV.Sign < 0 then
               LV.IVal := LULL - LV.IVal;
            end if;
            if RV.Sign < 0 then
               RV.IVal := LULL - RV.IVal;
            end if;
            LV.IVal := LV.IVal or RV.IVal;
            LV.Sign := 1;

         when K_Boolean =>
            LV.IVal := LV.IVal or RV.IVal;

         when others =>
            return Bad_Value;
      end case;
      return LV;
   end "or";

   -----------
   -- "xor" --
   -----------

   function "xor" (L, R : Value_Type) return Value_Type is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case LV.K is
         when K_Integer =>
            if LV.Base /= RV.Base then
               LV.Base := 10;
            end if;
            if LV.Sign < 0 then
               LV.IVal := LULL - LV.IVal;
            end if;
            if RV.Sign < 0 then
               RV.IVal := LULL - RV.IVal;
            end if;
            LV.IVal := LV.IVal xor RV.IVal;
            LV.Sign := 1;

         when K_Boolean =>
            LV.IVal := LV.IVal xor RV.IVal;

         when others =>
            return Bad_Value;
      end case;
      return LV;
   end "xor";

   ----------------------------
   -- Add_ULL_To_Name_Buffer --
   ----------------------------

   procedure Add_ULL_To_Name_Buffer (U : ULL; B : ULL; S : Integer := 1) is
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
      V        := VT.Table (Value);
      Name_Len := 0;
      case V.K is
         when K_Boolean =>
            if V.IVal = 1 then
               return "True";
            else
               return "False";
            end if;

         when K_Integer =>
            if V.Sign < 0 then
               Add_Char_To_Name_Buffer ('-');
            elsif V.Base = 16 then
               Add_Str_To_Name_Buffer ("16#");
            elsif V.Base = 8 then
               Add_Str_To_Name_Buffer ("8#");
            end if;

            Add_ULL_To_Name_Buffer (V.IVal, ULL (V.Base));

            if V.Base = 16 or else V.Base = 8 then
               Add_Char_To_Name_Buffer ('#');
            end if;

         when K_Float =>
            Add_Str_To_Name_Buffer (Long_Double'Image (V.FVal));
            declare
               Index : Natural := Name_Len;

            begin

               --  Find exponent if any

               while Index > 0 and then Name_Buffer (Index) /= 'E' loop
                  Index := Index - 1;
               end loop;

               --  Remove leading zero in exponent part.

               if Index > 0 then
                  Index := Index + 2;
                  while Index <= Name_Len and then Name_Buffer (Index) = '0'
                  loop
                     Name_Buffer (Index .. Name_Len - 1) :=
                       Name_Buffer (Index + 1 .. Name_Len);
                     Name_Len := Name_Len - 1;
                  end loop;

                  --  Remove exponent

                  if Index > Name_Len then
                     Name_Len := Name_Len - 2;
                     Index    := Name_Len;

                  else
                     Index := Name_Len;
                     while Name_Buffer (Index) /= 'E' loop
                        Index := Index - 1;
                     end loop;
                     Index := Index - 1;
                  end if;

               end if;

               --  Remove trailing zero in fraction part.

               while Name_Buffer (Index) = '0' loop
                  exit when Name_Buffer (Index - 1) = '.';
                  Name_Buffer (Index .. Name_Len - 1) :=
                    Name_Buffer (Index + 1 .. Name_Len);
                  Name_Len := Name_Len - 1;
                  Index    := Index - 1;
               end loop;
            end;

         when K_Character | K_Wide_Character =>
            if V.CVal <= 127 then
               declare
                  C : constant Character := Character'Val (Natural (V.CVal));
               begin
                  if C in '!' .. '~' or else C = ' ' then
                     Add_Char_To_Name_Buffer (''');
                     Add_Char_To_Name_Buffer (C);
                     Add_Char_To_Name_Buffer (''');
                  else
                     if V.K = K_Wide_Character then
                        Add_Str_To_Name_Buffer ("Wide_");
                     end if;
                     Add_Str_To_Name_Buffer ("Character'Val (");
                     Add_ULL_To_Name_Buffer (ULL (V.CVal), 10);
                     Add_Char_To_Name_Buffer (')');
                  end if;
               end;
            else
               Add_Str_To_Name_Buffer ("Wide_Character'Val (");
               Add_ULL_To_Name_Buffer (ULL (V.CVal), 10);
               Add_Char_To_Name_Buffer (')');
            end if;

         when K_String | K_Wide_String =>
            if V.SVal = No_Name then
               return '"' & '"';
            end if;
            Add_Char_To_Name_Buffer ('"'); -- "
            declare
               The_String : constant String := Get_Name_String (V.SVal);
            begin
               for J in The_String'Range loop
                  Add_Char_To_Name_Buffer (The_String (J));
                  --  Escape '"' character
                  if The_String (J) = '"' then
                     Add_Char_To_Name_Buffer (The_String (J));
                  end if;
               end loop;
            end;
            Add_Char_To_Name_Buffer ('"'); -- "

         when others =>
            raise Program_Error;
      end case;

      return Name_Buffer (1 .. Name_Len);
   end Image;

   ------------------
   -- To_Ada_Value --
   ------------------

   function To_Ada_Value (V : Value_Id) return Value_Id is
      VT     : constant OV.Value_Type := OV.Value (V);
      Result : Value_Id;
   begin
      case VT.T is
         when OV.LT_Integer =>
            if VT.ISign then
               Result := New_Integer_Value (VT.IVal, -1, VT.IBase);
            else
               Result := New_Integer_Value (VT.IVal, 1, VT.IBase);
            end if;
         when OV.LT_Real =>
            Result := New_Floating_Point_Value (Long_Double (VT.RVal));
         when OV.LT_String =>
            Result := New_String_Value (VT.SVal);
         when OV.LT_Boolean =>
            Result := New_Boolean_Value (VT.BVal);
         when others =>
            raise Constraint_Error;
      end case;

      return Result;
   end To_Ada_Value;

   -----------------------
   -- New_Boolean_Value --
   -----------------------

   function New_Boolean_Value (Value : Boolean) return Value_Id is
   begin
      return New_Value (Value_Type'(K_Boolean, Boolean'Pos (Value), 1, 10));
   end New_Boolean_Value;

   ------------------------------
   -- New_Floating_Point_Value --
   ------------------------------

   function New_Floating_Point_Value (Value : Long_Double) return Value_Id is
   begin
      return New_Value (Value_Type'(K_Float, Value));
   end New_Floating_Point_Value;

   -----------------------
   -- New_Integer_Value --
   -----------------------

   function New_Integer_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short) return Value_Id
   is
   begin
      return New_Value (Value_Type'(K_Integer, Value, Sign, Base));
   end New_Integer_Value;

   ----------------------
   -- New_String_Value --
   ----------------------

   function New_String_Value
     (Value : Name_Id;
      Wide  : Boolean := False) return Value_Id
   is
   begin
      if Wide then
         return New_Value (Value_Type'(K_Wide_String, Value));
      else
         return New_Value (Value_Type'(K_String, Value));
      end if;
   end New_String_Value;

   -------------------------
   -- New_Character_Value --
   -------------------------

   function New_Character_Value
     (Value : Unsigned_Short;
      Wide  : Boolean := False) return Value_Id
   is
   begin
      if Wide then
         return New_Value (Value_Type'(K_Wide_Character, Value));
      else
         return New_Value (Value_Type'(K_Character, Value));
      end if;
   end New_Character_Value;

   ---------------
   -- New_Value --
   ---------------

   function New_Value (Value : Value_Type) return Value_Id is
      V : Value_Id;
   begin
      VT.Increment_Last;
      V            := VT.Last;
      VT.Table (V) := Value;
      return V;
   end New_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (V : Value_Id; X : Value_Type) is
   begin
      VT.Table (V) := X;
   end Set_Value;

   ----------------
   -- Shift_Left --
   ----------------

   function Shift_Left (L, R : Value_Type) return Value_Type is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case RV.K is
         when K_Integer =>
            if RV.Sign < 0 then
               RV.Sign := 1;
               return Shift_Right (LV, RV);
            end if;

            --  Keep working with left operand base

            LV.IVal := Shift_Left (LV.IVal, Natural (RV.IVal));
            return LV;

         when others =>
            return Bad_Value;
      end case;
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   function Shift_Right (L, R : Value_Type) return Value_Type is
      LV : Value_Type := L;
      RV : Value_Type := R;
   begin
      case RV.K is
         when K_Integer =>
            if RV.Sign < 0 then
               RV.Sign := 1;
               return Shift_Left (LV, RV);
            end if;

            --  Keep working with left operand base

            LV.IVal := Shift_Right (LV.IVal, Natural (RV.IVal));
            return LV;

         when others =>
            return Bad_Value;
      end case;
   end Shift_Right;

   -----------
   -- Value --
   -----------

   function Value (V : Value_Id) return Value_Type is
   begin
      return VT.Table (V);
   end Value;

end Ocarina.Backends.Ada_Values;
