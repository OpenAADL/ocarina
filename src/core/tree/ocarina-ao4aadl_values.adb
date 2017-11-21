------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . A O 4 A A D L _ V A L U E S                --
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
with GNAT.Table;

with Ada.Characters.Handling;
with Ada.Long_Long_Float_Text_IO;

with Charset;
with Locations;

with Ocarina.AADL_Values;
with Ocarina.ME_AO4AADL.AO4AADL_Tree.Nutils;

package body Ocarina.AO4AADL_Values is

   AADL_True  : constant String := "true";
   AADL_False : constant String := "false";

   package VT is new GNAT.Table (Value_Type, Value_Id, No_Value + 1, 10, 10);

   ----------------
   -- AADL_Value --
   ----------------

   function AADL_Value (V : Value_Id) return Value_Id
   is
      package OV renames Ocarina.AADL_Values;

      VT : constant OV.Value_Type := OV.Get_Value_Type (V);
      R  : Value_Id;
   begin
      case VT.T is
         when OV.LT_Integer =>
            R := New_Integer_Value (VT.IVal, VT.ISign, VT.IBase, VT.IExp);

         when OV.LT_Real =>
            R := New_Real_Value (VT.RVal, VT.RSign, VT.RBase, VT.RExp);

         when OV.LT_String =>
            R := New_String_Value (VT.SVal);

         when OV.LT_Boolean =>
            R := New_Boolean_Value (VT.BVal);

         when OV.LT_Enumeration =>
            R := New_Enum_Value (VT.EVal);
      end case;

      return R;
   end AADL_Value;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      VT.Init;
   end Reset;

   --------------------
   -- Get_Value_Type --
   --------------------

   function Get_Value_Type (Value : Value_Id) return Value_Type is
   begin
      return VT.Table (Value);
   end Get_Value_Type;

   -----------
   -- Image --
   -----------

   function Image (Value : Value_Type; Quoted : Boolean := True) return String
   is
      use Namet;

   begin
      Name_Len := 0;

      case Value.T is
         when LT_Boolean =>
            if Value.BVal then
               return AADL_True;
            else
               return AADL_False;
            end if;

         when LT_String =>
            if Value.SVal = No_Name then
               return """""";    --  null string ' "" '
            else
               if Quoted then
                  Set_Char_To_Name_Buffer ('"');
                  Get_Name_String_And_Append (Value.SVal);
                  Add_Char_To_Name_Buffer ('"');
               else
                  Get_Name_String (Value.SVal);
               end if;
            end if;

         when LT_Enumeration =>
            if Value.EVal = No_Name then
               return "";    --  null string ' "" '
            else
               Get_Name_String (Value.EVal);
            end if;

         when LT_Real =>
            if Value.RSign then
               Set_Char_To_Name_Buffer ('-');
            end if;

            Add_Str_To_Name_Buffer
              (Image (Value.RVal, Value.RBase, Value.RExp));

         when LT_Integer =>
            if Value.ISign then
               Set_Char_To_Name_Buffer ('-');
            end if;
            Add_Str_To_Name_Buffer
              (Image (Value.IVal, Value.IBase, Value.IExp));

         when LT_List =>
            --  XXX FIXME :
            raise Program_Error with "not implemented yet";

         when LT_Range =>
            if Value.RSign_Left then
               Set_Char_To_Name_Buffer ('-');
            end if;
            Add_Str_To_Name_Buffer
              (Image (Value.RVal_Left, Value.RVBase, Value.RVExp));

            Add_Str_To_Name_Buffer (" - ");

            if Value.RSign_Right then
               Set_Char_To_Name_Buffer ('-');
            end if;
            Add_Str_To_Name_Buffer
              (Image (Value.RVal_Right, Value.RVBase, Value.RVExp));

         when LT_Element =>
            --  FIXME
            raise Program_Error;

      end case;

      return Name_Buffer (1 .. Name_Len);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : Value_Id; Quoted : Boolean := True) return String
   is
   begin
      if Value = No_Value then
         return "NoValue";
      else
         return Image (VT.Table (Value), Quoted);
      end if;
   end Image;

   --------------------
   -- New_List_Value --
   --------------------

   function New_List_Value (Value : List_Id) return Value_Id is
   begin
      return New_Value (Value_Type'(LT_List, Value));
   end New_List_Value;

   -----------------------
   -- New_Boolean_Value --
   -----------------------

   function New_Boolean_Value (Value : Boolean) return Value_Id is
   begin
      return New_Value (Value_Type'(LT_Boolean, Value));
   end New_Boolean_Value;

   ---------------------
   -- New_Range_Value --
   ---------------------

   function New_Range_Value
     (LValue    : Long_Long_Float;
      RValue    : Long_Long_Float;
      LNegative : Boolean              := False;
      RNegative : Boolean              := False;
      Base      : Unsigned_Short_Short := 10;
      Exp       : Integer              := 0)
     return Value_Id is
   begin
      return New_Value (Value_Type'(LT_Range,
                                    LValue,
                                    LNegative,
                                    RValue,
                                    RNegative,
                                    Base, Exp));
   end New_Range_Value;

   --------------------
   -- New_Real_Value --
   --------------------

   function New_Real_Value
     (Value    : Long_Long_Float;
      Negative : Boolean              := False;
      Base     : Unsigned_Short_Short := 10;
      Exp      : Integer              := 0)
     return Value_Id
   is
   begin
      return New_Value (Value_Type'(LT_Real, Value, Negative, Base, Exp));
   end New_Real_Value;

   -----------------------
   -- New_Integer_Value --
   -----------------------

   function New_Integer_Value
     (Value    : Unsigned_Long_Long;
      Negative : Boolean              := False;
      Base     : Unsigned_Short_Short := 10;
      Exp      : Integer              := 0)
     return Value_Id
   is
   begin
      return New_Value (Value_Type'(LT_Integer, Value, Negative, Base, Exp));
   end New_Integer_Value;

   ----------------------
   -- New_String_Value --
   ----------------------

   function New_String_Value (Value : Name_Id) return Value_Id is
   begin
      return New_Value (Value_Type'(LT_String, Value));
   end New_String_Value;

   --------------------
   -- New_Enum_Value --
   --------------------

   function New_Enum_Value (Value : Name_Id) return Value_Id is
   begin
      return New_Value (Value_Type'(LT_Enumeration, Value));
   end New_Enum_Value;

   --------------------
   -- New_Elem_Value --
   --------------------

   function New_Elem_Value (Value : Node_Id) return Value_Id is
   begin
      return New_Value (Value_Type'(LT_Element, Value));
   end New_Elem_Value;

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

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (V : Value_Id; X : Value_Type) is
   begin
      VT.Table (V) := X;
   end Set_Value;

   -----------
   -- Value --
   -----------

   function Value (V : Value_Id) return Value_Type is
   begin
      return VT.Table (V);
   end Value;

   function Image
     (V    : Long_Long_Float;
      Base : Unsigned_Short_Short)
     return String;

   function Remove_Ending_Zeros (Str : String) return String;
   --  Remove ending zeros '0'
   --  WARNING: if the result string terminates with an '.', the character
   --           which is next to '.' will be rescued
   --  Example: Remove_Ending_Zeros ("0.0") = "0.0"

   function Remove_Leading_Spaces (Str : String) return String;
   --  Remove leading spaces

   Minus_Character     : constant Character := '-';
   Base_Separator      : constant Character := '#';
   Exp_Separator       : constant Character := 'E';
   Real_Separator      : constant Character := '.';

   Real_Epsilon        : constant Long_Long_Float := 1.0E-10;
   Fraction_Max_Digits : constant Integer := 4 * 10;
   --  Max digits = digits (Real_Epsilon) in base 2 (not in base 10 !!)
   --             = about (4 * digits (Real_Epsilon) in base 10)

   function Image
     (V    : Unsigned_Long_Long;
      Base : Unsigned_Short_Short)
     return String;

   -----------
   -- Image --
   -----------

   function Image (V : Unsigned_Short_Short) return String is
   begin
      return Remove_Leading_Spaces (Unsigned_Short_Short'Image (V));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (V : Integer) return String is
   begin
      return Remove_Leading_Spaces (Integer'Image (V));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (V : Unsigned_Long_Long) return String is
   begin
      return Remove_Leading_Spaces (Unsigned_Long_Long'Image (V));
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (V    : Unsigned_Long_Long;
      Base : Unsigned_Short_Short)
     return String
   is
      Str     : String (1 .. Unsigned_Long_Long'Size + 4);
      --  Max digits = BB # (Max Bits) #

      Str_Pos : Integer := Str'Last;
      Rest    : Unsigned_Long_Long := V;
      LBase   : constant Unsigned_Long_Long := Unsigned_Long_Long (Base);
      Digit   : Unsigned_Short_Short;
      Ch      : Character;

   begin
      if Base < 2 or else Base > 16 then
         raise Constraint_Error;
      end if;

      loop
         Digit := Unsigned_Short_Short (Rest mod LBase);
         if Digit < 10 then
            Ch := Character'Val (Character'Pos ('0') + Digit);
         else
            Ch := Character'Val (Character'Pos ('A') + Digit - 10);
         end if;

         Str (Str_Pos) := Ch;
         Str_Pos       := Str_Pos - 1;

         Rest := Rest / LBase;
         exit when Rest = 0;
      end loop;

      return Str (Str_Pos + 1 .. Str'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (V    : Unsigned_Long_Long;
      Base : Unsigned_Short_Short;
      Exp  : Integer)
     return String
   is
      New_Value : Unsigned_Long_Long;

   begin
      if Exp = 0 then
         if Base = 10 then
            --  decimal integer without exponent
            return Image (V);
         else
            --  based integer without exponent
            return Image (Base) & Base_Separator &
                   Image (V, Base) & Base_Separator;
         end if;
      else
         New_Value := Unsigned_Long_Long (Long_Long_Float (V) /
                                          Power (Integer (Base), Exp));
         if Base = 10 then
            --  decimal integer with exponent
            return Image (New_Value) &
                   Exp_Separator & Image (Exp);
         else
            --  based intgeger with exponent
            return Image (Base) & Base_Separator &
                   Image (New_Value, Base) & Base_Separator &
                   Exp_Separator & Image (Exp);
         end if;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (V : Long_Long_Float) return String is
      Str : String (1 .. 2 * Long_Long_Float'Digits + 2);
      --  Max digits = [+/-] Fore . Aft

   begin
      Ada.Long_Long_Float_Text_IO.Put (Str, V, Long_Long_Float'Digits, 0);
      return Remove_Ending_Zeros (Remove_Leading_Spaces (Str));
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (V    : Long_Long_Float;
      Base : Unsigned_Short_Short)
     return String
   is
      Str          : String (1 .. Fraction_Max_Digits);
      Sign         : Boolean;
      Integer_Part : Unsigned_Long_Long;
      Rest         : Long_Long_Float;
      Fraction     : Long_Long_Float;
      LBase        : constant Long_Long_Float := Long_Long_Float (Base);
      Digit        : Unsigned_Short_Short;
      Str_Len      : Integer := 0;
      Ch           : Character;

   begin
      if V < 0.0 then
         Sign := True;
         Rest := -V;
      else
         Sign := False;
         Rest := V;
      end if;

      Integer_Part := Unsigned_Long_Long (Long_Long_Float'Floor (Rest));
      Rest         := Rest - Long_Long_Float (Integer_Part);
      Fraction     := 1.0 / LBase;

      loop
         Digit := Unsigned_Short_Short
                     (Long_Long_Float'Truncation (Rest / Fraction));
         if Digit < 10 then
            Ch := Character'Val (Character'Pos ('0') + Digit);
         else
            Ch := Character'Val (Character'Pos ('A') + Digit - 10);
         end if;

         Str_Len       := Str_Len + 1;
         Str (Str_Len) := Ch;

         Rest     := Rest - Long_Long_Float (Digit) * Fraction;
         Fraction := Fraction / LBase;
         exit when Rest < Real_Epsilon or else Str_Len = Str'Last;
      end loop;

      if Sign then
         return Minus_Character & Image (Integer_Part, Base) &
                Real_Separator & Str (1 .. Str_Len);
      else
         return Image (Integer_Part, Base) &
                Real_Separator & Str (1 .. Str_Len);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (V    : Long_Long_Float;
      Base : Unsigned_Short_Short;
      Exp  : Integer)
     return String
   is
      New_Value : Long_Long_Float;

   begin
      if Exp = 0 then
         if Base = 10 then
            --  decimal real without exponent
            return Image (V);
         else
            --  based real without exponent
            return Image (Base) & Base_Separator &
                   Image (V, Base) & Base_Separator;
         end if;
      else
         New_Value := V / Power (Integer (Base), Exp);

         if Base = 10 then
            --  decimal real with exponent
            return Image (New_Value) &
                   Exp_Separator & Image (Exp);
         else
            --  based real with exponent
            return Image (Base) & Base_Separator &
                   Image (New_Value, Base) & Base_Separator &
                   Exp_Separator & Image (Exp);
         end if;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Kind : Node_Kind) return String is
      use Charset;

      S       : String := Node_Kind'Image (Kind);
      Capital : Boolean := False;

   begin
      To_Lower (S);
      for I in S'Range loop
         if S (I) = '_' then
            Capital := True;
         else
            if Capital then
               S (I) := Ada.Characters.Handling.To_Upper (S (I));
            end if;
            Capital := False;
         end if;
      end loop;

      return S (3 .. S'Last);
   end Image;

   -----------
   -- Power --
   -----------

   function Power (Base : Integer; Exp : Integer) return Long_Long_Float is
      Result : Long_Long_Float := 1.0;
      LBase  : constant Long_Long_Float := Long_Long_Float (Base);
      PExp   : Natural;

   begin
      PExp := abs Exp;
      for I in 1 .. PExp loop
         Result := Result * LBase;
      end loop;

      if Exp < 0 then
         return 1.0 / Result;
      else
         return Result;
      end if;
   end Power;

   -------------------------
   -- Remove_Ending_Zeros --
   -------------------------

   function Remove_Ending_Zeros (Str : String) return String is
      I : Integer;

   begin
      I := Str'Last;
      loop
         if Str (I) /= '0' then
            if Str (I) = '.' and then I < Str'Last then
               return Str (Str'First .. I + 1);
            else
               return Str (Str'First .. I);
            end if;
         end if;
         exit when I = Str'First;
         I := I - 1;
      end loop;
      return Str;
   end Remove_Ending_Zeros;

   ---------------------------
   -- Remove_Leading_Spaces --
   ---------------------------

   function Remove_Leading_Spaces (Str : String) return String is
   begin
      for I in Str'Range loop
         if Str (I) /= ' ' then
            return Str (I .. Str'Last);
         end if;
      end loop;
      return Str;
   end Remove_Leading_Spaces;

   ---------
   -- "*" --
   ---------

   function "*" (L : Value_Type; R : Value_Type) return Value_Type is
   begin
      case L.T is
         when LT_Integer =>
            case R.T is
               when LT_Integer =>
                  declare
                     Result : Value_Type (LT_Integer);
                  begin
                     Result.IBase := 10;
                     Result.ISign := Safe_XOR (L.ISign, R.ISign);
                     Result.IVal := L.IVal * R.IVal;
                     return Result;
                  end;

               when LT_Real =>
                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RSign := Safe_XOR (L.ISign, R.RSign);
                     Result.RExp := 0;
                     Result.RVal := Long_Long_Float (L.IVal) * R.RVal;
                     return Result;
                  end;

               when others =>
                  raise Constraint_Error;

            end case;

         when LT_Real =>
            case R.T is
               when LT_Integer =>
                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RSign := Safe_XOR (L.RSign, R.ISign);
                     Result.RExp := 0;
                     Result.RVal := L.RVal * Long_Long_Float (R.IVal);
                     return Result;
                  end;

               when LT_Real =>
                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RSign := Safe_XOR (L.RSign, R.RSign);
                     Result.RExp := 0;
                     Result.RVal := L.RVal * R.RVal;
                     return Result;
                  end;

               when others =>
                  raise Constraint_Error;
            end case;

         when others =>
            raise Constraint_Error;
      end case;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L : Value_Type; R : Value_Type) return Value_Type is
   begin
      case L.T is
         when LT_Integer =>
            case R.T is
               when LT_Integer =>
                  if R.IVal = 0 then
                     raise Constraint_Error with "Division by zero";
                  end if;

                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RBase := 10;
                     Result.RSign := Safe_XOR (L.ISign, R.ISign);
                     Result.RVal := Long_Long_Float (L.IVal) /
                       Long_Long_Float (R.IVal);
                     Result.Rexp := 0;
                     return Result;
                  end;

               when LT_Real =>
                  if R.RVal = 0.0 then
                     raise Constraint_Error with "Division by zero";
                  end if;

                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RBase := 10;
                     Result.RSign := Safe_XOR (L.ISign, R.RSign);
                     Result.RExp := 0;
                     Result.RVal := Long_Long_Float (L.IVal) / R.RVal;
                     return Result;
                  end;

               when others =>
                  raise Constraint_Error;

            end case;

         when LT_Real =>
            case R.T is
               when LT_Integer =>
                  if R.IVal = 0 then
                     raise Constraint_Error with "Division by zero";
                  end if;

                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RBase := 10;
                     Result.RSign := Safe_XOR (L.RSign, R.ISign);
                     Result.RExp := 0;
                     Result.RVal := L.RVal / Long_Long_Float (R.IVal);
                     return Result;
                  end;

               when LT_Real =>
                  if R.RVal = 0.0 then
                     raise Constraint_Error with "Division by zero";
                  end if;

                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RBase := 10;
                     Result.RSign := Safe_XOR (L.RSign, R.RSign);
                     Result.RExp := 0;
                     Result.RVal := L.RVal / R.RVal;
                     Result.Rexp := 1;
                     return Result;
                  end;

               when others =>
                  raise Constraint_Error;
            end case;

         when others =>
            raise Constraint_Error;
      end case;
   end "/";

   ---------
   -- "-" --
   ---------

   function "-" (L : Value_Type; R : Value_Type) return Value_Type is
      T : Value_Type := R;
   begin
      case T.T is
         when LT_Integer =>
            T.ISign := not R.ISign;

         when LT_Real =>
            T.RSign := not R.RSign;

         when others =>
            raise Constraint_Error;
      end case;

      return (L + T);

   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (L : Value_Type; R : Value_Type) return Value_Type is
   begin
      case L.T is
         when LT_Integer =>
            case R.T is
               when LT_Integer =>
                  declare
                     Result : Value_Type (LT_Integer);
                  begin
                     Result.IBase := 10;
                     Result.IExp := 0;

                     --  |L| + -|R|
                     if R.ISign and then not L.ISign then

                        if L.IVal >= R.IVal then
                           Result.IVal := L.IVal - R.IVal;
                           Result.ISign := False;
                        else
                           Result.IVal := R.IVal - L.IVal;
                           Result.ISign := True;
                        end if;

                        --  -|L| + -|R|
                     elsif R.ISign and then L.ISign then
                        Result.IVal := R.IVal + L.IVal;
                        Result.ISign := True;

                        --  |L| + |R|
                     elsif not R.ISign and then not L.ISign then
                        Result.IVal := R.IVal + L.IVal;
                        Result.ISign := False;

                        --  -|L| + |R|
                     else
                        if R.IVal >= L.IVal then
                           Result.IVal := R.IVal - L.IVal;
                           Result.ISign := False;
                        else
                           Result.IVal := L.IVal - R.IVal;
                           Result.ISign := True;
                        end if;
                     end if;
                     return Result;
                  end;

               when LT_Real =>
                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RBase := 10;
                     Result.RExp := 0;

                     --  |L| + -|R|
                     if R.RSign and then not L.ISign then

                        if Long_Long_Float (L.IVal) >= R.RVal then
                           Result.RVal := Long_Long_Float (L.IVal) - R.RVal;
                           Result.RSign := False;
                        else
                           Result.RVal := R.RVal - Long_Long_Float (L.IVal);
                           Result.RSign := True;
                        end if;

                        --  -|L| + -|R|
                     elsif R.RSign and then L.ISign then
                        Result.RVal := R.RVal + Long_Long_Float (L.IVal);
                        Result.RSign := True;

                        --  |L| + |R|
                     elsif not R.RSign and then not L.ISign then
                        Result.RVal := R.RVal + Long_Long_Float (L.IVal);
                        Result.RSign := False;

                        --  -|L| + |R|
                     else
                        if R.RVal >= Long_Long_Float (L.IVal) then
                           Result.RVal := R.RVal - Long_Long_Float (L.IVal);
                           Result.RSign := False;
                        else
                           Result.RVal := Long_Long_Float (L.IVal) - R.RVal;
                           Result.RSign := True;
                        end if;
                     end if;
                     return Result;
                  end;

               when others =>
                  raise Constraint_Error;

            end case;

         when LT_Real =>
            case R.T is
               when LT_Integer =>
                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RBase := 10;
                     Result.RExp := 0;

                     --  |L| + -|R|
                     if R.ISign and then not L.RSign then

                        if L.RVal >= Long_Long_Float (R.IVal) then
                           Result.RVal := L.RVal - Long_Long_Float (R.IVal);
                           Result.RSign := False;
                        else
                           Result.RVal := Long_Long_Float (R.IVal) - L.RVal;
                           Result.RSign := True;
                        end if;

                        --  -|L| + -|R|
                     elsif R.ISign and then L.RSign then
                        Result.RVal := Long_Long_Float (R.IVal) + L.RVal;
                        Result.RSign := True;

                        --  |L| + |R|
                     elsif not R.ISign and then not L.RSign then
                        Result.RVal := Long_Long_Float (R.IVal) + L.RVal;
                        Result.RSign := False;

                        --  -|L| + |R|
                     else
                        if Long_Long_Float (R.IVal) >= L.RVal then
                           Result.RVal := Long_Long_Float (R.IVal) - L.RVal;
                           Result.RSign := False;
                        else
                           Result.RVal := L.RVal - Long_Long_Float (R.IVal);
                           Result.RSign := True;
                        end if;
                     end if;
                     return Result;
                  end;

               when LT_Real =>
                  declare
                     Result : Value_Type (LT_Real);
                  begin
                     Result.RBase := 10;
                     Result.RExp := 0;

                     --  |L| + -|R|
                     if R.RSign and then not L.RSign then

                        if L.RVal >= R.RVal then
                           Result.RVal := L.RVal - R.RVal;
                           Result.RSign := False;
                        else
                           Result.RVal := R.RVal - L.RVal;
                           Result.RSign := True;
                        end if;

                        --  -|L| + -|R|
                     elsif R.RSign and then L.RSign then
                        Result.RVal := R.RVal + L.RVal;
                        Result.RSign := True;

                        --  |L| + |R|
                     elsif not R.RSign and then not L.RSign then
                        Result.RVal := R.RVal + L.RVal;
                        Result.RSign := False;

                        --  -|L| + |R|
                     else
                        if R.RVal >= L.RVal then
                           Result.RVal := R.RVal - L.RVal;
                           Result.RSign := False;
                        else
                           Result.RVal := L.RVal - R.RVal;
                           Result.RSign := True;
                        end if;
                     end if;
                     return Result;
                  end;

               when others =>
                  raise Constraint_Error;
            end case;

         when LT_List =>
            case R.T is
               when LT_List =>
                  declare
                     use Locations;
                     use Ocarina.ME_AO4AADL.AO4AADL_Tree.Nutils;

                     Result : Value_Type;
                     Lst    : constant List_Id :=
                       New_List (K_List_Id, No_Location);
                  begin
                     if Is_Empty (L.LVal) then
                        Result := Value_Type'(LT_List, R.LVal);
                     elsif Is_Empty (R.LVal) then
                        Result := Value_Type'(LT_List, L.LVal);
                     else
                        Set_First_Node (Lst, First_Node (L.LVal));
                        Set_Next_Node (Last_Node (L.LVal),
                                       First_Node (R.LVal));
                        Set_Last_Node (Lst, Last_Node (R.LVal));
                        Result := Value_Type'(LT_List, Lst);
                     end if;
                     return Result;
                  end;

               when others =>
                  raise Constraint_Error;
            end case;

         when others =>
            raise Constraint_Error;
      end case;
   end "+";

   ---------
   -- "=" --
   ---------

   function "=" (L : Value_Type; R : Value_Type) return Boolean is
   begin
      case L.T is
         when LT_Integer =>
            case R.T is
               when LT_Integer =>
                  return (L.IVal = R.IVal) and then (L.ISign = R.ISign);

               when LT_Real =>
                  return (Long_Long_Float (L.IVal) = R.RVal)
                    and then (L.ISign = R.RSign);

               when others =>
                  raise Constraint_Error;

            end case;

         when LT_Real =>
            case R.T is
               when LT_Integer =>
                  return (L.RVal = Long_Long_Float (R.IVal))
                    and then (L.RSign = R.ISign);

               when LT_Real =>
                  return (L.RVal = R.RVal) and then (L.RSign = R.RSign);

               when others =>
                  raise Constraint_Error;
            end case;

         when others =>
            raise Constraint_Error;
      end case;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L : Value_Type; R : Value_Type) return Boolean is
      Sign_L : Boolean;
      Sign_R : Boolean;
      Result : Boolean;
   begin
      --  Compare absolute values

      case L.T is
         when LT_Integer =>
            case R.T is
               when LT_Integer =>
                  Sign_L := L.ISign;
                  Sign_R := R.ISign;
                  Result := L.IVal < R.IVal;

               when LT_Real =>
                  Sign_L := L.ISign;
                  Sign_R := R.RSign;
                  Result := Long_Long_Float (L.IVal) < R.RVal;

               when others =>
                  raise Constraint_Error;

            end case;

         when LT_Real =>
            case R.T is
               when LT_Integer =>
                  Sign_L := L.RSign;
                  Sign_R := R.ISign;
                  Result := L.RVal < Long_Long_Float (R.IVal);

               when LT_Real =>
                  Sign_L := L.RSign;
                  Sign_R := R.RSign;
                  Result := L.RVal < R.RVal;

               when others =>
                  raise Constraint_Error;
            end case;

         when others =>
            raise Constraint_Error;
      end case;

      --  Take signs into account

      if Sign_L then
         if Sign_R then
            return not Result;
         else
            return True;
         end if;
      else
         if Sign_R then
            return False;
         else
            return Result;
         end if;
      end if;
   end "<";

end Ocarina.AO4AADL_Values;
