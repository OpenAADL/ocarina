------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . A A D L _ V A L U E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2004-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

--  This package contains the functions related to value management
--  inside the Ocarina tree.

with Ocarina.ME_AADL.AADL_Tree.Nodes; use Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.Types;                           use Ocarina.Types;

package Ocarina.AADL_Values is

   type Literal_Type is
     (LT_Integer, LT_Real, LT_String, LT_Boolean, LT_Enumeration);

   type Value_Type (T : Literal_Type := LT_Integer) is record
      case T is
         when LT_Integer =>
            IVal  : Unsigned_Long_Long;   --  Absolute value
            ISign : Boolean;              --  True => Negative
            IBase : Unsigned_Short_Short; --  For printing purpose only
            IExp  : Integer;              --  For printing purpose only
         when LT_Real =>
            RVal  : Long_Long_Float;      --  Absolute value
            RSign : Boolean;              --  True => Negative
            RBase : Unsigned_Short_Short; --  For printing purpose only
            RExp  : Integer;              --  For printing purpose only
         when LT_String =>
            SVal : Name_Id;
         when LT_Boolean =>
            BVal : Boolean;
         when LT_Enumeration =>
            EVal : Name_Id;
      end case;
   end record;

   function Extract_Value (Value : Value_Type) return Long_Long is
     (if Value.ISign then -Long_Long (Value.IVal) else Long_Long (Value.IVal));

   function Extract_Value (Value : Value_Type) return Unsigned_Long_Long is
     (if Value.ISign then raise Constraint_Error else Value.IVal);

   function Extract_Value (Value : Value_Type) return Long_Double is
     (if Value.RSign then -Long_Double (Value.RVal)
      else Long_Double (Value.RVal));

   function Extract_Value (Value : Value_Type) return Name_Id is
      (if Value.T = LT_String then Value.SVal else Value.EVal);

   function Extract_Value (Value : Value_Type) return Boolean is (Value.BVal);

   No_Value : constant Value_Id;
   V_Zero : Value_Id;
   V_One  : Value_Id;

   function Get_Value_Type (Value : Value_Id) return Value_Type;

   function New_Boolean_Value (Value : Boolean) return Value_Id;

   function New_Real_Value
     (Value    : Long_Long_Float;
      Negative : Boolean              := False;
      Base     : Unsigned_Short_Short := 10;
      Exp      : Integer              := 0) return Value_Id;

   function New_Integer_Value
     (Value    : Unsigned_Long_Long;
      Negative : Boolean              := False;
      Base     : Unsigned_Short_Short := 10;
      Exp      : Integer              := 0) return Value_Id;

   function New_String_Value (Value : Name_Id) return Value_Id;

   function New_Enum_Value (Value : Name_Id) return Value_Id;

   function New_Value (Value : Value_Type) return Value_Id;
   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function Image (Value : Value_Type; Quoted : Boolean := True) return String;
   function Image (Value : Value_Id; Quoted : Boolean := True) return String;
   --  Return the image of the given value. These routines edit the
   --  name buffer.

   function Power (Base : Integer; Exp : Integer) return Long_Long_Float;
   --  Return (Base ** Exp)

   function Image (V : Unsigned_Short_Short) return String;
   pragma Inline (Image);
   function Image (V : Integer) return String;
   pragma Inline (Image);
   function Image (V : Unsigned_Long_Long) return String;
   pragma Inline (Image);
   function Image (V : Long_Long_Float) return String;
   pragma Inline (Image);
   --  Call Remove_Leading_Spaces (XXXXXX'Image (V))

   function Image
     (V    : Long_Long_Float;
      Base : Unsigned_Short_Short;
      Exp  : Integer) return String;

   function Image
     (V    : Unsigned_Long_Long;
      Base : Unsigned_Short_Short;
      Exp  : Integer) return String;

   function Image (Kind : Node_Kind) return String;
   --  Return corresponding string of node kind

   function "*" (L : Value_Type; R : Value_Type) return Value_Type;
   --  Return L * R (accept Integer * Real)

   function "=" (L : Value_Type; R : Value_Type) return Boolean;
   function "<" (L : Value_Type; R : Value_Type) return Boolean;
   --  Accept comparison between integer and real

   procedure Reset;
   --  Reset the value table

private

   No_Value : constant Value_Id := 0;

end Ocarina.AADL_Values;
