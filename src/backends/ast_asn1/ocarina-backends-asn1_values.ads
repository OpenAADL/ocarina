------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . A S N 1 _ V A L U E S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2012-2015 ESA & ISAE.                    --
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

with Ocarina.Backends.ASN1_Tree.Nodes; use Ocarina.Backends.ASN1_Tree.Nodes;

package Ocarina.Backends.ASN1_Values is

   type Value_Type (K : Node_Kind := K_Int) is record
      case K is
         when K_Int =>
            IVal : Unsigned_Long_Long;
            Sign : Short_Short;
            Base : Unsigned_Short_Short;

         when K_Float =>
            FVal : Long_Double;

         when K_Char =>
            CVal : Unsigned_Short;

         when K_String =>
            PCVal : Name_Id;

         when others =>
            null;
      end case;
   end record;

   Bad_Value : constant Value_Type;
   No_Value : constant Value_Id;

   function New_Floating_Point_Value (Value : Long_Double) return Value_Id;

   function New_Int_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short) return Value_Id;

   function New_Char_Value (Value : Unsigned_Short) return Value_Id;

   function New_String_Value (Value : Name_Id) return Value_Id;

   function New_Value (Value : Value_Type) return Value_Id;

   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function Image (Value : Value_Id) return String;

   function "not" (R : Value_Type) return Value_Type;
   function "-" (R : Value_Type) return Value_Type;
   function "-" (L, R : Value_Type) return Value_Type;
   function "+" (L, R : Value_Type) return Value_Type;
   function "mod" (L, R : Value_Type) return Value_Type;
   function "/" (L, R : Value_Type) return Value_Type;
   function "*" (L, R : Value_Type) return Value_Type;
   function "and" (L, R : Value_Type) return Value_Type;
   function "or" (L, R : Value_Type) return Value_Type;
   function "xor" (L, R : Value_Type) return Value_Type;
   function Shift_Left (L, R : Value_Type) return Value_Type;
   function Shift_Right (L, R : Value_Type) return Value_Type;

   function "<" (L, R : Value_Type) return Boolean;
   --  Assume L and R have the same type.

   procedure Reset;
   --  Reset the value table

   function To_ASN1_Value (V : Value_Id) return Value_Id;

private

   Bad_Value : constant Value_Type := Value_Type'((K => K_Node_Id));
   No_Value  : constant Value_Id   := 0;

end Ocarina.Backends.ASN1_Values;
