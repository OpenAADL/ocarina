------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . R E A L _ V A L U E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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

with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Types;

package Ocarina.REAL_Values is
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Types;

   type Literal_Type is (LT_Integer, LT_Real, LT_String, LT_Boolean,
                         LT_Enumeration, LT_List, LT_Range, LT_Element);

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
         when LT_List =>
            LVal : List_Id;
         when LT_Element =>     --  Element of a set
            ELVal : Node_Id;
         when LT_Range =>
            RVal_Left  : Long_Long_Float;      --  Absolute value
            RSign_Left : Boolean;              --  True => Negative
            RVal_Right  : Long_Long_Float;      --  Absolute value
            RSign_Right : Boolean;              --  True => Negative
            RVBase : Unsigned_Short_Short; --  For printing purpose only
            RVExp  : Integer;              --  For printing purpose only
      end case;
   end record;

   No_Value : constant Value_Id;
   V_Zero   : Value_Id;
   V_One    : Value_Id;

   function Get_Value_Type (Value : Value_Id) return Value_Type;

   function New_Boolean_Value (Value : Boolean) return Value_Id;

   function New_Range_Value
     (LValue    : Long_Long_Float;
      RValue    : Long_Long_Float;
      LNegative : Boolean              := False;
      RNegative : Boolean              := False;
      Base      : Unsigned_Short_Short := 10;
      Exp       : Integer              := 0)
     return Value_Id;

   function New_List_Value (Value : List_Id) return Value_Id;

   function New_Real_Value
     (Value    : Long_Long_Float;
      Negative : Boolean              := False;
      Base     : Unsigned_Short_Short := 10;
      Exp      : Integer              := 0)
     return Value_Id;

   function New_Integer_Value
     (Value    : Unsigned_Long_Long;
      Negative : Boolean              := False;
      Base     : Unsigned_Short_Short := 10;
      Exp      : Integer              := 0)
     return Value_Id;

   function New_String_Value (Value : Name_Id) return Value_Id;

   function New_Enum_Value (Value : Name_Id) return Value_Id;

   function New_Elem_Value (Value : Node_Id) return Value_Id;

   function New_Value (Value : Value_Type) return Value_Id;
   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function AADL_Value (V : Value_Id) return Value_Id;

   function Image
     (Value  : Value_Type;
      Quoted : Boolean    := True)
     return String;
   function Image
     (Value  : Value_Id;
      Quoted : Boolean  := True)
     return String;

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
      Exp  : Integer)
     return String;

   function Image
     (V    : Unsigned_Long_Long;
      Base : Unsigned_Short_Short;
      Exp  : Integer)
     return String;

   function Image (Kind : Node_Kind) return String;
   --  Return corresponding string of node kind

   function "*" (L : Value_Type; R : Value_Type) return Value_Type;
   --  Return L * R (accept Integer * Real)

   function "/" (L : Value_Type; R : Value_Type) return Value_Type;
   --  Return L / R (accept Integer * Real)

   function "mod" (L : Value_Type; R : Value_Type) return Value_Type;
   --  Return L modulo R (accept only Integer)

   function "+" (L : Value_Type; R : Value_Type) return Value_Type;
   --  Return L + R (accept Integer * Real)

   function "-" (L : Value_Type; R : Value_Type) return Value_Type;
   --  Return L - R (accept Integer * Real)

   function "=" (L : Value_Type; R : Value_Type) return Boolean;
   --  Return L = R iff type (L) = Type (R)

   function "<" (L : Value_Type; R : Value_Type) return Boolean;
   --  Accept comparison between integer and real

   function Power (Base : Value_Type; Exp : Value_Type) return Value_Type;
   --  Return Base**Exp, accept only integers yet

   procedure Reset;
   --  Reset the value table

private
   No_Value : constant Value_Id := 0;

end Ocarina.REAL_Values;
