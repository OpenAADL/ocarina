------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . A A D L _ V A L U E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2004-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

--  This package contains the functions related to value management
--  inside the Ocarina tree.

with Ocarina.Me_AADL.AADL_Tree.Nodes; use Ocarina.Me_AADL.AADL_Tree.Nodes;
with Ocarina.Types;         use Ocarina.Types;

package Ocarina.AADL_Values is

   type Literal_Type is (LT_Integer, LT_Real, LT_String, LT_Boolean,
                         LT_Enumeration);

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

   No_Value : constant Value_Id;
   V_Zero  :  Value_Id;
   V_One   :  Value_Id;

   function Get_Value_Type (Value : Value_Id) return Value_Type;

   function New_Boolean_Value (Value : Boolean) return Value_Id;

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

   function New_Value (Value : Value_Type) return Value_Id;
   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function Image
     (Value  : Value_Type;
      Quoted : Boolean    := True)
     return String;
   function Image
     (Value  : Value_Id;
      Quoted : Boolean  := True)
     return String;
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

   function "=" (L : Value_Type; R : Value_Type) return Boolean;
   function "<" (L : Value_Type; R : Value_Type) return Boolean;
   --  Accept comparison between integer and real

   procedure Reset;
   --  Reset the value table

private

   No_Value : constant Value_Id := 0;

end Ocarina.AADL_Values;
