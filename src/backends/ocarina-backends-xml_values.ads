------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . X M L _ V A L U E S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

with Ocarina.Backends.XML_Tree.Nodes; use Ocarina.Backends.XML_Tree.Nodes;

with Ada.Text_IO;

package Ocarina.Backends.XML_Values is

   package FLT_IO is new Ada.Text_IO.Float_IO (Float);

   type Value_Type (K : Node_Kind := K_Numeric) is
      record
         case K is
            when K_Numeric =>
               IVal : Unsigned_Long_Long;
               Sign : Short_Short;
               Base : Unsigned_Short_Short;

            when K_String =>
               PCVal : Name_Id;

            when K_Float =>
               FVal : Long_Double;

            when others =>
               null;
         end case;
      end record;

   Bad_Value : constant Value_Type;
   No_Value  : constant Value_Id;

   function New_Numeric_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short)
     return Value_Id;

   function New_String_Value
     (Value : Name_Id)
     return Value_Id;

   function New_Value
     (Value : Value_Type)
     return Value_Id;

   function Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   function Image (Value : Value_Id) return String;

   function To_XML_Value (V : Value_Id) return Value_Id;
   --  Converts an AADL value to an XML one

   function "not" (R : Value_Type) return Value_Type;
   function "-"   (R : Value_Type) return Value_Type;
   function "-"   (L, R : Value_Type) return Value_Type;
   function "+"   (L, R : Value_Type) return Value_Type;
   function "mod" (L, R : Value_Type) return Value_Type;
   function "/"   (L, R : Value_Type) return Value_Type;
   function "*"   (L, R : Value_Type) return Value_Type;
   function "and" (L, R : Value_Type) return Value_Type;
   function "or"  (L, R : Value_Type) return Value_Type;
   function "xor" (L, R : Value_Type) return Value_Type;
   function Shift_Left  (L, R : Value_Type) return Value_Type;
   function Shift_Right (L, R : Value_Type) return Value_Type;

   function "<"   (L, R : Value_Type) return Boolean;
   --  Assume L and R have the same type.

   procedure Reset;
   --  Reset the value table

   function New_Floating_Point_Value
     (Value : Long_Double) return Value_Id;

private

   Bad_Value : constant Value_Type := Value_Type'((K => K_Node_Id));
   No_Value  : constant Value_Id := 0;

end Ocarina.Backends.XML_Values;
