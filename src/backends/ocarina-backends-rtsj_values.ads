------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . R T S J _ V A L U E S          --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.Backends.RTSJ_Tree.Nodes; use Ocarina.Backends.RTSJ_Tree.Nodes;

package Ocarina.Backends.RTSJ_Values is

   type Value_Type (K : Node_Kind := K_Int) is record
      case K is
         when K_Int =>
            IVal : Unsigned_Long_Long;
            Sign : Short_Short;
            Base : Unsigned_Short_Short;

         when K_String =>
            PCVal : Name_Id;

         when others =>
            null;

      end case;
   end record;

   Bad_Value : constant Value_Type;
   No_Value : constant Value_Id;

   procedure Reset;

   function New_Int_Value
     (Value : Unsigned_Long_Long;
      Sign  : Short_Short;
      Base  : Unsigned_Short_Short) return Value_Id;

   function New_String_Value (Value : Name_Id) return Value_Id;

   function New_Value (Value : Value_Type) return Value_Id;

   function Get_Value (V : Value_Id) return Value_Type;
   procedure Set_Value (V : Value_Id; X : Value_Type);

   --  Converts a Value_Type into String
   function Image (Value : Value_Id) return String;

   --  Converts an AADL value into a Java value
   function To_RTSJ_Value (V : Value_Id) return Value_Id;

private

   Bad_Value : constant Value_Type := Value_Type'((K => K_Node_Id));
   No_Value  : constant Value_Id   := 0;

end Ocarina.Backends.RTSJ_Values;
