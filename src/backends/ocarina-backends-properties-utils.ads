------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P R O P E R T I E S . U T I L S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
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

with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with Ocarina.AADL_Values;         use Ocarina.AADL_Values;
with Ocarina.Backends.Properties; use Ocarina.Backends.Properties;

package Ocarina.Backends.Properties.Utils is

   --  This package proposes various generic accessor to ease the
   --  retrieval of property values.

   --  This high-level accessor return a GNAT.OS_Lib.String_List made
   --  of the name of the property in lower case, followed by a list
   --  of strings representing the property value. E.g.
   --
   --  ("source_stack_size", "13952 Bytes")
   --  ("Deadline", "500 Ms")
   --  ("Compute_Execution_Time", "0 Ms .. 3 Ms")
   --  ("Dispatch_Offset", "100 Ms")
   --  ("Period", "500 Ms")
   --  ("Dispatch_Protocol", "Periodic")

   function Check_And_Get_Property
     (E         : Node_Id;
      Prop_Name : Name_Id) return String_List;

   function Check_And_Get_Property
     (E        : Node_Id;
      Property : Node_Id) return String_List;

   --  The following accessors take as parameters the following entities
   --  * E: entity from the instance tree for which we look for a property
   --  * Property_Name: name of the property we are looking for, in
   --    lower case

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Unsigned_Long_Long := 0) return Unsigned_Long_Long;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Name_Id := No_Name) return Name_Id;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Node_Id := No_Node) return Node_Id;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id;
      Default_Value : List_Id := No_List) return List_Id;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Boolean := False) return Boolean;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id;
      Names         : Name_Array;
      Default_Value : Int := Int'First) return Int;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id) return Name_Array;

   generic
      type Elt_Type is private;
      type Elt_Array is array (Nat range <>) of Elt_Type;
      with function Extract_Value (V : Value_Type) return Elt_Type;
      Default_Value : Elt_Array;

   function Check_And_Get_Property_Generic
     (E             : Node_Id;
      Property_Name : Name_Id) return Elt_Array;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id) return ULL_Array;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id) return LL_Array;

   function Check_And_Get_Property
     (E             : Node_Id;
      Property_Name : Name_Id) return LD_Array;

   generic
      type T is (<>);
   function Check_And_Get_Property_Enumerator
     (E             : Node_Id;
      Property_Name : Name_Id) return T;

   generic
      type T is (<>);
      Default_Value : T;
   function Check_And_Get_Property_Enumerator_With_Default
     (E             : Node_Id;
      Property_Name : Name_Id) return T;

end Ocarina.Backends.Properties.Utils;
