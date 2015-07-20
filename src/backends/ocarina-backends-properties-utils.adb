------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P R O P E R T I E S . U T I L S     --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Backends.Messages;
with Ocarina.Instances.Queries;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Namet;
with Utils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.NUtils;
with Ocarina.AADL_Values;

package body Ocarina.Backends.Properties.Utils is

   use Ocarina.Backends.Messages;
   use Ocarina.Instances.Queries;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Namet;
   use Standard.Utils;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.NUtils;
   use type ATN.Node_Kind;

   function SA (S : String) return String_Access is (new String'(S));

   -----------------------------------------
   -- Print_Value_Of_Property_Association --
   -----------------------------------------

   function Print_Value_Of_Property_Association
     (AADL_Property_Value : Node_Id)
     return String;

   function Print_Value_Of_Property_Association
     (AADL_Property_Value : Node_Id)
     return String is
   begin
      if Present (AADL_Property_Value)
        and then ATN.Kind (AADL_Property_Value) = ATN.K_Signed_AADLNumber
      then
         return Ocarina.AADL_Values.Image
           (ATN.Value
              (ATN.Number_Value
                 (AADL_Property_Value)))
           & " "
           & (if Present (ATN.Unit_Identifier (AADL_Property_Value))
                then Get_Name_String
           (ATN.Display_Name
              (ATN.Unit_Identifier (AADL_Property_Value)))
                else "");

      elsif Present (AADL_Property_Value)
        and then ATN.Kind (AADL_Property_Value) = ATN.K_Literal
      then
         --  This property value denotes a literal

         return Ocarina.AADL_Values.Image
           (ATN.Value (AADL_Property_Value), Quoted => False);

      elsif Present (AADL_Property_Value)
        and then ATN.Kind (AADL_Property_Value) = ATN.K_Reference_Term
      then
         --  This property value denotes a reference term

         return Get_Name_String (ATN.Display_Name
                                   (ATN.First_Node --  XXX must iterate
                                      (ATN.List_Items
                                         (ATN.Reference_Term
                                            (AADL_Property_Value)))));

      elsif Present (AADL_Property_Value)
        and then ATN.Kind (AADL_Property_Value) = ATN.K_Enumeration_Term
      then
            --  This property value denotes an enumeration term

            return Get_Name_String (ATN.Display_Name
                                      (ATN.Identifier (AADL_Property_Value)));

      elsif Present (AADL_Property_Value)
        and then ATN.Kind (AADL_Property_Value) = ATN.K_Number_Range_Term
      then
         --  This property value denotes a number range term

         return Ocarina.AADL_Values.Image
           (ATN.Value
              (ATN.Number_Value
                 (ATN.Lower_Bound
                    (AADL_Property_Value))))
           &
           (if Present
              (ATN.Unit_Identifier
                 (ATN.Lower_Bound (AADL_Property_Value)))
              then
           " " & Get_Name_String
           (ATN.Display_Name
              (ATN.Unit_Identifier
                 (ATN.Lower_Bound (AADL_Property_Value))))
              else "")
                & " .. "
                & Ocarina.AADL_Values.Image
                (ATN.Value
                   (ATN.Number_Value
                      (ATN.Upper_Bound
                         (AADL_Property_Value))))
                &
                (if Present
                   (ATN.Unit_Identifier
                      (ATN.Upper_Bound (AADL_Property_Value)))
                   then
                " " & Get_Name_String
                (ATN.Display_Name
                   (ATN.Unit_Identifier
                   (ATN.Upper_Bound (AADL_Property_Value))))
                   else "");
      end if;

      raise Program_Error;
   end Print_Value_Of_Property_Association;

   ----------------------------
   -- Check_And_Get_Property --
   ----------------------------

   function Check_And_Get_Property
     (E : Node_Id;
      Property : Node_Id)
     return String_List
   is
      Prop_Name : constant Name_Id
        := Standard.Utils.To_Lower (Display_Name (Identifier (Property)));
   begin
      return Check_And_Get_Property (E, Prop_Name);
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Prop_Name : Name_Id)
     return String_List
   is
      A : constant String_Access
        := new String'(Get_Name_String (Prop_Name));
      AADL_Property_Value : Node_Id;

   begin
      if Is_Defined_String_Property (E, Prop_Name) then
         return
           A & SA (Get_Name_String (Get_String_Property (E, Prop_Name)));

      elsif (Is_Defined_Integer_Property (E, Prop_Name)
               or else Is_Defined_Float_Property (E, Prop_Name))
      then
         AADL_Property_Value :=
           Get_Value_Of_Property_Association (E, Prop_Name);
         return
           A & SA (Print_Value_Of_Property_Association (AADL_Property_Value));

      elsif Is_Defined_Boolean_Property (E, Prop_Name) then
         return
           A & SA (Boolean'Image (Get_Boolean_Property (E, Prop_Name)));

      elsif Is_Defined_Reference_Property (E, Prop_Name) then
         return A & SA (Get_Reference_Property (E, Prop_Name)'Img);

      elsif Is_Defined_Classifier_Property (E, Prop_Name) then
         return A & SA (Get_Classifier_Property (E, Prop_Name)'Img);

      elsif Is_Defined_Range_Property (E, Prop_Name) then
         AADL_Property_Value := Get_Range_Property (E, Prop_Name);
         return A
           & SA (Print_Value_Of_Property_Association (AADL_Property_Value));

      elsif Is_Defined_List_Property (E, Prop_Name) then
         declare
            It : Node_Id :=
              ATN.First_Node (Get_List_Property (E, Prop_Name));
            B : String_List
              (1 .. ATNU.Length (Get_List_Property (E, Prop_Name)));
            J : Integer := B'First;
         begin
            while Present (It) loop
               B (J) := new String'(Print_Value_Of_Property_Association (It));
               J := J + 1;
               It := ATN.Next_Node (It);
            end loop;
            return A & B;
         end;

      elsif Is_Defined_Enumeration_Property (E, Prop_Name) then
         return A & SA (Get_Name_String
                          (Get_Enumeration_Property (E, Prop_Name)));
      end if;

      return A & SA (" KO");
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Unsigned_Long_Long := 0)
  return Unsigned_Long_Long is
   begin
      if Is_Defined_Integer_Property (E, Property_Name) then
         return Get_Integer_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Name_Id := No_Name)
     return Name_Id is
   begin
      if Is_Defined_String_Property (E, Property_Name) then
         return Get_String_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Node_Id := No_Node)
     return Node_Id is
   begin
      if Is_Defined_Property (E, Property_Name) then
         return Get_Classifier_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : List_Id := No_List)
     return List_Id is
   begin
      if Is_Defined_List_Property (E, Property_Name) then
         return Get_List_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Default_Value : Boolean := False)
     return Boolean is
   begin
      if Is_Defined_Boolean_Property (E, Property_Name) then
         return Get_Boolean_Property (E, Property_Name);
      else
         return Default_Value;
      end if;
   end Check_And_Get_Property;

   function Check_And_Get_Property
     (E : Node_Id;
      Property_Name : Name_Id;
      Names : Name_Array;
      Default_Value : Int := Int'First)
     return Int
   is
      P_Name : Name_Id;
   begin
      if not Is_Defined_Enumeration_Property (E, Property_Name) then
         return Default_Value;
      end if;

      P_Name := Get_Enumeration_Property (E, Property_Name);
      for J in Names'Range loop
         if P_Name = Names (J) then
            return J;
         end if;
      end loop;

      Display_Located_Error
        (Loc (E),
         "Unknown enumerator " & Get_Name_String (P_Name),
         Fatal => True);
      return Default_Value;
   end Check_And_Get_Property;

end Ocarina.Backends.Properties.Utils;
