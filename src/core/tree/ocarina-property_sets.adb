------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . P R O P E R T Y _ S E T S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Charset; use Charset;
with Ocarina.Namet;   use Ocarina.Namet;

with Ocarina.ME_AADL.AADL_Tree.Nodes; use Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Property_Sets is

   AADLv1_Std_Property_Sets : aliased Standard_Property_Set_Array_Type :=
     (S_AADL_Project, S_AADL_Properties);

   AADLv2_Std_Property_Sets : aliased Standard_Property_Set_Array_Type :=
     (S_AADL_Project,
      S_Deployment_Properties,
      S_Thread_Properties,
      S_Communication_Properties,
      S_Memory_Properties,
      S_Modeling_Properties,
      S_Timing_Properties,
      S_Programming_Properties);

   Standard_Property_Sets_Table : constant array
   (AADL_Version_Type) of Standard_Property_Set_Array_Access :=
     (AADL_V1 => AADLv1_Std_Property_Sets'Access,
      AADL_V2 => AADLv2_Std_Property_Sets'Access);

   AADLv1_Ocarina_Property_Sets : aliased Ocarina_Property_Set_Array_Type :=
     (O_Data_Model,
      O_Deployment,
      O_Cheddar_Properties,
      O_Ocarina_Config,
      O_Base_Types,
      O_ASSERT_Types,
      O_ASSERT_Properties);

   AADLv2_Ocarina_Property_Sets : aliased Ocarina_Property_Set_Array_Type :=
     (O_Data_Model,
      O_Deployment,
      O_Cheddar_Properties,
      O_Ocarina_Config,
      O_Transformations,
      O_POK_Properties,
      O_Base_Types,
      O_Taste_Properties,
      O_ARINC653,
      O_ASSERT_Properties);

   Ocarina_Property_Sets_Table : constant array
   (AADL_Version_Type) of Ocarina_Property_Set_Array_Access :=
     (AADL_V1 => AADLv1_Ocarina_Property_Sets'Access,
      AADL_V2 => AADLv2_Ocarina_Property_Sets'Access);

   User_Defined_Mask : constant Byte := 2**1;

   Standard_Property_Set_Mask : constant Byte := 2**2;
   Ocarina_Property_Set_Mask  : constant Byte := 2**3;

   function Internal_Name (N : Name_Id) return Name_Id;
   function Internal_Name (S : String) return Name_Id;

   -----------
   -- Image --
   -----------

   function Image (S : Standard_Property_Set_Type) return String is
      N : constant String := To_Lower (S'Img);
   begin
      return N (N'First + 2 .. N'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : Ocarina_Property_Set_Type) return String is
      N : constant String := To_Lower (O'Img);
   begin
      case O is
         when O_Cheddar_Properties =>
            return "Cheddar_Properties";
         when others =>
            return N (N'First + 2 .. N'Last);
      end case;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      N : Name_Id;

   begin
      for S in Standard_Property_Sets'Range loop
         N := Internal_Name (Standard_Property_Sets (S)'Img);
         Set_Name_Table_Byte (N, Standard_Property_Set_Mask);
      end loop;

      for S in Ocarina_Property_Sets'Range loop
         N := Internal_Name (Ocarina_Property_Sets (S)'Img);
         Set_Name_Table_Byte (N, Ocarina_Property_Set_Mask);
      end loop;
   end Initialize;

   -------------------
   -- Internal_Name --
   -------------------

   function Internal_Name (N : Name_Id) return Name_Id is
   begin
      return Internal_Name (Get_Name_String (N));
   end Internal_Name;

   -------------------
   -- Internal_Name --
   -------------------

   function Internal_Name (S : String) return Name_Id is
      Tag : constant String := "%property_set%";

   begin
      if S'Length > 2 and then S (S'First + 1) = '_' then
         Set_Str_To_Name_Buffer (S (S'First + 2 .. S'Last));
      else
         Set_Str_To_Name_Buffer (S);
      end if;
      Add_Str_To_Name_Buffer (Tag);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end Internal_Name;

   -----------------
   -- Is_Standard --
   -----------------

   function Is_Standard (Identifier : Node_Id) return Boolean is
      pragma Assert (Kind (Identifier) = K_Identifier);
      N : constant Name_Id := Internal_Name (Name (Identifier));

   begin
      return (Get_Name_Table_Byte (N) and Standard_Property_Set_Mask) /= 0;
   end Is_Standard;

   ---------------------
   -- Is_User_Defined --
   ---------------------

   function Is_User_Defined (Identifier : Node_Id) return Boolean is
      pragma Assert (Kind (Identifier) = K_Identifier);
      N : constant Name_Id := Internal_Name (Name (Identifier));
      B : constant Byte    := Get_Name_Table_Byte (N);

   begin
      if B = 0 then
         return False;
      end if;
      return (Get_Name_Table_Byte (N) and User_Defined_Mask) /= 0;
   end Is_User_Defined;

   -------------------------
   -- Property_Set_Entity --
   -------------------------

   function Property_Set_Entity (Identifier : Node_Id) return Node_Id is
      pragma Assert (Kind (Identifier) = K_Identifier);
      N : constant Name_Id := Internal_Name (Name (Identifier));
      B : constant Byte    := Get_Name_Table_Byte (N);

   begin
      if B = 0 then
         return No_Node;
      end if;
      return Node_Id (Get_Name_Table_Info (N));
   end Property_Set_Entity;

   -----------------------------
   -- Set_Property_Set_Entity --
   -----------------------------

   procedure Set_Property_Set_Entity (Identifier : Node_Id) is
      pragma Assert (Kind (Identifier) = K_Identifier);
      N : constant Name_Id := Internal_Name (Name (Identifier));
      B : constant Byte    := Get_Name_Table_Byte (N);

   begin
      if B = 0 then
         return;
      end if;
      Set_Name_Table_Info (N, Int (Identifier));
   end Set_Property_Set_Entity;

   ----------------------------
   -- Standard_Property_Sets --
   ----------------------------

   function Standard_Property_Sets return Standard_Property_Set_Array_Type is
   begin
      return Standard_Property_Sets_Table (AADL_Version).all;
   end Standard_Property_Sets;

   ---------------------------
   -- Ocarina_Property_Sets --
   ---------------------------

   function Ocarina_Property_Sets return Ocarina_Property_Set_Array_Type is
   begin
      return Ocarina_Property_Sets_Table (AADL_Version).all;
   end Ocarina_Property_Sets;

   -------------------------
   -- Set_As_User_Defined --
   -------------------------

   procedure Set_As_User_Defined (Identifier : Node_Id) is
      pragma Assert (Kind (Identifier) = K_Identifier);
      N : constant Name_Id := Internal_Name (Name (Identifier));
      B : constant Byte    := Get_Name_Table_Byte (N);

   begin
      Set_Name_Table_Byte (N, B or User_Defined_Mask);
   end Set_As_User_Defined;

end Ocarina.Property_Sets;
