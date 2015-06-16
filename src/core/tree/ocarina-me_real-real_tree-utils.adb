------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . M E _ R E A L . R E A L _ T R E E . U T I L S       --
--                                                                          --
--                                 B o d y                                  --
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

with Charset; use Charset;
with Errors;  use Errors;
with Ocarina.Namet;   use Ocarina.Namet;

package body Ocarina.ME_REAL.REAL_Tree.Utils is

   Up_To_Low : constant := Character'Pos ('A') - Character'Pos ('a');

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Str : Name_Id) return Name_Id is
      N : constant String := To_Lower (Get_Name_String (Str));
   begin
      return Get_String_Name (N);
   end To_Lower;

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Up : Boolean := True;
   begin
      for I in S'Range loop
         if Up then
            Up := False;
            if S (I) in 'a' .. 'z' then
               S (I) := Character'Val (Character'Pos (S (I)) + Up_To_Low);
            end if;
         else
            if S (I) in 'A' .. 'Z' then
               S (I) := Character'Val (Character'Pos (S (I)) - Up_To_Low);
            end if;
         end if;
         if S (I) = '_' then
            Up := True;
         end if;
      end loop;
   end Capitalize;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return String is
   begin
      return (1 => D) & S & (1 => D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (S : String; D : Character := '"') return Name_Id is
   begin
      Set_Char_To_Name_Buffer (D);
      Add_Str_To_Name_Buffer (S);
      Add_Char_To_Name_Buffer (D);
      return Name_Find;
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return String is
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   ------------
   -- Quoted --
   ------------

   function Quoted (N : Name_Id; D : Character := '"') return Name_Id is
   begin
      return Quoted (Get_Name_String (N), D);
   end Quoted;

   function Remove_Prefix (N : Name_Id) return Name_Id is
      M : constant Natural := Name_Len;
      S : constant String  := Get_Name_String (N);
      P : Boolean          := False;
      L : Natural          := 0;
   begin
      --  Search the last occurence of "::"

      for I in 1 .. M loop
         if S (I) = ':' then
            if P then
               L := I;
               P := False;
            else
               P := True;
            end if;
         else
            P := False;
         end if;
      end loop;

      Set_Str_To_Name_Buffer (S (L + 1 .. M));

      return To_Lower (Name_Find);
   end Remove_Prefix;

   -------------------------------
   -- Translate_Predefined_Sets --
   -------------------------------

   function Translate_Predefined_Sets (T : Token_Type) return Value_Id is
   begin
      case T is
         when T_Processor_Set =>
            return SV_Processor_Set;
         when T_Virtual_Processor_Set =>
            return SV_Virtual_Processor_Set;
         when T_Process_Set =>
            return SV_Process_Set;
         when T_Thread_Set =>
            return SV_Thread_Set;
         when T_Threadgroup_Set =>
            return SV_Threadgroup_Set;
         when T_Subprogram_Call_Set =>
            return SV_Subprogram_Call_Set;
         when T_Sequence_Call_Set =>
            return SV_Sequence_Call_Set;
         when T_Subprogram_Set =>
            return SV_Subprogram_Set;
         when T_Data_Set =>
            return SV_Data_Set;
         when T_Memory_Set =>
            return SV_Memory_Set;
         when T_Bus_Set =>
            return SV_Bus_Set;
         when T_Virtual_Bus_Set =>
            return SV_Virtual_Bus_Set;
         when T_Connection_Set =>
            return SV_Connection_Set;
         when T_Device_Set =>
            return SV_Device_Set;
         when T_System_Set =>
            return SV_System_Set;
         when T_Abstract_Set =>
            return SV_Abstract_Set;
         when T_Root_System_Set =>
            return SV_Root_System_Set;
         when T_End_To_End_Flows_Set =>
            return SV_End_To_End_Flows_Set;
         when T_Local_Set =>
            return SV_Local_Set;
         when T_Unknown_Set =>
            return SV_No_Type;
         when others =>
            DE ("Unknown REAL predefined set");
            return SV_No_Type;
      end case;
   end Translate_Predefined_Sets;

   -------------------------------
   -- Translate_Predefined_Sets --
   -------------------------------

   function Translate_Predefined_Sets (T : Value_Id) return Token_Type is
   begin
      case T is
         when SV_Processor_Set =>
            return T_Processor_Set;
         when SV_Virtual_Processor_Set =>
            return T_Virtual_Processor_Set;
         when SV_Process_Set =>
            return T_Process_Set;
         when SV_Thread_Set =>
            return T_Thread_Set;
         when SV_Threadgroup_Set =>
            return T_Threadgroup_Set;
         when SV_Subprogram_Call_Set =>
            return T_Subprogram_Call_Set;
         when SV_Sequence_Call_Set =>
            return T_Sequence_Call_Set;
         when SV_Subprogram_Set =>
            return T_Subprogram_Set;
         when SV_Data_Set =>
            return T_Data_Set;
         when SV_Memory_Set =>
            return T_Memory_Set;
         when SV_Bus_Set =>
            return T_Bus_Set;
         when SV_Virtual_Bus_Set =>
            return T_Virtual_Bus_Set;
         when SV_Connection_Set =>
            return T_Connection_Set;
         when SV_Device_Set =>
            return T_Device_Set;
         when SV_System_Set =>
            return T_System_Set;
         when SV_Abstract_Set =>
            return T_Abstract_Set;
         when SV_Root_System_Set =>
            return T_Root_System_Set;
         when SV_End_To_End_Flows_Set =>
            return T_End_To_End_Flows_Set;
         when SV_Local_Set =>
            return T_Local_Set;
         when SV_No_Type =>
            return T_Unknown_Set;
         when others =>
            DE ("Unknown REAL predefined set");
            return T_Unknown_Set;
      end case;
   end Translate_Predefined_Sets;

   -----------------------------
   -- Translate_Function_Code --
   -----------------------------

   function Translate_Function_Code (T : Token_Type) return Value_Id is
   begin
      case T is
         when T_Is_Subcomponent_Of =>
            return FC_Is_Subcomponent_Of;
         when T_Is_Bound_To =>
            return FC_Is_Bound_To;
         when T_Is_Provided_Class =>
            return FC_Is_Provided_Class;
         when T_Is_Predecessor_Of =>
            return FC_Is_Predecessor_Of;
         when T_Is_Connected_To =>
            return FC_Is_Connected_To;
         when T_Is_Connecting_To =>
            return FC_Is_Connecting_To;
         when T_Get_System_Property_Value =>
            return FC_Get_System_Property_Value;
         when T_Is_Called_By =>
            return FC_Is_Called_By;
         when T_Is_Calling =>
            return FC_Is_Calling;
         when T_Is_Accessed_By =>
            return FC_Is_Accessed_By;
         when T_Is_Accessing_To =>
            return FC_Is_Accessing_To;
         when T_Get_Property_Value =>
            return FC_Get_Property_Value;
         when T_Property_Exists =>
            return FC_Property_Exists;
         when T_Cardinal =>
            return FC_Cardinal;
         when T_Is_Passing_Through =>
            return FC_Is_Passing_Through;
         when T_Is_In =>
            return FC_Is_In;
         when T_GCD =>
            return FC_GCD;
         when T_LCM =>
            return FC_LCM;
         when T_Non_Null =>
            return FC_Non_Null;
         when T_Size =>
            return FC_Size;
         when T_Max =>
            return FC_Max;
         when T_Min =>
            return FC_Min;
         when T_All_Equals =>
            return FC_All_Equals;
         when T_List =>
            return FC_List;
         when T_Integer =>
            return FC_Int;
         when T_Ceil =>
            return FC_Ceil;
         when T_Floor =>
            return FC_Floor;
         when T_Float =>
            return FC_Float;
         when T_Sum =>
            return FC_Sum;
         when T_Product =>
            return FC_Product;
         when T_Expr =>
            return FC_Expr;
         when T_First =>
            return FC_First;
         when T_Last =>
            return FC_Last;
         when T_MMax =>
            return FC_MMax;
         when T_MMin =>
            return FC_MMin;
         when T_MAll_Equals =>
            return FC_MAll_Equals;
         when T_MProduct =>
            return FC_MProduct;
         when T_MSum =>
            return FC_MSum;
         when T_Head =>
            return FC_Head;
         when T_Queue =>
            return FC_Queue;
         when T_Cos =>
            return FC_Cos;
         when T_Sin =>
            return FC_Sin;
         when T_Tan =>
            return FC_Tan;
         when T_Cosh =>
            return FC_Cosh;
         when T_Sinh =>
            return FC_Sinh;
         when T_Tanh =>
            return FC_Tanh;
         when T_Ln =>
            return FC_Ln;
         when T_Exp =>
            return FC_Exp;
         when T_Sqrt =>
            return FC_Sqrt;
         when others =>
            DE ("Unknown REAL function");
            return FC_Unknown;
      end case;

   end Translate_Function_Code;

   -----------------------
   -- Get_Returned_Type --
   -----------------------

   function Get_Returned_Type (T : Value_Id) return Return_Type is
   begin
      case T is
         when FC_Is_Subcomponent_Of =>
            return RT_Boolean;
         when FC_Is_Bound_To =>
            return RT_Boolean;
         when FC_Is_Connected_To =>
            return RT_Boolean;
         when FC_Is_Connecting_To =>
            return RT_Boolean;
         when FC_Get_System_Property_Value =>
            return RT_Unknown;
         when FC_Is_Called_By =>
            return RT_Boolean;
         when FC_Is_Provided_Class =>
            return RT_Boolean;
         when FC_Is_Predecessor_Of =>
            return RT_Boolean;
         when FC_Is_Calling =>
            return RT_Boolean;
         when FC_Is_Accessed_By =>
            return RT_Boolean;
         when FC_Is_Accessing_To =>
            return RT_Boolean;
         when FC_Is_Passing_Through =>
            return RT_Boolean;
         when FC_Get_Property_Value =>
            return RT_Unknown;
         when FC_Property_Exists =>
            return RT_Boolean;
         when FC_Cardinal =>
            return RT_Integer;
         when FC_Size =>
            return RT_Integer;
         when FC_All_Equals =>
            return RT_Boolean;
         when FC_Min =>      --  Result is casted to float
            return RT_Float;
         when FC_Max =>      --  Result is casted to float
            return RT_Float;
         when FC_Product =>  --  Result is casted to float
            return RT_Float;
         when FC_Expr =>     --  Result is casted to float
            return RT_Float_List;
         when FC_Sum =>      --  Result is casted to float
            return RT_Float;
         when FC_Is_In =>
            return RT_Boolean;
         when FC_LCM =>
            return RT_Integer;
         when FC_GCD =>
            return RT_Integer;
         when FC_Non_Null =>
            return RT_Integer;
         when FC_First | FC_Last =>  --  Result is casted to float
            return RT_Float;
         when FC_List =>
            return RT_Unknown;
         when FC_Head =>
            return RT_Unknown;
         when FC_Queue =>
            return RT_Unknown;
         when FC_Cos | FC_Sin | FC_Tan | FC_Ceil | FC_Floor =>
            return RT_Float;
         when others =>
            DE ("Unable to determine REAL function returning type");
            return RT_Error;
      end case;
   end Get_Returned_Type;

end Ocarina.ME_REAL.REAL_Tree.Utils;
