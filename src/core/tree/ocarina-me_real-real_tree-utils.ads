------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . M E _ R E A L . R E A L _ T R E E . U T I L S       --
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

--  Defines types for REAL

with Ocarina.Types;
with Ocarina.ME_REAL.Tokens;
with Ocarina.ME_REAL.REAL_Tree.Debug;

package Ocarina.ME_REAL.REAL_Tree.Utils is

   use Ocarina.Types;
   use Ocarina.ME_REAL.Tokens;

   --  Operators values

   OV_Invalid       : constant Operator_Id := 0;
   OV_Equal         : constant Operator_Id := 1;
   OV_Not           : constant Operator_Id := 2;
   OV_And           : constant Operator_Id := 3;
   OV_Or            : constant Operator_Id := 4;
   OV_Greater       : constant Operator_Id := 5;
   OV_Less_Equal    : constant Operator_Id := 6;
   OV_Greater_Equal : constant Operator_Id := 7;
   OV_Less          : constant Operator_Id := 8;
   OV_Minus         : constant Operator_Id := 9;
   OV_Plus          : constant Operator_Id := 10;
   OV_Star          : constant Operator_Id := 11;
   OV_Modulo        : constant Operator_Id := 12;
   OV_Slash         : constant Operator_Id := 13;
   OV_Different     : constant Operator_Id := 14;
   OV_Power         : constant Operator_Id := 15;
   OV_If_Then_Else  : constant Operator_Id := 16;
   OV_Affect        : constant Operator_Id := 17;

   --  Predfined sets values

   SV_No_Type               : constant := 0;
   SV_Generic               : constant := 1;
   SV_Processor_Set         : constant := 18;
   SV_Virtual_Processor_Set : constant := 19;
   SV_Virtual_Bus_Set       : constant := 20;
   SV_Process_Set           : constant := 21;
   SV_Thread_Set            : constant := 22;
   SV_Threadgroup_Set       : constant := 23;
   SV_Subprogram_Call_Set   : constant := 24;
   SV_Sequence_Call_Set     : constant := 25;
   SV_Subprogram_Set        : constant := 26;
   SV_Data_Set              : constant := 27;
   SV_Memory_Set            : constant := 28;
   SV_Bus_Set               : constant := 29;
   SV_Connection_Set        : constant := 30;
   SV_Device_Set            : constant := 31;
   SV_System_Set            : constant := 32;
   SV_End_To_End_Flows_Set  : constant := 33;
   SV_Local_Set             : constant := 34;
   SV_Abstract_Set          : constant := 35;
   SV_Root_System_Set       : constant := 36;

   --  Selection functions codes

   FC_Is_Subcomponent_Of : constant := 1;
   FC_Is_Bound_To        : constant := 2;
   FC_Is_Connected_To    : constant := 3;
   FC_Is_Called_By       : constant := 5;
   FC_Is_Accessed_By     : constant := 6;
   FC_Is_Connecting_To   : constant := 7;
   FC_Is_Passing_Through : constant := 8;
   FC_Is_Calling         : constant := 9;
   FC_Is_Provided_Class  : constant := 10;
   FC_Is_Predecessor_Of  : constant := 11;
   FC_Is_Accessing_To    : constant := 12;

   FC_Get_Property_Value        : constant := 20;
   FC_Get_System_Property_Value : constant := 21;
   FC_Cardinal                  : constant := 22;
   FC_First                     : constant := 23;
   FC_Last                      : constant := 24;
   FC_Property_Exists           : constant := 25;
   FC_Expr                      : constant := 26;
   FC_Int                       : constant := 27;
   FC_Float                     : constant := 28;
   FC_List                      : constant := 29;
   FC_Max                       : constant := 30;
   FC_Min                       : constant := 31;
   FC_Is_In                     : constant := 32;
   FC_LCM                       : constant := 33;
   FC_GCD                       : constant := 34;
   FC_Non_Null                  : constant := 35;
   FC_Head                      : constant := 36;
   FC_Queue                     : constant := 37;
   FC_Size                      : constant := 38;
   FC_All_Equals                : constant := 39;
   FC_Product                   : constant := 40;
   FC_Sum                       : constant := 41;
   FC_Cos                       : constant := 42;
   FC_Sin                       : constant := 43;
   FC_Tan                       : constant := 44;
   FC_Cosh                      : constant := 45;
   FC_Sinh                      : constant := 46;
   FC_Tanh                      : constant := 47;
   FC_Ln                        : constant := 48;
   FC_Exp                       : constant := 49;
   FC_Sqrt                      : constant := 50;
   FC_Ceil                      : constant := 51;
   FC_Floor                     : constant := 52;

   FC_MMax        : constant := 60;
   FC_MMin        : constant := 61;
   FC_MAll_Equals : constant := 62;
   FC_MProduct    : constant := 63;
   FC_MSum        : constant := 64;

   FC_Unknown : constant := 65;

   --  return types

   RT_Boolean      : constant := 1;
   RT_Integer      : constant := 2;
   RT_Float        : constant := 3;
   RT_String       : constant := 4;
   RT_Float_List   : constant := 5;
   RT_Int_List     : constant := 6;
   RT_String_List  : constant := 7;
   RT_Bool_List    : constant := 8;
   RT_Range        : constant := 9;
   RT_Range_List   : constant := 10;
   RT_Element      : constant := 11;
   RT_Element_List : constant := 12;
   RT_Error        : constant := 13;
   RT_Unknown      : constant := 14;

   subtype Operator_Values is Operator_Id range OV_Equal .. OV_Star;

   subtype Predefined_Set_Values is
     Byte range SV_Processor_Set .. SV_Local_Set;

   subtype Return_Type is Value_Id range RT_Boolean .. RT_Unknown;

   function To_Lower (Str : Name_Id) return Name_Id;

   procedure Capitalize (S : in out String);

   function Quoted (S : String; D : Character := '"') return String;
   function Quoted (S : String; D : Character := '"') return Name_Id;
   function Quoted (N : Name_Id; D : Character := '"') return String;
   function Quoted (N : Name_Id; D : Character := '"') return Name_Id;
   --  Embrace string S or name N with character D

   function Image
     (Node : Node_Id) return String renames
     Ocarina.ME_REAL.REAL_Tree.Debug.Image;

   function Remove_Prefix (N : Name_Id) return Name_Id;
   --  returns the token after the last "::" separator

   function Translate_Predefined_Sets (T : Token_Type) return Value_Id;

   function Translate_Predefined_Sets (T : Value_Id) return Token_Type;

   function Translate_Function_Code (T : Token_Type) return Value_Id;

   function Get_Returned_Type (T : Value_Id) return Return_Type;

end Ocarina.ME_REAL.REAL_Tree.Utils;
