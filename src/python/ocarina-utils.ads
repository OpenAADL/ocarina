------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                        O C A R I N A . U T I L S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2013-2015 ESA & ISAE.                    --
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

with Ocarina.Types;                      use Ocarina.Types;

package Ocarina.Utils is

   procedure Print_Status;
   --  Display status information on Ocarina

   procedure Load_AADL_File (Filename : String);
   function Analyze return Boolean;
   function Instantiate (Root_System : String) return Boolean;
   procedure Generate (Backend_Name : String);
   procedure Reset;
   function Set_REAL_Theorem (Theorem_Name : String) return Boolean;
   function Add_REAL_Library (Library_Name : String) return Boolean;

   function Get_AADL_Root return Node_Id;
   function Get_Node_Id_From_String (Name : String) return Node_Id;
   function Get_Name_Id_From_String (Name : String) return Name_Id;
   function Get_Boolean_From_String (Name : String) return Boolean;
   function Get_Byte_From_String (Name : String) return Byte;
   function Get_List_Id_From_String (Name : String) return List_Id;
   function Get_Int_From_String (Name : String) return Int;
   function Get_Value_Id_From_String (Name : String) return Value_Id;

end Ocarina.Utils;
