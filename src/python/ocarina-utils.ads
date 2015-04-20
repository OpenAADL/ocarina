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

pragma Warnings (Off);
with Ocarina.Types;                      use Ocarina.Types;
with GNATCOLL.Scripts;                   use GNATCOLL.Scripts;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package Ocarina.Utils is

   procedure Version;
   --  Display version information

   procedure Usage;
   --  Display a message describing the usage of Ocarina

   procedure Print_Status;
   --  Display status information on Ocarina

   procedure Load_AADL_File (Filename : String);
   function Analyze return Boolean;
   procedure Instantiate (Root_System : String);
   procedure Generate (Backend_Name : String);
   procedure Reset;

   function Get_AADL_Root return Node_Id;
   function Get_Node_Id_From_String (Name : String) return Node_Id;
   function Get_Name_Id_From_String (Name : String) return Name_Id;
   function Get_Boolean_From_String (Name : String) return Boolean;
   function Get_Byte_From_String (Name : String) return Byte;
   function Get_List_Id_From_String (Name : String) return List_Id;
   function Get_Int_From_String (Name : String) return Int;
   function Get_Value_Id_From_String (Name : String) return Value_Id;
   procedure Get_Node_Id (Data : in out Callback_Data'Class; N : String);

end Ocarina.Utils;
