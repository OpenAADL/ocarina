------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . P R O P E R T Y _ S E T S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2008-2009, GET-Telecom Paris.                --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Ocarina.Property_Sets is

   type Standard_Property_Set_Type is
     (S_Error,                       --  Invalid enumerator
      S_AADL_Project,
      S_AADL_Properties,
      S_Deployment_Properties,
      S_Thread_Properties,
      S_Communication_Properties,
      S_Memory_Properties,
      S_Timing_Properties,
      S_Programming_Properties);
   --  The standard property sets

   type Ocarina_Property_Set_Type is
     (O_Error,                       --  Invalid enumerator
      O_Base_Types,
      O_Data_Model,
      O_Deployment,
      O_Cheddar_Properties,
      O_Ocarina_Config,
      O_Transformations,
      O_POK_Properties,
      O_ARINC653_Properties,
      O_Taste_Properties,
      O_ASSERT_Properties);
   --  The Ocarina property sets

   type Standard_Property_Set_Array_Type is
     array (Integer range <>) of Standard_Property_Set_Type;

   type Standard_Property_Set_Array_Access is
     access all Standard_Property_Set_Array_Type;

   type Ocarina_Property_Set_Array_Type is
     array (Integer range <>) of Ocarina_Property_Set_Type;

   type Ocarina_Property_Set_Array_Access is
     access all Ocarina_Property_Set_Array_Type;

   procedure Initialize;

   function Image (S : Standard_Property_Set_Type) return String;
   --  Return the base name of the AADL file containing the standard
   --  property set corresponding to the given enumerator.

   function Image (O : Ocarina_Property_Set_Type) return String;
   --  Return the base name of the AADL file containing the Ocarina
   --  property set corresponding to the given enumerator.

   function Is_Standard (Identifier : Node_Id) return Boolean;

   function Property_Set_Entity (Identifier : Node_Id) return Node_Id;
   procedure Set_Property_Set_Entity (Identifier : Node_Id);

   function Is_User_Defined (Identifier : Node_Id) return Boolean;
   procedure Set_As_User_Defined (Identifier : Node_Id);

   function Standard_Property_Sets
     return Standard_Property_Set_Array_Type;

   function Ocarina_Property_Sets
     return Ocarina_Property_Set_Array_Type;

end Ocarina.Property_Sets;
