------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B E _ A A D L . P R O P E R T I E S . V A L U E S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2008, GET-Telecom Paris.                   --
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

package Ocarina.BE_AADL.Properties.Values is

   procedure Print_Or_Boolean_Term (Node : Node_Id);
   procedure Print_Computed_Term (Node : Node_Id);

   procedure Print_Classifier_Type (L : List_Id);
   procedure Print_Enumeration_Type (Node : Node_Id);
   procedure Print_Literal (Node : Node_Id);

   procedure Print_Numeric_Term (Node : Node_Id);
   procedure Print_Number_Type (Node : Node_Id);
   procedure Print_Property_Value (Node : Node_Id);
   procedure Print_Property_Type_Designator (Node : Node_Id);

   procedure Print_Range (Node : Node_Id);
   procedure Print_Range_Type (Node : Node_Id);

   procedure Print_Record_Type (Node : Node_Id);
   procedure Print_Record_Term (Node : Node_Id);
   procedure Print_Reference_Type (L : List_Id);

   procedure Print_Units_Type (Node : Node_Id);

   procedure Print_Named_Element (Node : Node_Id);

   procedure Print_Signed_AADLNumber (Node : Node_Id);

   procedure Print_Contained_Element_Path (Node : Node_Id);

end Ocarina.BE_AADL.Properties.Values;
