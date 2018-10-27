------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B E _ A A D L . P R O P E R T I E S . V A L U E S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

package Ocarina.BE_AADL.Properties.Values is

   procedure Print_Unique_Property_Constant_Identifier (Node : Node_Id);

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
