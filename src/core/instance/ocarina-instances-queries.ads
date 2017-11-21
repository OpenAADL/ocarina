------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . I N S T A N C E S . Q U E R I E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

--  This package does the same work of Ocarina.Analyzer.Queries, but
--  on the instance tree.

package Ocarina.Instances.Queries is

   function Compute_Absolute_Name_Of_Entity
     (Entity    : Node_Id;
      Separator : Name_Id := No_Name) return Name_Id;
   --  Concatenate the names of all the parent entities of Entity,
   --  including its name, separated by dots.

   --  Functions that ask for the presence of the property

   function Is_Defined_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Property

   function Is_Defined_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Property

   function Is_Defined_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_String_Property

   function Is_Defined_Integer_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Integer_Property

   function Is_Defined_Boolean_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Boolean_Property

   function Is_Defined_Float_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Float_Property

   function Is_Defined_Reference_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Reference_Property

   function Is_Defined_Classifier_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Classifier_Property

   function Is_Defined_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Range_Property

   function Is_Defined_Record_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;

   function Is_Defined_List_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_List_Property

   function Is_Defined_List_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_List_Property

   function Is_Defined_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Is_Defined_Enumeration_Property

   --  Functions that return the property values

   function Get_Property_Association
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Property_Association

   function Get_Value_Of_Property_Association
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Value_Of_Property_Association

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return String;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_String_Property

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_String_Property

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_String_Property

   function Get_Integer_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Unsigned_Long_Long;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Integer_Property

   function Get_Integer_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Unsigned_Long_Long;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Integer_Property

   function Get_Float_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Long_Long_Float;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Float_Property

   function Get_Boolean_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Boolean_Property

   function Get_Reference_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Reference_Property

   function Get_Classifier_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Classifier_Property

   function Get_Classifier_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Classifier_Property

   function Get_List_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return List_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_List_Property

   function Get_List_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return List_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_List_Property

   function Get_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Range_Property

   function Get_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return String;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Enumeration_Property

   function Get_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Get_Enumeration_Property

   function Compute_Property_Value (Property_Value : Node_Id) return Node_Id;
   --  Cf. the documentation of
   --  Ocarina.Analyzer.Queries.Compute_Property_Value

end Ocarina.Instances.Queries;
