------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . A N A L Y Z E R . A A D L . Q U E R I E S         --
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

--  This package contains routines that are used to get several
--  information from the AADL tree.

package Ocarina.Analyzer.AADL.Queries is

   function Is_An_Extension
     (Component : Node_Id;
      Ancestor  : Node_Id) return Boolean;
   --  Returns True if Component is an extension of Ancestor, whether
   --  by the keyword 'extends' or because Ancestor is a corresponding
   --  component type.

   function Needed_By (N : Node_Id; Entity : Node_Id) return Boolean;
   --  Return True iff N is needed by Entity (for example Entity has a
   --  subcompnent of type N). It also return True if N is needed
   --  indirectly by Entity (through another intermediate need). In
   --  order for this function to work fine, the AADL tree must have
   --  been expanded. However, since it acts only on the AADL syntax
   --  tree, this function is put in this package.

   --  NOTE: If N is a property *declaration* node, the result will be
   --  True reguardless the actual need of Entity to N.

   function Property_Can_Apply_To_Entity
     (Property : Node_Id;
      Entity   : Node_Id) return Boolean;
   --  Return True if the property association Property can be applied
   --  to Entity. Otherwise, return False. Beware that this function
   --  performs exact verification; a property cannot apply to a
   --  package.

   function Is_Defined_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the property named 'Name' is defined for the
   --  AADL entity 'Entity'. If 'In_Mode' is a valid name, consider
   --  only the property defined for the given mode.

   function Is_Defined_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the aadlstring property named 'Name' is defined
   --  for the AADL entity 'Entity'. If 'In_Mode' is a valid name,
   --  consider only the property defined for the given mode.

   function Is_Defined_Integer_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the aadlinteger property named 'Name' is defined
   --  for the AADL entity 'Entity'. If 'In_Mode' is a valid name,
   --  consider only the property defined for the given mode.

   function Is_Defined_Boolean_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the aadlboolean property named 'Name' is defined
   --  for the AADL entity 'Entity'. If 'In_Mode' is a valid name,
   --  consider only the property defined for the given mode.

   function Is_Defined_Float_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the aadlreal property named 'Name' is defined
   --  for the AADL entity 'Entity'. If 'In_Mode' is a valid name,
   --  consider only the property defined for the given mode.

   function Is_Defined_Reference_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the component reference property named 'Name' is
   --  defined for the AADL entity 'Entity'. If 'In_Mode' is a valid
   --  name, consider only the property defined for the given mode.

   function Is_Defined_Classifier_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the component classifier property named 'Name' is
   --  defined for the AADL entity 'Entity'. If 'In_Mode' is a valid
   --  name, consider only the property defined for the given mode.

   function Is_Defined_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the component range property named 'Name' is
   --  defined for the AADL entity 'Entity'. If 'In_Mode' is a valid
   --  name, consider only the property defined for the given mode.

   function Is_Defined_List_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the 'list of XXX' property named 'Name' is
   --  defined for the AADL entity 'Entity'. If 'In_Mode' is a valid
   --  name, consider only the property defined for the given mode.

   function Is_Defined_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return True if the enumeration property named 'Name' is defined
   --  for the AADL entity 'Entity'. If 'In_Mode' is a valid name,
   --  consider only the property defined for the given mode.

   function Get_Property_Association
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Return the property association node corresponding to Name. If
   --  the propoerty designed by name is not present for Entity,
   --  return No_Node. If 'In_Mode' is a valid name, consider only
   --  the property defined for the given mode.

   function Get_Value_Of_Property_Association
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Return the value of the property association named 'Name' if it
   --  is defined defined for 'Entity'.  Otherwise, return No_Node. If
   --  'In_Mode' is a valid name, consider only the property defined
   --  for the given mode.

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return String;
   --  Return the value of the aadlstring property association named
   --  'Name' if it is defined defined for 'Entity'.  Otherwise,
   --  return "". If 'In_Mode' is a valid name, consider only the
   --  property defined for the given mode.

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Return the value of the aadlstring property association named
   --  'Name' if it is defined defined for 'Entity'.  Otherwise,
   --  return No_Name. If 'In_Mode' is a valid name, consider only
   --  the property defined for the given mode.

   function Get_Integer_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Unsigned_Long_Long;
   --  Return the value of the aadlinteger property association named
   --  'Name' if it is defined defined for 'Entity'.  Otherwise,
   --  return 0. If 'In_Mode' is a valid name, consider only the
   --  property defined for the given mode.

   function Get_Float_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Long_Long_Float;
   --  Return the value of the aadlreal property association named
   --  'Name' if it is defined defined for 'Entity'.  Otherwise,
   --  return 0.0. If 'In_Mode' is a valid name, consider only the
   --  property defined for the given mode.

   function Get_Boolean_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean;
   --  Return the value of the aadlboolean property association named
   --  'Name' if it is defined defined for 'Entity'.  Otherwise,
   --  return False. If 'In_Mode' is a valid name, consider only the
   --  property defined for the given mode.

   function Get_Reference_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Return the value of the component reference property
   --  association named 'Name' if it is defined for 'Entity'.
   --  Otherwise, return No_Node. If 'In_Mode' is a valid name,
   --  consider only the property defined for the given mode.

   function Get_Classifier_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Return the value of the component classifier property
   --  association named 'Name' if it is defined for 'Entity'.
   --  Otherwise, return No_Node. If 'In_Mode' is a valid name,
   --  consider only the property defined for the given mode.

   function Get_List_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return List_Id;
   --  Return the value of the 'list of XXX' property association
   --  named 'Name' if it is defined for 'Entity'. The returned list
   --  is a Node_Container list. Otherwise, return No_List. If
   --  'In_Mode' is a valid name, consider only the property defined
   --  for the given mode.

   function Get_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Return the values of the range property association named
   --  'Name' if it is defined for 'Entity'.  Otherwise, return
   --  No_List. If 'In_Mode' is a valid name, consider only the
   --  property defined for the given mode.

   function Get_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return String;
   --  Return the value of the enumeration property association named
   --  'Name' if it is defined defined for 'Entity'. Otherwise, return
   --  "". If 'In_Mode' is a valid name, consider only the property
   --  defined for the given mode.

   function Get_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Return the value of the enumeration property association named
   --  'Name' if it is defined defined for 'Entity'. Otherwise, return
   --  No_Name. If 'In_Mode' is a valid name, consider only the
   --  property defined for the given mode.

   function Compute_Property_Value (Property_Value : Node_Id) return Node_Id;
   --  Compute the value of a property value and return a Node_Id
   --  containing this value. This value does not contain any
   --  reference (value ()).

end Ocarina.Analyzer.AADL.Queries;
