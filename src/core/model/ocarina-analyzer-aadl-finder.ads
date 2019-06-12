------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . A N A L Y Z E R . A A D L . F I N D E R          --
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

--  This package provides functions to search nodes in the abstract
--  tree. The functions return No_Node if nothing was found.

with Ocarina.ME_AADL.AADL_Tree.Nodes;

package Ocarina.Analyzer.AADL.Finder is

   type Node_Kind_Array is
     array (Positive range <>) of Ocarina.ME_AADL.AADL_Tree.Nodes.Node_Kind;

   procedure Select_Nodes
     (Decl_List  :        List_Id;
      Kinds      :        Node_Kind_Array;
      First_Node : in out Node_Id;
      Last_Node  : in out Node_Id);
   --  Build a list (chained using the accessor Next_Entity) from
   --  Decl_List and appends it to Last_Node. This list will contain
   --  the nodes whose kinds correspond to Kinds.

   function Find_Property_Entity
     (Root                    : Node_Id;
      Property_Set_Identifier : Node_Id;
      Property_Identifier     : Node_Id) return Node_Id;
   --  Find a property entity (type, name or constant). If
   --  Property_Set_Identifier is No_Node and the current scope is the
   --  one of a property set, try to find the property in it. Finally,
   --  look for the implicit property sets.

   function Find_Component_Classifier
     (Root                 : Node_Id;
      Package_Identifier   : Node_Id;
      Component_Identifier : Node_Id) return Node_Id;
   --  Same as above, but find a component classifier

   function Find_Component_Classifier_By_Name
     (Root                 : Node_Id;
      Package_Identifier   : Node_Id;
      Component_Name       : Name_Id) return Node_Id;

   function Find_Port_Group_Classifier
     (Root                  : Node_Id;
      Package_Identifier    : Node_Id;
      Port_Group_Identifier : Node_Id) return Node_Id;
   --  Same as above, but find a port group

   function Find_Feature
     (Component          : Node_Id;
      Feature_Identifier : Node_Id) return Node_Id;
   --  Find a feature in a component type or implementation

   function Find_Mode
     (Component       : Node_Id;
      Mode_Identifier : Node_Id) return Node_Id;
   --  Same as above, but find a mode

   function Find_Prototype
     (Component            : Node_Id;
      Prototype_Identifier : Node_Id) return Node_Id;
   --  XXX

   function Find_Subcomponent
     (Component               : Node_Id;
      Subcomponent_Identifier : Node_Id;
      In_Modes                : Node_Id := No_Node) return Node_Id;
   --  Find a subcomponent in a component implementation. If In_Modes
   --  is specified, return the subcomponent that are set in the
   --  given modes.

   function Find_Subprogram_Call
     (Component       : Node_Id;
      Call_Identifier : Node_Id;
      In_Modes        : Node_Id := No_Node) return Node_Id;
   --  Same as above but find a subprogram call

   function Find_Connection
     (Component             : Node_Id;
      Connection_Identifier : Node_Id;
      In_Modes              : Node_Id := No_Node) return Node_Id;
   --  Same as above but find a connection

   function Find_Flow_Spec
     (Component       : Node_Id;
      Flow_Identifier : Node_Id) return Node_Id;
   --  Find a flow in a component type or implementation

   function Find_Subclause
     (Component  : Node_Id;
      Identifier : Node_Id) return Node_Id;
   --  Same as above but find a subclause

   function Find_All_Declarations
     (Root      : Node_Id;
      Kinds     : Node_Kind_Array;
      Namespace : Node_Id := No_Node) return Node_List;
   --  Returns the first node of a list of declarations corresponding
   --  to the Kinds requested. Following nodes are accessed through
   --  the Next_Entity accessor. If no Kinds are requested, then
   --  return all the declarations found. If the Namespace is not
   --  given, search the declaration in the whole AADL specification
   --  declarations and its namespaces. Otherwise, search the
   --  declaration in the given namespace.

   function Find_All_Component_Types
     (Root      : Node_Id;
      Namespace : Node_Id := No_Node) return Node_List;
   --  Return the first component type found in the Namespace. If
   --  Namespace is No_Node, then return the first component type
   --  declaration in the whole AADL specification. Following
   --  declarations are accessed using the Next_Entity accessor.

   function Find_All_Root_Systems (Root : Node_Id) return Node_List;
   --  Return all systems implementations whose component type do not
   --  have any feature. Those systems correspond to the roots of the
   --  instantiated architecture.

   function Find_All_Subclauses
     (AADL_Declaration : Node_Id;
      Kinds            : Node_Kind_Array) return Node_List;
   --  General function that returns the first node of a list of
   --  subclauses corresponding to the Kinds requested. Following
   --  nodes are accessed through the Next_Entity accessor.

   function Find_All_Features (AADL_Declaration : Node_Id) return Node_List;
   --  Applicable to component types and implementations, and port
   --  group types.

   function Find_All_Subclause_Declarations_Except_Properties
     (AADL_Declaration : Node_Id) return Node_List;
   --  Applicable to component types and implementations, and port
   --  group types.

   function Find_All_Property_Associations
     (AADL_Declaration : Node_Id) return Node_List;
   --  Applicable to component types and implementations, and port
   --  group types.

   function Find_Property_Association
     (AADL_Declaration          : Node_Id;
      Property_Association_Name : Name_Id) return Node_Id;
   --  Find the property association named
   --  Property_Association_Name. Return No_Node if nothing was found.

   function Find_Property_Enumeration
     (Root               : Node_Id;
      Container          : Node_Id;
      Property_Container : Node_Id;
      Default_Value      : Node_Id;
      Designator         : Node_Id) return Node_Id;

   function Find_In_Import_Declaration
     (Package_Container : Node_Id;
      Node              : Node_Id) return Boolean;
   --  The kind of Package_Container is K_Package_Specification,
   --  the kind of Node is K_Identifier or K_Entity_Reference,
   --  return True if the Node is 'with' in Package_Container
   --  'with' declarations.

end Ocarina.Analyzer.AADL.Finder;
