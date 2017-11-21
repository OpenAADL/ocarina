------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L . C O M P O N E N T S       --
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

with Ocarina.Types;
with Locations;

with Ocarina.ME_AADL;

package Ocarina.Builder.AADL.Components is

   --  for component types and implementations

   use Ocarina.Types;
   use Locations;

   function Add_Annex (Component : Node_Id; Annex : Node_Id) return Boolean;
   --  Add an annex subclause into a component (type or
   --  implementation). Component is a Node_Id referencing the
   --  component. Annex is a Node_Id referencing the annex
   --  subclause. Returns True if the annex was correctly added into
   --  the component, else False.

   function Add_Connection
     (Component  : Node_Id;
      Connection : Node_Id) return Boolean;
   --  Add a connection into a component implementation. Component is
   --  a Node_Id referencing the component implementation. Connection
   --  is a Node_Id referencing the connection. Returns True if the
   --  connection was correctly added into the component, else False.

   function Add_Feature
     (Component : Node_Id;
      Feature   : Node_Id) return Boolean;
   --  Add a feature into a component type. Component is a Node_Id
   --  referencing the component type. Feature is a Node_Id
   --  referencing the feature. Returns True if the feature was
   --  correctly added into the component, else False.

   function Add_Refined_Type
     (Component    : Node_Id;
      Refined_Type : Node_Id) return Boolean;
   --  Add a refined type into a component implementation. Refined
   --  types correspond to refinements of the component type features.

   function Add_Subcomponent
     (Component    : Node_Id;
      Subcomponent : Node_Id) return Boolean;
   --  Add a subcomponent into a component implementation. Component
   --  is a Node_Id referencing the component
   --  implementation. Subcomponent is a Node_Id referencing the
   --  subcomponent. Returns True if the subcomponent was correctly
   --  added into the component, else False.

   function Add_Prototype
     (Component : Node_Id;
      Prototype : Node_Id) return Boolean;
   --  Add a prototype into a component implementation or a component type.
   --  Component is a Node_Id referencing the component implementation or the
   --  component type. Prototype is a Node_Id referencing the prototype.
   --  Returns True if the prototype was correctly added into the component,
   --  else False.

   function Add_Subprogram_Call_Sequence
     (Component     : Node_Id;
      Call_Sequence : Node_Id) return Boolean;
   --  Add a subprogram call sequence into a component
   --  implementation. Component is a Node_Id referencing the
   --  component implementation. Call_Sequence is a Node_Id
   --  referencing the subcomponent. Returns True if the sequence was
   --  correctly added into the component, else False.

   function Add_Flow_Spec
     (Component : Node_Id;
      Flow_Spec : Node_Id) return Boolean;
   --  Add a flow specification into a component type. Component is a
   --  Node_Id referencing the component type. Flow_Spec is a Node_Id
   --  referencing the flow. Returns True if the flow was correctly
   --  added into the component, else False.

   function Add_Flow_Implementation
     (Component : Node_Id;
      Flow_Impl : Node_Id) return Boolean;
   --  Add a flow implementation into a component
   --  implementation. Component is a Node_Id referencing the
   --  component implementation. Flow_Impl is a Node_Id referencing
   --  the flow. Returns True if the flow was correctly added into the
   --  component, else False.

   function Add_End_To_End_Flow_Spec
     (Component       : Node_Id;
      End_To_End_Flow : Node_Id) return Boolean;
   --  Add an end to end flow specification into a component
   --  implementation. Component is a Node_Id referencing the
   --  component implementation. Flow_Impl is a Node_Id referencing
   --  the flow. Returns True if the flow was correctly added into the
   --  component, else False.

   function Add_Mode (Component : Node_Id; Mode : Node_Id) return Boolean;
   --  Add a mode (declaration or transition) into a component
   --  implementation. Component is a Node_Id referencing the
   --  component implementation. Mode is a Node_Id referencing the
   --  mode declaration or mode transition. Returns True if the mode
   --  was correctly added into the component, else False.

   function Add_Property_Association
     (Component            : Node_Id;
      Property_Association : Node_Id) return Boolean;
   --  Add a property association into a component (type or
   --  implementation). Component is a Node_Id referencing the
   --  component type or implementation. Property_Association is a
   --  Node_Id referencing the property association. Returns True if
   --  the property was correctly added into the component, else
   --  False.

   --  Component creation

   function Add_New_Component_Type
     (Loc            : Location;
      Identifier     : Node_Id;
      Namespace      : Node_Id;
      Component_Type : Ocarina.ME_AADL.Component_Category;
      Is_Private     : Boolean := False) return Node_Id;
   --  Create a new component type node. A component type can be
   --  inserted into a package or the top level AADL specification
   --  (aka the unnamed namespace). Loc is the location of the
   --  component in the parsed text. Identifier is a Node_Id
   --  referencing the name of the component. Namespace is either a
   --  package specification or the top level AADL
   --  specification. Component_Type is the component category
   --  (processor, memory, process, etc.). Is_Private indicates if the
   --  component is declaraed in the private or the public part of the
   --  package; it is only relevant if Namespace references a package
   --  specification. Returns the Node_Id of the newly created
   --  component type node, or No_Node if something went wrong.

   function Add_New_Component_Implementation
     (Loc            : Location;
      Identifier     : Node_Id;
      Namespace      : Node_Id;
      Component_Type : Ocarina.ME_AADL.Component_Category;
      Is_Private     : Boolean := False) return Node_Id;
   --  Create a new component implementation node. A component
   --  implementation can be inserted into a package or the top level
   --  AADL specification (aka the unnamed namespace). Loc is the
   --  location of the component in the parsed text. Identifier is a
   --  Node_Id referencing the name of the component. Namespace is
   --  either a package specification or the top level AADL
   --  specification. Component_Type is the component category
   --  (processor, memory, process, etc.). Is_Private indicates if the
   --  component is declaraed in the private or the public part of the
   --  package; it is only relevant if Namespace references a package
   --  specification. Returns the Node_Id of the newly created
   --  component implementation node, or No_Node if something went
   --  wrong.

   function Add_New_Feature_Group
     (Loc        : Location;
      Name       : Node_Id;
      Namespace  : Node_Id;
      Is_Private : Boolean := False) return Node_Id;
   --  Create a new feature group type (AADL_V2) or port group type (AADL_V1).
   --  It can be inserted into a package or the top level AADL specification.)

end Ocarina.Builder.AADL.Components;
