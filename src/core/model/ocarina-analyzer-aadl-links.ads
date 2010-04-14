------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . A N A L Y Z E R . A A D L . L I N K S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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

--  This package contains the routines that perform the links between
--  the different nodes in the AADL tree (subprogram calls/subprogram
--  components, subcomponents/components, properties/values...). All
--  the routines return True IFF the link process succeded.

package Ocarina.Analyzer.AADL.Links is

   function Link_Declarations_Of_Namespaces (Root : Node_Id) return Boolean;
   --  Perform all the several links of the declarations in the given
   --  namespace.

   function Link_Properties_Of_AADL_Description
     (Root    : Node_Id)
     return Boolean;
   --  Perform the link of the properties in the AADL package to the
   --  components they apply to.

   function Link_Subclauses_In_Components_And_Feature_Groups
     (Root    : Node_Id)
     return Boolean;
   --  Perform the link of subclauses (subcomponents, calls...)

   function Link_Component_Or_Feature_Group_Extension
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link of components or portgroup extension (AADL_V1)
   --  or feature group extension (AADL_V2)

   function Link_Inverse_Of_Feature_Group_Type
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link of the 'inverse of' of a port group (AADL_V1)
   --  or feature group (AADL_V2)

   function Link_Component_Implementation_To_Component_Type
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link of an implementation to its corresponding type

   function Link_Properties_Of_Component
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link of a property association to a component

   function Link_Property_Name
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link between the property name the actual property
   --  declaration.

   function Link_Property_Type
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link between the property type and the type
   --  declaration.

   function Link_Property_Constant
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link between a contant and the corresponding
   --  constant declaration.

   function Link_Property_Association
     (Root      : Node_Id;
      Container : Node_Id;
      Node      : Node_Id)
     return Boolean;
   --  Container is the component or package that contains the
   --  property association or the declarations that contains the
   --  property associations. Node is the property association. Root
   --  is the root of the tree.

   function Link_Properties
     (Root      : Node_Id;
      Container : Node_Id;
      List      : List_Id)
     return Boolean;
   --  Same as above but with a list of property associatins.

   function Link_Call
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the links between subprogram calls and subprogram
   --  components.

   function Link_Connection
     (Component : Node_Id;
      Node      : Node_Id)
     return Boolean;
   --  Perform the links between the different connection extremities
   --  and the actual features they represent.

   function Link_Feature
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link between the feature type to the actual data
   --  component.

   function Link_Subcomponent
     (Root    : Node_Id;
      Node    : Node_Id)
     return Boolean;
   --  Perform the link between the given subcomponent to the
   --  corresponding component.

   function Link_Flow_Spec
     (Component : Node_Id;
      Flow      : Node_Id)
     return Boolean;
   --  XXX: TBC

   function Link_Flow_Implementation
     (Component : Node_Id;
      Flow      : Node_Id)
     return Boolean;
   --  XXX: TBC

   function Link_End_To_End_Flow_Spec
     (Component : Node_Id;
      Flow      : Node_Id)
     return Boolean;
   --  XXX: TBC

   function Link_Mode_Transition
     (Component : Node_Id;
      Node      : Node_Id)
     return Boolean;
   --  XXX: TBC

   function Link_In_Modes_Statement
     (Component : Node_Id;
      In_Modes  : Node_Id)
     return Boolean;
   --  XXX: TBC

end Ocarina.Analyzer.AADL.Links;
