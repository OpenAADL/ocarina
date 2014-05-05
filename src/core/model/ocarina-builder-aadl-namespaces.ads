------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L . N A M E S P A C E S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

with Ocarina.Types;
with Locations;

with Ocarina.ME_AADL;

package Ocarina.Builder.AADL.Namespaces is

   function Add_Declaration
     (Namespace : Ocarina.Types.Node_Id;
      Element : Ocarina.Types.Node_Id)
     return Boolean;
   --  Insert any component, property_set, package or port_group into
   --  the AADL specification. Namespace must reference the node
   --  created with Initialize_Unnamed_Namespace or a package
   --  specification. Return True if the element was correctly
   --  inserted, else False

   function Initialize_Unnamed_Namespace
     (Loc : Locations.Location)
     return Ocarina.Types.Node_Id;
   --  Create the AADL specification node, which corresponds to the
   --  top level of the AADL description. This function must be
   --  invoked first, as all the other elements of the description
   --  will be added to this one. Loc is the location of the AADL
   --  specification in the parsed text. Return a reference to the
   --  newly created node if everything went right, else False.

   function Add_New_Package
     (Loc : Locations.Location;
      Pack_Name : Ocarina.Types.Node_Id;
      Namespace : Ocarina.Types.Node_Id)
     return Ocarina.Types.Node_Id;
   --  Checks if a package of that name already exists. If so, return
   --  this one, else create a new one and return it. Loc is the
   --  location of the package specification in the parsed
   --  text. Pack_Name is a Node_Id referencing an identifier which
   --  contains the name of the package. Namespace must reference the
   --  top level AADL specification node.

   function Add_New_Package_Name
     (Loc            : Locations.Location;
      Identifiers    : Ocarina.Types.List_Id)
     return Ocarina.Types.Node_Id;

   function Add_Property_Association
     (Pack : Ocarina.Types.Node_Id;
      Property_Association : Ocarina.Types.Node_Id)
     return Boolean;
   --  Add a property association to the list of the package
   --  properties, without checking for homonyms or whatever. This
   --  function should be only used by other functions of the core
   --  API. Namespace must reference a package specification. Return
   --  True if the property was added, else False.

   function Add_New_Name_Visibility_Declaration
     (Loc         : Locations.Location;
      Namespace   : Ocarina.Types.Node_Id;
      List_Items  : Ocarina.Types.List_Id;
      Is_Private  : Boolean := False)
     return Ocarina.Types.Node_Id;
   --  Create the name visibility declaration node to the list of
   --  the package declarations, without checking.

   function Add_New_Import_Declaration
     (Loc         : Locations.Location;
      Namespace   : Ocarina.Types.Node_Id;
      List_Items  : Ocarina.Types.List_Id;
      Is_Private  : Boolean := False)
     return Ocarina.Types.Node_Id;
   --  Create the import node to the list of the name visibility declarations,
   --  without checking.

   function Add_New_Alias_Declaration
     (Loc            : Locations.Location;
      Namespace      : Ocarina.Types.Node_Id;
      Identifier     : Ocarina.Types.Node_Id;
      Package_Name   : Ocarina.Types.Node_Id;
      Classifier_Ref : Ocarina.Types.Node_Id;
      Entity_Cat     : Ocarina.ME_AADL.Entity_Category;
      Component_Cat  : Ocarina.ME_AADL.Component_Category;
      Is_All         : Boolean := False;
      Is_Private     : Boolean := False)
     return Ocarina.Types.Node_Id;
   --  Create the alias node to the list of the name visibility declarations,
   --  without checking.

end Ocarina.Builder.AADL.Namespaces;
