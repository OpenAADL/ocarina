------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . A N A L Y Z E R . A A D L . N A M I N G _ R U L E S    --
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

with GNAT.Table;

package Ocarina.Analyzer.AADL.Naming_Rules is

   type Scope_Stack_Entry is record
      Node : Node_Id;
   end record;

   No_Scope_Depth : constant Int := -1;
   package Scope_Stack is new GNAT.Table
     (Scope_Stack_Entry,
      Int,
      No_Scope_Depth + 1,
      10,
      10);

   procedure Initialize;

   procedure Push_Scope (Scope : Node_Id);

   procedure Pop_Scope;

   function Current_Scope return Node_Id;
   --  Return current scope

   function Node_Explicitly_In_Scope
     (Identifier : Node_Id;
      Scope      : Node_Id) return Node_Id;
   --  Find whether there is a definition for identifier Identifier in
   --  scope Scope. Return the corresponding entity if the identifier
   --  exists in the scope, else No_Node.

   function Node_Explicitly_In_Scope
     (Name_Of_Identifier : Name_Id;
      Scope              : Node_Id) return Node_Id;

   function Node_In_Scope
     (Identifier : Node_Id;
      Scope      : Node_Id) return Node_Id;
   --  Find whether there is a definition for identifier Identifier in
   --  scope Scope or the above scopes. The Homonym field of the
   --  identifier of the returned node points to the next homonym of
   --  the node in the scopes, so that it is possible to get all the
   --  homonyms.

   function Node_In_Scope
     (Name_Of_Identifier : Name_Id;
      Scope              : Node_Id) return Node_Id;

   function Enter_Name_In_Scope (Identifier : Node_Id) return Boolean;
   --  Detect naming conflict with Identifier. Conflict happens when
   --  an identifier already exists in the scope, corresponding to an
   --  entity of the same kind (e.g. mode, subcomponent, feature,
   --  etc.), except if they are in different modes. In case of
   --  success, add Identifier to the current scope and return True;
   --  else False.

   function Enter_Name_In_Scope (Identifier : Node_Id) return Node_Id;

   function Remove_From_Homonyms
     (First_Homonym     : Node_Id;
      Homonym_To_Remove : Node_Id) return Node_Id;
   --  Remove Homonym_To_Remove from the homonym list whose first
   --  element is First_Homonym, and return the homonym after
   --  Homonym_To_Remove (which may be No_Node)

   procedure Remove_From_Scope (Identifier : Node_Id; Scope : Node_Id);
   --  Remove from Scope the first entity in Homonym chain

   D_Scopes : constant Boolean := False;
   --  Control the display of debug messages

end Ocarina.Analyzer.AADL.Naming_Rules;
