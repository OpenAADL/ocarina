------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . A N A L Y Z E R . A A D L . S E M A N T I C S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2012 ESA & ISAE.        --
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

--  This package checks the validity of the AADL model relatively to
--  the 'Semantics' parts of the AADL standard.

package Ocarina.Analyzer.AADL.Semantics is

   function Check_Semantics_In_Namespaces (Root : Node_Id) return Boolean;
   --  Verify that all the namespaces in the given architecture verify
   --  all the semantics rules given by the standard. Return True if
   --  all rules are respected.

   function Check_Semantics_In_Components (Root : Node_Id) return Boolean;
   --  Verify that all the components (and component implementations)
   --  in the given architecture respect their respective semantics
   --  rules given by the AADL standard. Return True if all rules are
   --  respected.

   function Check_Connection (Node : Node_Id) return Boolean;
   --  Verify that the given connection respects the semantics given
   --  by the standard (type compatibility, orientation
   --  compatibility...). Return True if all rules are respected.

   function Check_Semantics_Of_Properties (Root : Node_Id) return Boolean;
   --  Verify the validity of all property declarations. Return True
   --  is everything is OK, else False. Verify the type and value
   --  consistancies.

   function Check_Qualified_References
     (Container           : Node_Id;
      Qualified_Reference : Node_Id)
     return Boolean;
   --  Qualified_Reference refers to a Package::ComponentElt
   --  or to a PropertySet::PropertyElt, return true if the
   --  qualified name Package or PropertySet is present in
   --  'with' declarations in the package which contained
   --  the Qualified_Reference.

   procedure Reset_All_Connections (Root : Node_Id);
   --  Reset the name table info
   --  must be called before multiple iteration of Analyze

end Ocarina.Analyzer.AADL.Semantics;
