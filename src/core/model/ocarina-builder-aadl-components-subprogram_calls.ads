------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            OCARINA.BUILDER.AADL.COMPONENTS.SUBPROGRAM_CALLS              --
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

package Ocarina.Builder.AADL.Components.Subprogram_Calls is

   function Add_Property_Association
     (Subprogram_Call      : Node_Id;
      Property_Association : Node_Id)
     return Boolean;
   --  Add a property association to the subprogram
   --  call. Subprogram_Call must reference a subprogram call (not a
   --  call sequence). Property_Association references the property
   --  association. Return True if everything went right, else False.

   function Add_Subprogram_Call
     (Call_Sequence   : Node_Id;
      Subprogram_Call : Node_Id)
     return Boolean;
   --  Add a subprogram call to the subprogram call
   --  sequence. Subprogram_Call must reference a subprogram call (not
   --  a call sequence). Call_Sequence references the subprogram call
   --  sequence. Return True if everything went right, else False.

   function Add_New_Subprogram_Call
     (Loc           : Location;
      Name          : Node_Id;
      Call_Sequence : Node_Id)
     return Node_Id;
   --  Create and add a new subprogram call into a subprogram call
   --  sequence. Loc is the location of the call sequence in the
   --  parsed text. Name references an identifier which contains the
   --  name of the subprogram call. Call_Sequence references the
   --  subprogram call sequence that contains the subprogram call.
   --  The function return the Node_Id of the newly created subprogram
   --  call if everything went right, else No_Node.

   function Add_New_Subprogram_Call_Sequence
     (Loc       : Location;
      Name      : Node_Id;
      Comp_Impl : Node_Id;
      In_Modes  : Node_Id := No_Node)
     return Node_Id;
   --  Create and add a new subprogram call sequence into a component
   --  implementation. Loc is the location of the call sequence in the
   --  parsed text. Name references an identifier which contains the
   --  name of the call sequence, if any. Comp_Impl references the
   --  component implementation.  In_Modes contains the list of the
   --  modes associated to the connection. Name can be No_Node, if the
   --  sequence is not nammed. Subprogram calls Return the Node_Id of
   --  the newly created call sequence if everything went right, else
   --  No_Node.

end Ocarina.Builder.AADL.Components.Subprogram_Calls;
