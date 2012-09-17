------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . B A C K E N D S . R E A L                 --
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

--  Resolves REAL theorems for a given AADL model

with Ocarina.Instances.REAL_Checker.Queries;

package Ocarina.Backends.REAL is
   use Ocarina.Instances.REAL_Checker.Queries;

   procedure Generate (AADL_Root : Node_Id);
   --  Display the result of all the REAL theorems execution on
   --  the specified AADL model

   procedure Init;
   --  Initialize the REAL module

   procedure Reset;
   --  Reset the REAL module

   procedure Compute_Theorem_Call
     (E         : Node_Id;
      Local_Set : Result_Set;
      Result    : out Value_Id;
      Success   : out Boolean);
   --  Allows to call directly returning theorems with user-specified
   --  domain, without using top level theorems

private

   --  Internal types declarations

   type Set_Table is array (Integer range <>)
     of Ocarina.Instances.REAL_Checker.Queries.Result_Set;

   type Set_Table_Access is access Set_Table;

   --  Global sets array
   --  all existing sets are declared within this array

   Set_Array : Set_Table_Access;

end Ocarina.Backends.REAL;
