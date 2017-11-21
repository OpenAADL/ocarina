------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B E _ A A D L . N A M E S P A C E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

package Ocarina.BE_AADL.Namespaces is

   procedure Print_AADL_Specification (Node : Node_Id);
   --  Print the AADL source corresponding to the given AADL
   --  specification node.

   generic
      with function Is_Printable
        (Node      : Node_Id;
         Criterion : Node_Id) return Boolean;
      --  This is a general purpose comparison routine. The Criterion
      --  may not be necessary in some cases.

   procedure Print_Constrained_AADL_Specification
     (Node      : Node_Id;
      Criterion : Node_Id);
   --  Same as above but only prints the AADL declarations for which
   --  the given Is_Printable (<Node>, Criterion) is True.

   procedure Print_Package (Node : Node_Id);
   --  Print the AADL source correspodning to the given package
   --  specification node.

   generic
      with function Is_Printable
        (Node      : Node_Id;
         Criterion : Node_Id) return Boolean;
      --  This is a general purpose comparison routine. The Criterion
      --  may not be necessary in some cases.

   procedure Print_Constrained_Package (Node : Node_Id; Criterion : Node_Id);
   --  Same as above but only prints the AADL declarations for which
   --  the given Is_Printable (<Node>, Criterion) is True.

   procedure Print_Property_Set (Node : Node_Id);
   --  Prints the AADL source corresponding to the given property set
   --  node.

   generic
      with function Is_Printable
        (Node      : Node_Id;
         Criterion : Node_Id) return Boolean;
      --  This is a general purpose comparison routine. The Criterion
      --  may not be necessary in some cases.

   procedure Print_Constrained_Property_Set
     (Node      : Node_Id;
      Criterion : Node_Id);
   --  Same as above but only prints the AADL declarations for which
   --  the given Is_Printable (<Node>, Criterion) is True.

   procedure Print_Name_Visibility_Declaration (Node : Node_Id);

end Ocarina.BE_AADL.Namespaces;
