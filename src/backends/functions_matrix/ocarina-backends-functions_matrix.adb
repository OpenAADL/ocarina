------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . F U N C T I O N S _ M A T R I X     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2018 ESA & ISAE.                    --
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

with Ocarina.Instances;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Expander;
with Ocarina.Backends.XML_Tree.Generator;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Functions_Matrix.Main;

package body Ocarina.Backends.Functions_Matrix is

   use Ocarina.Instances;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Utils;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
   begin

      Instance_Root := Instantiate_Model (AADL_Root);

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);
      --  Abort if the construction of the tree failed

      if No (XML_Root) then
         Display_Error
           ("Functional traceability matrix " & "generation failed",
            Fatal => True);
      end if;

      --  At this point, we have a valid tree, we can begin the XML
      --  file generation.

      --  Enter the output directory

      Enter_Directory (Generated_Sources_Directory);

      if not Remove_Generated_Sources then
         --  Create the source files

         XML_Tree.Generator.Generate (XML_Root);

      end if;

      --  Leave the output directory
      Leave_Directory;
   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend
        ("Function_Matrix",
         Generate'Access,
         Functions_Matrix_Analysis);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      Main.Visit (E);

   end Visit_Architecture_Instance;

end Ocarina.Backends.Functions_Matrix;
