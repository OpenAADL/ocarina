------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . P N . P R I N T E R           --
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

--  generic

--     with procedure Print_Place (Pn_Generated : Node_Id;
--                                 Pn_P : Node_Id);

--     with procedure Print_Trans (Pn_Generated : Node_Id;
--                                 Pn_T : Node_Id);

--     with procedure Print_Formalism_Information
--       (Pn_Generated : Node_Id);

package Ocarina.Backends.PN.Printer is

   type P_Print_Place is access procedure
     (Pn_Generated : Node_Id;
      Pn_P         : Node_Id);

   type P_Print_Trans is access procedure
     (Pn_Generated : Node_Id;
      Pn_T         : Node_Id);

   type P_Print_Formalism_Information is access procedure
     (Pn_Generated : Node_Id);

   procedure Set_Printers
     (P_Place    : P_Print_Place;
      P_Trans    : P_Print_Trans;
      P_Form_Inf : P_Print_Formalism_Information);

   procedure Print_Pn_Generated (Pn_Generated : Node_Id);

   procedure Print_Pn_Box (Pn_Generated : Node_Id);

   procedure Print_Thread (Pn_Thread : Node_Id; Pn_Generated : Node_Id);

   procedure Print_Call_Seq (Pn_Call : Node_Id; Pn_Generated : Node_Id);

   procedure Print_Component (Pn_Sub : Node_Id; Pn_Generated : Node_Id);

   --  XXX TODO : print_devices / print_processor / print_memory

private

   Proc_Print_Place                 : P_Print_Place;
   Proc_Print_Trans                 : P_Print_Trans;
   Proc_Print_Formalism_Information : P_Print_Formalism_Information;

end Ocarina.Backends.PN.Printer;
