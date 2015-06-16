------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . P N . P R I N T E R           --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Backends.PN.Nodes;
with Ocarina.Backends.PN.Nutils;

package body Ocarina.Backends.PN.Printer is

   package OPN renames Ocarina.Backends.PN.Nodes;
   package OPU renames Ocarina.Backends.PN.Nutils;

   --------------------
   --  Set_Printers  --
   --------------------

   procedure Set_Printers
     (P_Place    : P_Print_Place;
      P_Trans    : P_Print_Trans;
      P_Form_Inf : P_Print_Formalism_Information)
   is
   begin
      Proc_Print_Place                 := P_Place;
      Proc_Print_Trans                 := P_Trans;
      Proc_Print_Formalism_Information := P_Form_Inf;
   end Set_Printers;

   --------------------------
   --  Print_Pn_Generated  --
   --------------------------

   procedure Print_Pn_Generated (Pn_Generated : Node_Id) is
   begin
      Proc_Print_Formalism_Information.all (Pn_Generated);
      Print_Pn_Box (Pn_Generated);
   end Print_Pn_Generated;

   --------------------
   --  Print_Pn_Box  --
   --------------------

   procedure Print_Pn_Box (Pn_Generated : Node_Id) is
      use OPN;
      use OPU;

      Node_Iter : Node_Id;
   begin
      if not Is_Empty (Pn_Subcomponents (Pn_Box (Pn_Generated))) then
         Node_Iter :=
           OPN.First_Node (Pn_Subcomponents (Pn_Box (Pn_Generated)));
         while Present (Node_Iter) loop
            if OPN.Kind (Node_Iter) = K_Thread_Pattern then
               Print_Thread (Node_Iter, Pn_Generated);
            else
               Print_Component (Node_Iter, Pn_Generated);
            end if;

            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;
      if not Is_Empty (Pn_Interconnections (Pn_Box (Pn_Generated))) then
         Node_Iter :=
           OPN.First_Node (Pn_Interconnections (Pn_Box (Pn_Generated)));
         while Present (Node_Iter) loop
            Proc_Print_Place.all (Pn_Generated, Node_Iter);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;
   end Print_Pn_Box;

   --------------------
   --  Print_Thread  --
   --------------------

   procedure Print_Thread (Pn_Thread : Node_Id; Pn_Generated : Node_Id) is
      use OPN;
      use OPU;

      Node_Iter : Node_Id;
   begin
      Print_Component (Pn_Thread, Pn_Generated);

      --  call sequences
      if not Is_Empty (Call_Seq (Pn_Thread)) then
         Node_Iter := OPN.First_Node (Call_Seq (Pn_Thread));
         while Present (Node_Iter) loop
            Print_Call_Seq (Node_Iter, Pn_Generated);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;

      --  in ports
      if not Is_Empty (In_Ports (Pn_Thread)) then
         Node_Iter := OPN.First_Node (In_Ports (Pn_Thread));
         while Present (Node_Iter) loop
            Print_Component (Node_Iter, Pn_Generated);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;

      --  out ports
      if not Is_Empty (Out_Ports (Pn_Thread)) then
         Node_Iter := OPN.First_Node (Out_Ports (Pn_Thread));
         while Present (Node_Iter) loop
            Print_Component (Node_Iter, Pn_Generated);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;

   end Print_Thread;

   ----------------------
   --  Print_Call_Seq  --
   ----------------------

   procedure Print_Call_Seq (Pn_Call : Node_Id; Pn_Generated : Node_Id) is
      use OPN;
      use OPU;

      Node_Iter : Node_Id;
   begin
      if not Is_Empty (Spg_Call (Pn_Call)) then
         Node_Iter := OPN.First_Node (Spg_Call (Pn_Call));
         while Present (Node_Iter) loop
            Print_Component (Node_Iter, Pn_Generated);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;
   end Print_Call_Seq;

   -----------------------
   --  Print_Component  --
   -----------------------

   procedure Print_Component (Pn_Sub : Node_Id; Pn_Generated : Node_Id) is
      use OPN;
      use OPU;

      Node_Iter : Node_Id;
   begin
      if not Is_Empty (Public_Interfaces (Pn_Sub)) then
         Node_Iter := OPN.First_Node (Public_Interfaces (Pn_Sub));
         while Present (Node_Iter) loop
            Proc_Print_Trans.all (Pn_Generated, Node_Iter);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;

      end if;
      if not Is_Empty (Internal_Transitions (Pn_Sub)) then
         Node_Iter := OPN.First_Node (Internal_Transitions (Pn_Sub));
         while Present (Node_Iter) loop
            Proc_Print_Trans.all (Pn_Generated, Node_Iter);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;

      if not Is_Empty (Internal_Places (Pn_Sub)) then
         Node_Iter := OPN.First_Node (Internal_Places (Pn_Sub));
         while Present (Node_Iter) loop
            Proc_Print_Place.all (Pn_Generated, Node_Iter);
            Node_Iter := OPN.Next_Node (Node_Iter);
         end loop;
      end if;
   end Print_Component;

end Ocarina.Backends.PN.Printer;
