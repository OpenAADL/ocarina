------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . P N . I U T I L S            --
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

package Ocarina.Backends.PN.Iutils is

   function PN_Get_New_PN_Generated return Ocarina.Types.Node_Id;

   procedure PN_Init_PN_Generated
     (G : Ocarina.Types.Node_Id; F : Ocarina.Types.Value_Id);

   function PN_Get_New_TPN_Informations return Ocarina.Types.Node_Id;

   function PN_Get_New_Processor_Priority return Ocarina.Types.Node_Id;

   procedure PN_Init_Processor_Priority
     (PN_Proc : Ocarina.Types.Node_Id;
      Aadl_P  : Ocarina.Types.Node_Id);

   procedure Append_Trans_To_Processor
     (PN_T    : Ocarina.Types.Node_Id;
      PN_Proc : Ocarina.Types.Node_Id);

   function PN_Get_New_Processor_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_Processor_Pattern
     (PN_Proc : Ocarina.Types.Node_Id;
      Aadl_P  : Ocarina.Types.Node_Id);

   function PN_Get_New_Thread_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_Thread_Pattern
     (PN_T   : Ocarina.Types.Node_Id;
      Aadl_T : Ocarina.Types.Node_Id);

   function PN_Get_New_Port_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_Port_Pattern
     (PN_P   : Ocarina.Types.Node_Id;
      Aadl_P : Ocarina.Types.Node_Id);

   function PN_Get_New_D_Port_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_D_Port_Pattern
     (PN_DP   : Ocarina.Types.Node_Id;
      Aadl_DP : Ocarina.Types.Node_Id);

   function PN_Get_New_ED_Port_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_ED_Port_Pattern
     (PN_DEP   : Ocarina.Types.Node_Id;
      Aadl_DEP : Ocarina.Types.Node_Id);

   function PN_Get_New_Call_Seq_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_Call_Seq_Pattern
     (PN_CS  : Ocarina.Types.Node_Id;
      Aadl_T : Ocarina.Types.Node_Id);

   function PN_Get_New_Spg_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_Spg_Pattern
     (PN_Spg   : Ocarina.Types.Node_Id;
      Aadl_Spg : Ocarina.Types.Node_Id);

   function PN_Get_New_Spg_Par_Pattern return Ocarina.Types.Node_Id;

   procedure PN_Init_Spg_Par_Pattern
     (PN_Spg_Par   : Ocarina.Types.Node_Id;
      Aadl_Spg_Par : Ocarina.Types.Node_Id);

   procedure PN_Init_PN_Component
     (PN_C   : Ocarina.Types.Node_Id;
      Aadl_C : Ocarina.Types.Node_Id);

   procedure PN_Init_PN_Node
     (PN_N   : Ocarina.Types.Node_Id;
      Aadl_N : Ocarina.Types.Node_Id;
      Name   : Ocarina.Types.Name_Id);

   procedure PN_Init_Node (N : Ocarina.Types.Node_Id);

   procedure PN_TPN_Init_Place
     (P    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      Name : Ocarina.Types.Name_Id;
      PN_G : Ocarina.Types.Node_Id;
      M    : Unsigned_Long_Long);

   procedure PN_Init_Place
     (P    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      Name : Ocarina.Types.Name_Id);

   function PN_TPN_Get_New_Place return Ocarina.Types.Node_Id;

   procedure PN_TPN_Init_Transition
     (T    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      Name : Ocarina.Types.Name_Id;
      PN_G : Ocarina.Types.Node_Id;
      M    : Unsigned_Long_Long);

   procedure PN_Init_Transition
     (T    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      Name : Ocarina.Types.Name_Id);

   function PN_TPN_Get_New_Transition return Ocarina.Types.Node_Id;

   procedure PN_TPN_Set_Guard
     (T           : Ocarina.Types.Node_Id;
      Low, Up     : Ocarina.Types.Value_Id;
      Braces_Mode : Ocarina.Types.Value_Id;
      Priority    : Ocarina.Types.Value_Id);

   function PN_TPN_Get_New_Arc return Ocarina.Types.Node_Id;

   procedure PN_TPN_Init_Arc
     (A    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      From : Ocarina.Types.Node_Id      := No_Node;
      To   : Ocarina.Types.Node_Id      := No_Node;
      K    : Unsigned_Long_Long := 0);

   procedure PN_TPN_Duplicate_Arc
     (A        : Ocarina.Types.Node_Id;
      A_Inst   : Ocarina.Types.Node_Id;
      Endpoint : Ocarina.Types.Node_Id;
      From     : Boolean := False);

   procedure PN_Init_Arc
     (A    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      From : Ocarina.Types.Node_Id;
      To   : Ocarina.Types.Node_Id;
      Name : Ocarina.Types.Name_Id);

   ----------------------

   function PN_Get_New_CPN_Informations return Ocarina.Types.Node_Id;

   function PN_CPN_Get_New_Place return Ocarina.Types.Node_Id;

   procedure PN_CPN_Init_Place
     (P    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      Name : Ocarina.Types.Name_Id;
      PN_G : Ocarina.Types.Node_Id;
      M    : Unsigned_Long_Long);

   function PN_CPN_Get_New_Transition return Ocarina.Types.Node_Id;

   procedure PN_CPN_Init_Transition
     (T    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      Name : Ocarina.Types.Name_Id;
      PN_G : Ocarina.Types.Node_Id;
      M    : Unsigned_Long_Long);

   function PN_CPN_Get_New_Arc return Ocarina.Types.Node_Id;

   procedure PN_CPN_Init_Arc
     (A    : Ocarina.Types.Node_Id;
      Aadl : Ocarina.Types.Node_Id;
      From : Ocarina.Types.Node_Id := No_Node;
      To   : Ocarina.Types.Node_Id := No_Node;
      K    : Unsigned_Long_Long);

   procedure PN_CPN_Duplicate_Arc
     (A        : Ocarina.Types.Node_Id;
      A_Inst   : Ocarina.Types.Node_Id;
      Endpoint : Ocarina.Types.Node_Id;
      From     : Boolean := False);

end Ocarina.Backends.PN.Iutils;
