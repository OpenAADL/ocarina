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

with Types;

package Ocarina.Backends.PN.Iutils is

   function PN_Get_New_PN_Generated return Types.Node_Id;

   procedure PN_Init_PN_Generated (G : Types.Node_Id; F : Types.Value_Id);

   function PN_Get_New_TPN_Informations return Types.Node_Id;

   function PN_Get_New_Processor_Priority return Types.Node_Id;

   procedure PN_Init_Processor_Priority
     (PN_Proc : Types.Node_Id;
      Aadl_P  : Types.Node_Id);

   procedure Append_Trans_To_Processor
     (PN_T    : Types.Node_Id;
      PN_Proc : Types.Node_Id);

   function PN_Get_New_Processor_Pattern return Types.Node_Id;

   procedure PN_Init_Processor_Pattern
     (PN_Proc : Types.Node_Id;
      Aadl_P  : Types.Node_Id);

   function PN_Get_New_Thread_Pattern return Types.Node_Id;

   procedure PN_Init_Thread_Pattern
     (PN_T   : Types.Node_Id;
      Aadl_T : Types.Node_Id);

   function PN_Get_New_Port_Pattern return Types.Node_Id;

   procedure PN_Init_Port_Pattern
     (PN_P   : Types.Node_Id;
      Aadl_P : Types.Node_Id);

   function PN_Get_New_D_Port_Pattern return Types.Node_Id;

   procedure PN_Init_D_Port_Pattern
     (PN_DP   : Types.Node_Id;
      Aadl_DP : Types.Node_Id);

   function PN_Get_New_ED_Port_Pattern return Types.Node_Id;

   procedure PN_Init_ED_Port_Pattern
     (PN_DEP   : Types.Node_Id;
      Aadl_DEP : Types.Node_Id);

   function PN_Get_New_Call_Seq_Pattern return Types.Node_Id;

   procedure PN_Init_Call_Seq_Pattern
     (PN_CS  : Types.Node_Id;
      Aadl_T : Types.Node_Id);

   function PN_Get_New_Spg_Pattern return Types.Node_Id;

   procedure PN_Init_Spg_Pattern
     (PN_Spg   : Types.Node_Id;
      Aadl_Spg : Types.Node_Id);

   function PN_Get_New_Spg_Par_Pattern return Types.Node_Id;

   procedure PN_Init_Spg_Par_Pattern
     (PN_Spg_Par   : Types.Node_Id;
      Aadl_Spg_Par : Types.Node_Id);

   procedure PN_Init_PN_Component
     (PN_C   : Types.Node_Id;
      Aadl_C : Types.Node_Id);

   procedure PN_Init_PN_Node
     (PN_N   : Types.Node_Id;
      Aadl_N : Types.Node_Id;
      Name   : Types.Name_Id);

   procedure PN_Init_Node (N : Types.Node_Id);

   procedure PN_TPN_Init_Place
     (P    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long);

   procedure PN_Init_Place
     (P    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id);

   function PN_TPN_Get_New_Place return Types.Node_Id;

   procedure PN_TPN_Init_Transition
     (T    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long);

   procedure PN_Init_Transition
     (T    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id);

   function PN_TPN_Get_New_Transition return Types.Node_Id;

   procedure PN_TPN_Set_Guard
     (T           : Types.Node_Id;
      Low, Up     : Types.Value_Id;
      Braces_Mode : Types.Value_Id;
      Priority    : Types.Value_Id);

   function PN_TPN_Get_New_Arc return Types.Node_Id;

   procedure PN_TPN_Init_Arc
     (A    : Types.Node_Id;
      Aadl : Types.Node_Id;
      From : Types.Node_Id      := No_Node;
      To   : Types.Node_Id      := No_Node;
      K    : Unsigned_Long_Long := 0);

   procedure PN_TPN_Duplicate_Arc
     (A        : Types.Node_Id;
      A_Inst   : Types.Node_Id;
      Endpoint : Types.Node_Id;
      From     : Boolean := False);

   procedure PN_Init_Arc
     (A    : Types.Node_Id;
      Aadl : Types.Node_Id;
      From : Types.Node_Id;
      To   : Types.Node_Id;
      Name : Types.Name_Id);

   ----------------------

   function PN_Get_New_CPN_Informations return Types.Node_Id;

   function PN_CPN_Get_New_Place return Types.Node_Id;

   procedure PN_CPN_Init_Place
     (P    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long);

   function PN_CPN_Get_New_Transition return Types.Node_Id;

   procedure PN_CPN_Init_Transition
     (T    : Types.Node_Id;
      Aadl : Types.Node_Id;
      Name : Types.Name_Id;
      PN_G : Types.Node_Id;
      M    : Unsigned_Long_Long);

   function PN_CPN_Get_New_Arc return Types.Node_Id;

   procedure PN_CPN_Init_Arc
     (A    : Types.Node_Id;
      Aadl : Types.Node_Id;
      From : Types.Node_Id := No_Node;
      To   : Types.Node_Id := No_Node;
      K    : Unsigned_Long_Long);

   procedure PN_CPN_Duplicate_Arc
     (A        : Types.Node_Id;
      A_Inst   : Types.Node_Id;
      Endpoint : Types.Node_Id;
      From     : Boolean := False);

end Ocarina.Backends.PN.Iutils;
