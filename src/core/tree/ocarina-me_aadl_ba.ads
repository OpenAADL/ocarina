------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                   O C A R I N A . M E _ A A D L _ B A                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

package Ocarina.ME_AADL_BA is

   type Behavior_State_Kind is
     (BSK_Error,
      BSK_No_Kind,
      BSK_Initial,
      BSK_Initial_Complete,
      BSK_Initial_Complete_Final,
      BSK_Initial_Final,
      BSK_Complete,
      BSK_Complete_Final,
      BSK_Final);
   --  Behavior_State_Kind for Behavior_State Node

   type Dispatch_Trigger_Kind is
     (TRI_Error, TRI_No_Kind, TRI_Abort, TRI_Stop, TRI_Timeout);
   --  Dispatch_Trigger_Kind for Dispatch_Trigger Node

   type Communication_Kind is
     (CK_Error,
      CK_No_Kind,
      CK_Exclamation,
      CK_Interrogative,
      CK_Greater_Greater,
      CK_Exclamation_Less_Than,
      CK_Exclamation_Greater_Than);
   --  Communication_Kind for Communication_Action Node

   type Distribution_Kind is
     (DK_Error, DK_No_Kind, DK_Fixed, DK_Normal, DK_Poisson, DK_Random);
   --  Distribution_Kind for Timing_Action Node

   type Operator_Kind is
     (OK_Error,
      OK_No_Kind,

   --  logical operator
      OK_And,                     --  and
      OK_Or,                      --  or
      OK_Xor,                     --  xor
      OK_Cand,                    --  cand
      OK_Cor,                     --  cor

   --  relational_operator
      OK_Equal,                   --  =
      OK_Non_Equal,               --  !=
      OK_Less_Than,               --  <
      OK_Less_Or_Equal,           --  <=
      OK_Greater_Than,            --  >
      OK_Greater_Or_Equal,        --  >=

   --  unary_adding_opetor
   --  binary_adding_operator
      OK_Plus,                    --  +
      OK_Minus,                   --  -
      OK_Concat,                  --  &

   --  multiplying operator
      OK_Multiply,                --  *
      OK_Divide,                  --  /
      OK_Mod,                     --  mod
      OK_Rem,                     --  rem

   --  highest precedence operator
      OK_Exponent,                --  **
      OK_Abs,                     --  abs
      OK_Not);                    --  not

   --  Subtype definitions

   subtype Logical_Operator is Operator_Kind range OK_And .. OK_Or;

   subtype Relational_Operator is
     Operator_Kind range OK_Equal .. OK_Greater_Or_Equal;

   subtype Unary_Adding_Operator is Operator_Kind range OK_Plus .. OK_Minus;

   subtype Binary_Adding_Operator is Operator_Kind range OK_Plus .. OK_Concat;

   subtype Multiplying_Operator is Operator_Kind range OK_Multiply .. OK_Rem;

   subtype Highest_Prec_Operator is Operator_Kind range OK_Exponent .. OK_Not;

end Ocarina.ME_AADL_BA;
