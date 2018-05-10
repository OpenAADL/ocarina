------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                   O C A R I N A . M E _ A O 4 A A D L                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

package Ocarina.ME_AO4AADL is

   type Component_Category is
     (CC_Subprogram,
      CC_Thread,
      CC_Process,
      CC_System,
      CC_Unknown);

   type Advice_Category is
     (AC_Before,
      AC_After,
      AC_Around,
      AC_Unknown);

   type Com_Kind is
     (CK_Exclamation,
      CK_Interrogation,
      CK_Unknown);

   type Caller_Kind is
     (CK_Call,
      CK_Execution,
      CK_Unknown);

   type Operator_Kind is
     (OK_Error,
      OK_No_Kind,

      --  logical operator
      OK_And,                     --  and
      OK_Or,                      --  or
      OK_Concat,                  --  &&
      OK_Or_Logic,                --  ||

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

   subtype Logical_Operator is Operator_Kind
     range OK_And .. OK_Or;

   subtype Relational_Operator is Operator_Kind
     range OK_Equal  .. OK_Greater_Or_Equal;

   subtype Unary_Adding_Operator is Operator_Kind
     range OK_Plus .. OK_Minus;

   subtype Binary_Adding_Operator is Operator_Kind
     range OK_Equal .. OK_Greater_Or_Equal;

   subtype Multiplying_Operator is Operator_Kind
     range OK_Multiply .. OK_Rem;

   subtype Highest_Prec_Operator is Operator_Kind
     range OK_Exponent .. OK_Not;

end Ocarina.ME_AO4AADL;
