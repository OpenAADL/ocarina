------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . M E _ A A D L _ E M A                   --
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

package Ocarina.ME_AADL_EMA is

   type Operator_Kind is
     (OK_Error,
      OK_No_Kind,

      --  logical operator
      OK_And,                     --  and
      OK_Or,                      --  or
      OK_OrMore,                  --  ormore
      OK_OrLess,                  --  orless
      OK_Not                      --  not
     );

   type Binding_Kind is
     (BK_Processor,
      BK_Memory,
      BK_Connection,
      BK_Binding,
      BK_Bindings,
      BK_Unknown);

   type Propagation_Kind is
     (PK_In,
      PK_Out,
      PK_Unknown);

end Ocarina.ME_AADL_EMA;
