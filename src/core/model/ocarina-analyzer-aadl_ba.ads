------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . A N A L Y Z E R . A A D L _ B A              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2016-2019 ESA & ISAE.                    --
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

package Ocarina.Analyzer.AADL_BA is

   procedure Init;

   procedure Reset;

   function Analyze_Model (Root : Node_Id) return Boolean;
   --  Proceed to BA analysis

   function Is_Complete
     (BA_Root : Node_Id;
      State   : Node_Id)
     return Boolean;

   function Is_Initial
     (BA_Root : Node_Id;
      State   : Node_Id)
     return Boolean;

   function Is_Final
     (BA_Root : Node_Id;
      State   : Node_Id)
      return Boolean;

   function Find_BA_Variable
     (Node             : Node_Id;
      BA_Root          : Node_Id)
      return Node_Id;

end Ocarina.Analyzer.AADL_BA;
