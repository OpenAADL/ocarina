------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                              O C A R I N A                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2004-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

package Ocarina is

   type AADL_Version_Type is (AADL_V1, AADL_V2);
   --  Define AADL Version 1.0 and AADL Version 2.0

   Default_AADL_Version : AADL_Version_Type;
   --  Default AADL version initialised by main with Get_Default_AADL_Version.

   AADL_Version : AADL_Version_Type;

   procedure Initialize;
   --  Initialize the Ocarina core

   procedure Reset;
   --  Reset the node tree. All the information related to the node
   --  will be lost.

   function GNU_Make_Cmd return String;
   --  Return the command used to invoke the GNU make on the current
   --  platform. Fetched at configure time.

private

   function Default_GNU_Make return String;
   --  Return the default executable name for GNU make

   type GNU_Make_Access is access function return String;
   GNU_Make_Ptr : GNU_Make_Access := Default_GNU_Make'Access;

end Ocarina;
