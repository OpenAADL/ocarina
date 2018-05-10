------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
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

package body Types is

   --------
   -- No --
   --------

   function No (E : Node_Id) return Boolean is
   begin
      return E = No_Node;
   end No;

   -------------
   -- Present --
   -------------

   function Present (E : Node_Id) return Boolean is
   begin
      return E /= No_Node;
   end Present;

   --------------
   -- Safe_XOR --
   --------------

   function Safe_XOR (Right : Boolean; Left : Boolean) return Boolean is
   begin
      return (Right and then not Left) or else (Left and then not Right);
   end Safe_XOR;

   ---------------------
   -- Change_If_Empty --
   ---------------------

   procedure Change_If_Empty (Str_Ptr : in out String_Ptr; Value : String) is
   begin
      if Str_Ptr = null or else Str_Ptr.all = "" then
         if Str_Ptr /= null then
            Free (Str_Ptr);
         end if;

         Str_Ptr := new String'(Value);
      end if;
   end Change_If_Empty;

end Types;
