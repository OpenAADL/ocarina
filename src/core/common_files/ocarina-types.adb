------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2007-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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
