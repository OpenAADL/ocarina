------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.PROPERTIES.ARINC653                    --
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

with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.Instances.Queries; use Ocarina.Instances.Queries;

with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.AADL_Values;
use Ocarina.AADL_Values;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
use Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Backends.Properties.ARINC653 is

   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   Is_Initialized : Boolean := False;

   --  ARINC653::Schedule_Window
   --   Schedule_Window_Name : Name_Id;

   --  ARINC653::Schedule_Window associated anonymous record element term
   Partition_Name : Name_Id;
   Duration_Name : Name_Id;
   Periodic_Processing_Start_Name : Name_Id;

   --  ARINC653::Module_Schedule
   Module_Schedule_Name : Name_Id;

   procedure Init;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
--        Schedule_Window_Name := Get_String_Name
--          ("arinc::schedule_window");

      Module_Schedule_Name := Get_String_Name
        ("arinc653::module_schedule");

      Partition_Name := Get_String_Name ("partition");
      Duration_Name := Get_String_Name ("duration");
      Periodic_Processing_Start_Name
        := Get_String_Name ("periodic_processing_start");
      Is_Initialized := True;
   end Init;

   ----------------------------------
   -- Get_Module_Schedule_Property --
   ----------------------------------

   function Get_Module_Schedule_Property
     (E : Node_Id)
     return Schedule_Window_Record_Term_Array
   is
      pragma Assert (True or else
                     AINU.Is_Processor (E) or else
                       AINU.Is_Virtual_Processor (E));

      Property_Value : List_Id;

   begin
      if not Is_Initialized then
         Init;
      end if;

      if not Is_Defined_List_Property (E, Module_Schedule_Name) then
         return Empty_Schedule_Window_Record_Term_Array;
      end if;

      Property_Value := Get_List_Property (E, Module_Schedule_Name);

      declare
         Result : Schedule_Window_Record_Term_Array
           (1 .. ATNU.Length (Property_Value));
         A : Node_Id := First_Node (Property_Value);
         J : Integer := Result'First;
      begin
         while Present (A) loop
            declare
               L : Node_Id := First_Node (List_Items (A));
               V : Ocarina.AADL_Values.Value_Type;

            begin
               while Present (L) loop
                  if Name (Identifier (L)) = Partition_Name then
                     --  Partition is a component reference

                     Result (J).Partition :=
                       Entity
                       (Reference_Term (Property_Expression (L)));

                  elsif Name (Identifier (L)) = Duration_Name then
                     Result (J).Duration := Convert_Value_To_Time_Type
                       (Property_Expression (L));

                  elsif Name (Identifier (L))
                    = Periodic_Processing_Start_Name
                  then
                     --  Periodic_Processing_Start is an aadlboolean,
                     --  the corresponding Property_expression is thus
                     --  an AADL value

                     V := Get_Value_Type (Value (Property_Expression (L)));
                     Result (J).Periodic_Processing_Start := V.BVal;

                  else
                     raise Program_Error;
                  end if;

                  L := Next_Node (L);

               end loop;
            end;

            A := Next_Node (A);
            J := J + 1;
         end loop;

         return Result;
      end;
   end Get_Module_Schedule_Property;

end Ocarina.Backends.Properties.ARINC653;
