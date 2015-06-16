------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . A L L O Y                --
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

with Charset; use Charset;
with Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Instances;           use Ocarina.Instances;
with Ocarina.ME_AADL.AADL_Instances.Entities;
use Ocarina.ME_AADL.AADL_Instances.Entities;

with GNAT.Command_Line;
with Ocarina.Backends.Utils;
with Ada.Text_IO;

package body Ocarina.Backends.Alloy is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   use Ocarina.Namet;
   use Ada.Text_IO;
   use Ocarina.ME_AADL;
   use Ocarina.Backends.Utils;
   use AIN;

   procedure Visit (E : Node_Id);

   procedure Visit_Architecture_Instance (E : Node_Id);

   procedure Visit_Component_Instance (E : Node_Id);

   FD            : File_Type;
   Root_System_Name : Name_Id;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Visit_Architecture_Instance (E);

         when K_Component_Instance =>
            Visit_Component_Instance (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      Visit (Root_System (E));
   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category_Name_String : constant array
        (Component_Category'Range) of Name_Id :=
        (CC_Abstract          => Get_String_Name ("abstract"),
         CC_Bus               => Get_String_Name ("bus"),
         CC_Data              => Get_String_Name ("data"),
         CC_Device            => Get_String_Name ("device"),
         CC_Memory            => Get_String_Name ("memory"),
         CC_Process           => Get_String_Name ("process"),
         CC_Processor         => Get_String_Name ("processor"),
         CC_Subprogram        => Get_String_Name ("subprogram"),
         CC_Subprogram_Group  => Get_String_Name ("subprogram group"),
         CC_System            => Get_String_Name ("system"),
         CC_Thread            => Get_String_Name ("thread"),
         CC_Thread_Group      => Get_String_Name ("thread group"),
         CC_Unknown           => No_Name,
         CC_Virtual_Bus       => Get_String_Name ("virtual_bus"),
         CC_Virtual_Processor => Get_String_Name ("virtual_processor"));

      Category : constant Component_Category := Get_Category_Of_Component (E);

      T : Node_Id;
   begin
      --  Create Alloy component

      --  Rule #1: the name of this component is deduced from the name
      --  of the corresponding instance name

      if Present (Parent_Subcomponent (E)) then
         Put_Line (FD, "one sig " &
                     To_Lower
                     (Get_Name_String
                        (Normalize_Name
                           (Fully_Qualified_Instance_Name (E))))
                     & " extends Component{}{");
      else
         Put_Line (Kind (E)'Img);
         Root_System_Name := Normalize_Name
           (Display_Name (Identifier (E)));

         Put_Line (FD, "one sig " &
                     To_Lower (Get_Name_String (Root_System_Name))
                     & " extends Component{}{");

      end if;

      Put_Line (FD, ASCII.HT & "type="
                  & Get_Name_String (Category_Name_String (Category)));

      --  Rule #2: list subcomponents

      Put (FD, ASCII.HT & "subcomponents=");
      if Present (Subcomponents (E)) then
         T := First_Node (Subcomponents (E));
         while Present (T) loop
            declare
               Subcomponent_Name : constant String :=
                 To_Lower
                 (Get_Name_String
                    (Normalize_Name
                       (Fully_Qualified_Instance_Name
                          (Corresponding_Instance (T)))));
            begin
               Put (FD, Subcomponent_Name);

               T := Next_Node (T);
               if Present (T) then
                  Put (FD, "+");
               end if;
            end;
         end loop;
         New_Line (FD);

      else
         Put_Line (FD, "none");
      end if;

      --  Rule#3: list properties

      Put (FD, ASCII.HT & "properties=");
      if Present (AIN.Properties (E)) then
         T := First_Node (AIN.Properties (E));
         while Present (T) loop
            Put (FD,
                 To_Lower
                   (Get_Name_String
                      (Normalize_Name
                         (Display_Name
                            (Identifier (T))))));
            T := Next_Node (T);
            if Present (T) then
               Put (FD, "+");
            end if;
         end loop;
         New_Line (Fd);

      else
         Put_Line (FD, "none");
      end if;

      --  Close create of component

      Put_Line (FD, "}");
      New_Line (FD);

      --  Iterate over subcomponents

      if Present (Subcomponents (E)) then
         T := First_Node (Subcomponents (E));
         while Present (T) loop
            Visit (Corresponding_Instance (T));

            T := Next_Node (T);
         end loop;
      end if;
   end Visit_Component_Instance;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      --  Registration of the generator

      Register_Backend ("alloy", Generate'Access, Alloy_Backend);
   end Init;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      use GNAT.Command_Line;

      Instance_Root : Node_Id;
   begin
      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);
      if No (Instance_Root) then
         raise Program_Error;
      end if;

      Initialize_Option_Scan;
      loop
         case Getopt ("* ") is

            when ASCII.NUL =>
               exit;

            when others =>
               null;
         end case;
      end loop;

      --  Open a new .als file

      Create (File => FD, Name => "con_model.als");

      Put_Line (FD, "// This file has been generated by Ocarina");
      Put_Line (FD, "// DO NOT EDIT IT");
      New_Line (FD);
      Put_Line (FD, "module con_model");
      Put_Line (FD, "open alloy/common/lib_sig");
      New_Line (FD);

      --  Visit instance model

      New_Line (FD);
      Put_Line (FD, "// Mapping of the AADL instance tree");
      New_Line (FD);

      Visit_Architecture_Instance (Instance_Root);

      --  Add global contract

      New_Line (FD);
      Put_Line (FD, "// Declaration of the contract(s) "
                  & "representing the model(s)");
      New_Line (FD);
      Put_Line (FD, "one sig aadl_model extends Contract{}{");
      Put_Line (FD, ASCII.HT & "assumption=none");
      Put_Line (FD, ASCII.HT & "input=none");
      Put_Line (FD, ASCII.HT & "guarantee=none");

      --  Generate output

      declare
         Print_Subcomponents : Boolean := True;
         E : constant Node_Id := Root_System (Instance_Root);
         T : Node_Id;
      begin
         --  We consider two patterns
         --  a) system with subcomponents as system/bus/device only
         --  b) general case

         if Present (Subcomponents (E)) then
            T := First_Node (Subcomponents (E));
            while Present (T) loop
               Print_Subcomponents := Print_Subcomponents
                 and then (Get_Category_Of_Component (T) = CC_System
                             or else Get_Category_Of_Component (T) = CC_Device
                             or else Get_Category_Of_Component (T) = CC_Bus);
               T := Next_Node (T);
            end loop;
         end if;

         --  We are in case a), generate all subcomponents of root system

         if Print_Subcomponents then
            Put (Fd, ASCII.HT & "output=");
            if Present (Subcomponents (E)) then
               T := First_Node (Subcomponents (E));
               while Present (T) loop
                  declare
                     Subcomponent_Name : constant String :=
                       To_Lower
                       (Get_Name_String
                          (Normalize_Name
                             (Fully_Qualified_Instance_Name
                                (Corresponding_Instance (T)))));
                  begin
                     Put (FD, Subcomponent_Name);

                     T := Next_Node (T);
                     if Present (T) then
                        Put (FD, "+");
                     end if;
                  end;
               end loop;
               New_Line (FD);

            else
               Put_Line (FD, "none");
            end if;
         else
            --  We are in case b), generate only root system

            Put_Line (FD, ASCII.HT & "output="
                        & To_Lower (Get_Name_String (Root_System_Name)));
         end if;
      end;

      Put_Line (FD, "}");

      --  Close file descriptor

      Close (FD);
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

end Ocarina.Backends.Alloy;
