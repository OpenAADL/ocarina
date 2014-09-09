------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . A L L O Y                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2014 ESA & ISAE.                       --
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

with Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Instances;           use Ocarina.Instances;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with GNAT.Command_Line;

with Ada.Text_IO;

package body Ocarina.Backends.Alloy is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   use Ocarina.Namet;
   use Ada.Text_IO;
   use Ocarina.ME_AADL;
   use AIN;

   procedure Visit (E : Node_Id);

   procedure Visit_Architecture_Instance (E : Node_Id);

   procedure Visit_Component_Instance (E : Node_Id);

   FD            : File_Type;

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
      use Ocarina.ME_AADL.AADL_Instances.Entities;

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
         CC_Virtual_Bus       => Get_String_Name ("virtual bus"),
         CC_Virtual_Processor => Get_String_Name ("virtual processor"));

      Category : constant Component_Category := Get_Category_Of_Component (E);

      T : Node_Id;
   begin
      --  Create Alloy component

      --  Rule #1: the name of this component is deduced from the name
      --  of the corresponding instance name

      if Present (Parent_Subcomponent (E)) then
         Put_Line (FD, "one sig " &
                     Get_Name_String
                     (Display_Name (Identifier (Parent_Subcomponent (E))))
                     & " extends Component{}{");
      else
         Put_Line (FD, "one sig " &
                  Get_Name_String (Display_Name (Identifier (E)))
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
                 String'(Get_Name_String
                           (Display_Name
                              (Identifier (T))));
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
            Put (FD, Get_Name_String (Display_Name (Identifier (T))));
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

      Create (File => FD, Name => "tran_model.als");

      Put_Line (FD, "module tran_model");
      Put_Line (FD, "open lib_sig");

      --  Visit instance model

      Visit_Architecture_Instance (Instance_Root);

      --  Close file descriptors

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
