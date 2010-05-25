------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . A S N 1 . D E P L O Y M E N T      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010, GET-Telecom Paris.                   --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Namet; use Namet;
with Utils; use Utils;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.ASN1_Tree.Nutils;
with Ocarina.Backends.ASN1_Tree.Nodes;

package body Ocarina.Backends.ASN1.Deployment is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.ASN1_Tree.Nutils;

   package ASN1N renames Ocarina.Backends.ASN1_Tree.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Device_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);
   procedure Visit_Subprogram_Instance (E : Node_Id);

   Thread_Enumeration : List_Id;
   Thread_Id : Unsigned_Long_Long := 0;
   Port_Enumeration : List_Id;
   Port_Id : Unsigned_Long_Long := 0;
   Module_Node : Node_Id;

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
      ASN1_Root := Make_ASN1_File
         (Make_Defining_Identifier
            (Get_String_Name ("deployment")));
      ASN1N.Set_Name
         (ASN1N.Module_Node (ASN1_Root),
         Get_String_Name ("POHIC-DEPLOYMENT"));
      Module_Node := ASN1N.Module_Node (ASN1_Root);

      Thread_Enumeration := New_List (ASN1N.K_Enumerated_Value_List);
      Port_Enumeration := New_List (ASN1N.K_Enumerated_Value_List);

      Visit (Root_System (E));

      if Length (Thread_Enumeration) > 0 then
         Append_Node_To_List
         (Make_Type_Definition
            (Get_String_Name ("THREADS"),
            Make_Enumerated (Thread_Enumeration)),
         ASN1N.Definitions (Module_Node));
      end if;

      if Length (Thread_Enumeration) > 0 then
         Append_Node_To_List
         (Make_Type_Definition
            (Get_String_Name ("PORTS"),
            Make_Enumerated (Port_Enumeration)),
         ASN1N.Definitions (Module_Node));
      end if;

   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category
        := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Thread =>
            Visit_Thread_Instance (E);

         when CC_Device =>
            Visit_Device_Instance (E);

         when CC_Subprogram =>
            Visit_Subprogram_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      S               : Node_Id;
   begin
      if not AAU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      --  Visit all the subcomponents of the system

      if not AAU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System_Instance;

   ---------------------------
   -- Visit_Thread_Instance --
   ---------------------------

   procedure Visit_Thread_Instance (E : Node_Id) is
      S        : Node_Id;
      F        : Node_Id;
      Call_Seq : Node_Id;
      Spg_Call : Node_Id;
      Thread_Name : Name_Id;
      Parent_Name : Name_Id;
      Port_Name : Name_Id;
   begin

      Set_Str_To_Name_Buffer ("thread-");
      Parent_Name := Display_Name
         (Identifier
          (Parent_Subcomponent
           (Parent_Component
            (Parent_Subcomponent (E)))));
      Get_Name_String_And_Append (Parent_Name);
      Add_Str_To_Name_Buffer ("-");
      Get_Name_String_And_Append
         (Display_Name
           (Identifier
            (Parent_Subcomponent (E))));
      Thread_Name := Name_Find;

      Thread_Name := To_Upper (Thread_Name);

      Append_Node_To_List
         (Make_Enumerated_Value
            (Thread_Name, Thread_Id),
         Thread_Enumeration);

      Thread_Id := Thread_Id + 1;

      if not AAU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      if not AAU.Is_Empty (Calls (E)) then
         Call_Seq := First_Node (Calls (E));

         while Present (Call_Seq) loop
            --  For each call sequence visit all the called
            --  subprograms.

            if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
               Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

               while Present (Spg_Call) loop
                  Visit (Corresponding_Instance (Spg_Call));

                  Spg_Call := Next_Node (Spg_Call);
               end loop;
            end if;

            Call_Seq := Next_Node (Call_Seq);
         end loop;
      end if;

      if Has_Ports (E) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               Set_Str_To_Name_Buffer ("port-");
               Get_Name_String_And_Append (Thread_Name);
               Add_Str_To_Name_Buffer ("-");
               Get_Name_String_And_Append
                  (Display_Name (Identifier (F)));
               Port_Name := Name_Find;
               Port_Name := To_Upper (Port_Name);

               Append_Node_To_List
               (Make_Enumerated_Value
                (Port_Name, Port_Id),
                Port_Enumeration);

               Port_Id := Port_Id + 1;

            end if;
            F := Next_Node (F);
         end loop;
      end if;
   end Visit_Thread_Instance;

   -------------------------------
   -- Visit_Subprogram_Instance --
   -------------------------------

   procedure Visit_Subprogram_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

   procedure Visit_Device_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Device_Instance;

end Ocarina.Backends.ASN1.Deployment;
