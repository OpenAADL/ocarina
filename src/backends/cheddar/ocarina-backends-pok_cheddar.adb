------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . P O K _ C H E D D A R          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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

with Ocarina.Instances;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Expander;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Tree.Generator;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Instances.Queries;

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.POK_Cheddar is

   use Ocarina.Instances;

   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Utils;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;

   use Ocarina.Instances.Queries;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   procedure Visit (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);

   Task_Id                   : Unsigned_Long_Long := 0;
   Current_System            : Node_Id;
   Current_Virtual_Processor : Node_Id;
   Current_Process           : Node_Id;
   Current_Processor         : Node_Id;

   ----------------
   -- Map_Thread --
   ----------------

   procedure Map_Thread (E : Node_Id) is
      N   : Node_Id;
      L   : Node_Id;
      R   : Node_Id;
      Tmp : Name_Id;
   begin
      --  Delete the space introduced by Ada.
      Set_Str_To_Name_Buffer (Unsigned_Long_Long'Image (Task_Id));
      Set_Str_To_Name_Buffer (Name_Buffer (2 .. Name_Len));
      Tmp := Name_Find;

      N := Make_XML_Node ("thread");

      --  Set the pokname attribute.
      Set_Str_To_Name_Buffer ("pokname");
      L := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("task");
      Get_Name_String_And_Append (Tmp);

      R := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (L, R), XTN.Items (N));

      --  Set the aadlprocessor attribute.
      if Current_Processor /= No_Node then
         Set_Str_To_Name_Buffer ("aadlprocessor");
         L := Make_Defining_Identifier (Name_Find);

         R :=
           Make_Defining_Identifier
             (Display_Name
                (Identifier (Parent_Subcomponent (Current_Processor))));

         Append_Node_To_List (Make_Assignement (L, R), XTN.Items (N));
      end if;

      --  Set the aadlprocess attribute.
      if Current_Processor /= No_Node then
         Set_Str_To_Name_Buffer ("aadlprocess");
         L := Make_Defining_Identifier (Name_Find);

         R :=
           Make_Defining_Identifier
             (Display_Name
                (Identifier (Parent_Subcomponent (Current_Process))));

         Append_Node_To_List (Make_Assignement (L, R), XTN.Items (N));
      end if;

      --  Set the aadlvirtualprocessor attribute.
      if Current_Processor /= No_Node then
         Set_Str_To_Name_Buffer ("aadlvirtualprocessor");
         L := Make_Defining_Identifier (Name_Find);

         R :=
           Make_Defining_Identifier
             (Display_Name
                (Identifier
                   (Parent_Subcomponent (Current_Virtual_Processor))));

         Append_Node_To_List (Make_Assignement (L, R), XTN.Items (N));
      end if;

      --  Set the cheddarname attribute.
      if Current_Processor /= No_Node then
         Set_Str_To_Name_Buffer ("cheddarname");
         L := Make_Defining_Identifier (Name_Find);

         Get_Name_String
           (Display_Name
              (Identifier (Parent_Subcomponent (Current_Virtual_Processor))));

         Add_Str_To_Name_Buffer (".");

         Get_Name_String_And_Append
           (Display_Name (Identifier (Parent_Subcomponent (E))));

         R := Make_Defining_Identifier (Name_Find);

         Append_Node_To_List (Make_Assignement (L, R), XTN.Items (N));
      end if;

      Append_Node_To_List (N, XTN.Subitems (Current_XML_Node));

      Task_Id := Task_Id + 1;
   end Map_Thread;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (XTN.K_HI_Node);
   begin
      pragma Assert
        (AINU.Is_Process (E)
         or else AINU.Is_System (E)
         or else AINU.Is_Processor (E));

      if AINU.Is_System (E) then
         Set_Str_To_Name_Buffer ("general");
      else
         Get_Name_String
           (To_XML_Name
              (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_pok_cheddar_mapping");
      end if;

      XTN.Set_Name (N, Name_Find);

      XTN.Set_Units (N, XTU.New_List (XTN.K_List_Id));

      --  Append the partition N to the node list

      XTU.Append_Node_To_List (N, XTN.HI_Nodes (Current_Entity));
      XTN.Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------

   function Map_HI_Unit (E : Node_Id) return Node_Id is
      U    : Node_Id;
      N    : Node_Id;
      P    : Node_Id;
      Root : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process (E)
         or else AINU.Is_Processor (E));

      U := XTU.New_Node (XTN.K_HI_Unit, AIN.Identifier (E));

      --  Packages that are common to all nodes
      Get_Name_String
        (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Add_Str_To_Name_Buffer ("_pok-mapping");
      N := XTU.Make_Defining_Identifier (Name_Find);
      P := XTU.Make_XML_File (N);
      XTN.Set_Distributed_Application_Unit (P, U);
      XTN.Set_XML_File (U, P);

      Root := Make_XML_Node ("Mapping_Rules");

      XTN.Set_Root_Node (P, Root);

      Current_XML_Node := Root;

      XTU.Append_Node_To_List (U, XTN.Units (Current_Entity));
      XTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
   begin

      Instance_Root := Instantiate_Model (AADL_Root);

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);
      --  Abort if the construction of the C tree failed

      if No (XML_Root) then
         Display_Error ("POK_Cheddar generation failed", Fatal => True);
      end if;

      --  At this point, we have a valid tree, we can begin the XML
      --  file generation.

      --  Enter the output directory

      Enter_Directory (Generated_Sources_Directory);

      if not Remove_Generated_Sources then
         --  Create the source files

         XML_Tree.Generator.Generate (XML_Root);

      end if;

      --  Leave the output directory
      Leave_Directory;
   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend ("POK_Cheddar", Generate'Access, Statistics);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      XML_Root := XTU.New_Node (XTN.K_HI_Distributed_Application);
      Set_Str_To_Name_Buffer ("generated-code");
      XTN.Set_Name (XML_Root, Name_Find);
      XTN.Set_Units (XML_Root, XTU.New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (XML_Root, XTU.New_List (XTN.K_List_Id));

      XTU.Push_Entity (XML_Root);

      Visit (Root_System (E));

      XTU.Pop_Entity;

   end Visit_Architecture_Instance;

   ------------------
   -- Get_XML_Root --
   ------------------

   function Get_XML_Root return Node_Id is
   begin
      return XML_Root;
   end Get_XML_Root;

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

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Thread =>
            Visit_Thread_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      Current_Process := E;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Current_Process := No_Node;
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_Thread_Instance --
   ---------------------------

   procedure Visit_Thread_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      Map_Thread (E);

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Thread_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      Current_System := E;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if AINU.Is_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
            end if;
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S : Node_Id;
      U : Node_Id;
      P : Node_Id;
   begin
      Current_Processor := E;

      P := Map_HI_Node (E);
      Push_Entity (P);

      U := Map_HI_Unit (E);
      Push_Entity (U);

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Current_Processor := No_Node;

      Pop_Entity;
      Pop_Entity;
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S       : Node_Id;
      Process : Node_Id;
   begin
      Current_Virtual_Processor := E;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      if not AINU.Is_Empty (Subcomponents (Current_System)) then
         S := First_Node (Subcomponents (Current_System));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            if AINU.Is_Process (Corresponding_Instance (S)) then
               Process := Corresponding_Instance (S);

               if Get_Bound_Processor (Process) = E then
                  --  Visit the partition, we need the init task
                  Task_Id := Task_Id + 1;
                  Visit (Process);

                  if Is_Defined_Property (E, "arinc653::hm_errors")
                    or else Is_Defined_Property (E, "pok::recovery_errors")
                  then
                     --  If we handle errors, we need to consider the error
                     --  recovery task.
                     Task_Id := Task_Id + 1;
                  end if;
               end if;
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      Current_Virtual_Processor := No_Node;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.POK_Cheddar;
