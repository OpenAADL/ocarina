------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . S U B P R O G R A M S          --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.C_Common.Mapping;

with Ocarina.Instances.Queries;

with Ocarina.Instances;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Generator;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.Subprograms is
   use Ocarina.Instances;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Tree.Nodes;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;

   Source_File : Node_Id;
   Header_File : Node_Id;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   package Subprograms_Generation is
      procedure Visit (E : Node_Id);
   end Subprograms_Generation;

   package body Subprograms_Generation is
      use Ocarina.ME_AADL;
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.Backends.C_Common.Mapping;

      use Ocarina.Instances.Queries;

      package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
      package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
      package CTN renames Ocarina.Backends.C_Tree.Nodes;

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      -------------------------
      -- Map_Subprogram_Spec --
      -------------------------

      function Map_Subprogram_Spec (S : Node_Id) return Node_Id is
         Profile : constant List_Id := CTU.New_List (CTN.K_Parameter_Profile);
         Param   : Node_Id;
         Mode    : Mode_Id;
         F       : Node_Id;
         N       : Node_Id;
         D       : Node_Id;
         Field   : Node_Id;
      begin
         pragma Assert (AINU.Is_Subprogram (S));

         --  We build the parameter profile of the subprogram instance by
         --  adding:

         --  First, the parameter features mapping

         if not AINU.Is_Empty (Features (S)) then
            F := AIN.First_Node (Features (S));

            while Present (F) loop
               if Kind (F) = K_Parameter_Instance then
                  if Is_In (F) and then Is_Out (F) then
                     Mode := Mode_Inout;
                  elsif Is_Out (F) then
                     Mode := Mode_Out;
                  elsif Is_In (F) then
                     Mode := Mode_In;
                  else
                     Display_Located_Error
                       (AIN.Loc (F),
                        "Unspecified parameter mode",
                        Fatal => True);
                  end if;

                  D := Corresponding_Instance (F);

                  if Mode = Mode_In then
                     Param :=
                       CTU.Make_Parameter_Specification
                         (Defining_Identifier => Map_C_Defining_Identifier (F),
                          Parameter_Type => Map_C_Defining_Identifier (D));
                  else
                     Param :=
                       CTU.Make_Parameter_Specification
                         (Defining_Identifier => Map_C_Defining_Identifier (F),
                          Parameter_Type      =>
                            CTU.Make_Pointer_Type
                              (Map_C_Defining_Identifier (D)));
                  end if;
                  CTU.Append_Node_To_List (Param, Profile);
               end if;

               F := AIN.Next_Node (F);
            end loop;
         end if;

         --  Second, the data access mapping. The data accesses are not
         --  mapped in the case of pure call sequence subprogram because
         --  they are used only to close the access chain.

         if Get_Subprogram_Kind (S) /= Subprogram_Pure_Call_Sequence then

            if not AINU.Is_Empty (Features (S)) then
               F := AIN.First_Node (Features (S));

               while Present (F) loop
                  if Kind (F) = K_Subcomponent_Access_Instance then
                     case Get_Required_Data_Access (Corresponding_Instance (F))
                     is
                        when Access_Read_Only =>
                           Mode := Mode_In;
                        when Access_Write_Only =>
                           Mode := Mode_Out;
                        when Access_Read_Write =>
                           Mode := Mode_Inout;
                        when Access_None =>
                           --  By default, we allow read/write access
                           Mode := Mode_Inout;
                        when others =>
                           Display_Located_Error
                             (AIN.Loc (F),
                              "Unsupported required access",
                              Fatal => True);
                     end case;

                     D := Corresponding_Instance (F);

                     case Get_Data_Representation (D) is
                        when Data_Integer     |
                          Data_Boolean        |
                          Data_Float          |
                          Data_Fixed          |
                          Data_Struct         |
                          Data_String         |
                          Data_Wide_String    |
                          Data_Character      |
                          Data_Wide_Character |
                          Data_Array          =>
                           --  If the data component is a simple data
                           --  component (not a structure), we simply add a
                           --  parameter with the computed mode and with a
                           --  type mapped from the data component.

                           if Mode = Mode_In then
                              Param :=
                                CTU.Make_Parameter_Specification
                                  (Defining_Identifier =>
                                     Map_C_Defining_Identifier (F),
                                   Parameter_Type =>
                                     Map_C_Data_Type_Designator (D));
                           else
                              Param :=
                                CTU.Make_Parameter_Specification
                                  (Defining_Identifier =>
                                     Map_C_Defining_Identifier (F),
                                   Parameter_Type =>
                                     CTU.Make_Pointer_Type
                                       (Map_C_Data_Type_Designator (D)));
                           end if;

                           CTU.Append_Node_To_List (Param, Profile);

                        when Data_With_Accessors =>
                           --  If the data component is a complex data
                           --  component (which has subcomponents), we add a
                           --  parameter with the computed mode and with a
                           --  type mapped from each subcomponent type.

                           Field := AIN.First_Node (Subcomponents (D));

                           while Present (Field) loop
                              if Mode = Mode_In then
                                 Param :=
                                   CTU.Make_Parameter_Specification
                                     (Defining_Identifier =>
                                        Map_C_Defining_Identifier (Field),
                                      Parameter_Type =>
                                        Map_C_Data_Type_Designator
                                          (Corresponding_Instance (Field)));
                              else
                                 Param :=
                                   CTU.Make_Parameter_Specification
                                     (Defining_Identifier =>
                                        Map_C_Defining_Identifier (Field),
                                      Parameter_Type =>
                                        Make_Pointer_Type
                                          (Map_C_Data_Type_Designator
                                             (Corresponding_Instance
                                                (Field))));
                              end if;
                              CTU.Append_Node_To_List (Param, Profile);

                              Field := AIN.Next_Node (Field);
                           end loop;

                        when others =>
                           Display_Located_Error
                             (AIN.Loc (F),
                              "Unsupported data type",
                              Fatal => True);
                     end case;
                  end if;

                  F := AIN.Next_Node (F);
               end loop;
            end if;
         end if;

         N :=
           CTU.Make_Function_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (Get_Source_Name (S)),
              Parameters  => Profile,
              Return_Type => New_Node (CTN.K_Void));

         return N;
      end Map_Subprogram_Spec;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case AAN.Kind (E) is
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
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Device =>
               Visit_Device_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : Node_Id;
      begin
         Implementation := Get_Classifier_Property (E, "implemented_as");

         if Implementation /= No_Node then
            Visit_Subcomponents_Of (Implementation);
         end if;
      end Visit_Device_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
      begin
         Visit_Subcomponents_Of (E);
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         Spec     : Node_Id;
         Impl     : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         if Get_Source_Name (E) = No_Name then
            return;
         end if;

         --  We only generate users programs.

         Spec := Map_Subprogram_Spec (E);
         Append_Node_To_List (Spec, CTN.Declarations (Header_File));

         Impl := Make_Function_Implementation (Spec, No_List, No_List);
         Append_Node_To_List (Impl, CTN.Declarations (Source_File));

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := AIN.First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := AIN.First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := AIN.Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := AIN.Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
      begin
         Visit_Subcomponents_Of (E);
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Visit all the call sequences of the thread

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := AIN.First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := AIN.First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := AIN.Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := AIN.Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Subprograms_Generation;

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

      if No (AADL_Root) then
         Display_Error ("Code generation failed", Fatal => True);
      end if;

      --  Enter the output directory

      Enter_Directory (Generated_Sources_Directory);

      if not Remove_Generated_Sources then
         --  Create the source files

         C_Tree.Generator.Generate (C_Root);
      end if;

      --  Leave the output directory
      Leave_Directory;
   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend ("Subprograms", Generate'Access, Subprograms_Generator);
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
      D : constant Node_Id := CTU.New_Node (CTN.K_HI_Distributed_Application);
      N               : Name_Id;
      File_Identifier : Node_Id;
      Unit            : Node_Id;
      Clause          : Name_Id;
      Unit_Identifier : Node_Id;
      Ifdef_Clause    : Node_Id;
      Header_Name     : Name_Id;
      Header_Node     : Node_Id;
   begin
      CTN.Set_Units (D, CTU.New_List (CTN.K_List_Id));
      CTN.Set_HI_Nodes (D, CTU.New_List (CTN.K_List_Id));
      N := Get_String_Name ("generated-code");
      CTN.Set_Name (D, N);

      Set_Str_To_Name_Buffer ("subprograms-unit");
      Unit_Identifier := Make_Defining_Identifier (Name_Find);

      --  Create a "false" unit to store files.
      Unit := New_Node (CTN.K_HI_Unit, Unit_Identifier);
      Append_Node_To_List (Unit, Units (D));

      Set_Str_To_Name_Buffer ("generated-subprograms");
      File_Identifier := Make_Defining_Identifier (Name_Find);

      --  Create the Source_File node.

      Source_File := Make_Source_File (File_Identifier);
      Set_Distributed_Application_Unit (Source_File, D);
      Append_Node_To_List (Source_File, HI_Nodes (D));

      --  Create the Header_File node.

      Header_File := Make_Header_File (File_Identifier);
      Set_Distributed_Application_Unit (Header_File, D);
      Append_Node_To_List (Header_File, HI_Nodes (D));

      --  Generate #ifdef __POK_C__ #include <gtypes.h>.

      Set_Str_To_Name_Buffer ("gtypes");
      Header_Name := Name_Find;

      Header_Node :=
        Make_Include_Clause (Make_Defining_Identifier (Header_Name), False);

      Set_Str_To_Name_Buffer ("__POK_C__");
      Clause := Name_Find;

      Ifdef_Clause :=
        Make_Ifdef_Clause
          (Make_Defining_Identifier (Clause, C_Conversion => False),
           False,
           Make_List_Id (Header_Node),
           No_List);

      Append_Node_To_List (Ifdef_Clause, CTN.Declarations (Source_File));

      Append_Node_To_List
        (Copy_Node (Ifdef_Clause),
         CTN.Declarations (Header_File));

      --  Generate #ifdef __PO_HI_C__ #include <gtypes.h>.

      Set_Str_To_Name_Buffer ("types");
      Header_Name := Name_Find;

      Header_Node :=
        Make_Include_Clause (Make_Defining_Identifier (Header_Name), False);

      Set_Str_To_Name_Buffer ("__PO_HI_C__");
      Clause := Name_Find;

      Ifdef_Clause :=
        Make_Ifdef_Clause
          (Make_Defining_Identifier (Clause, C_Conversion => False),
           False,
           Make_List_Id (Header_Node),
           No_List);

      Append_Node_To_List (Ifdef_Clause, CTN.Declarations (Source_File));

      Append_Node_To_List
        (Copy_Node (Ifdef_Clause),
         CTN.Declarations (Header_File));

      --  Generate #ifdef is now finished.

      C_Root := D;

      Subprograms_Generation.Visit (E);
   end Visit_Architecture_Instance;

end Ocarina.Backends.Subprograms;
