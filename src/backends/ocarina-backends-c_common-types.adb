------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . C _ C O M M O N . T Y P E S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2017 ESA & ISAE.      --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.POK_C;
with Ocarina.Backends.PO_HI_C;
with Ocarina.Backends.C_Common;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.POK_C.Runtime;

package body Ocarina.Backends.C_Common.Types is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.Backends.POK_C;
   use Ocarina.Backends.POK_C.Runtime;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package CV renames Ocarina.Backends.C_Values;
   package PKR renames Ocarina.Backends.POK_C.Runtime;
   package PHR renames Ocarina.Backends.PO_HI_C.Runtime;

   C_Root : Node_Id;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
        (E            : Node_Id;
         Real_Process : Boolean := True);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Abstract_Instance (E : Node_Id);
      procedure Visit_Bus_Instance (E : Node_Id);
      procedure Visit_Virtual_Bus_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      function Feature_Spg_Spec (E : Node_Id) return Node_Id;

      ----------------------
      -- Feature_Spg_Spec --
      ----------------------

      function Feature_Spg_Spec (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Spg        : Node_Id;
         Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
      begin
         pragma Assert
           (Kind (E) = K_Subprogram_Spec_Instance
            or else Kind (E) = K_Subcomponent_Access_Instance);

         Spg := Corresponding_Instance (E);

         pragma Assert (AINU.Is_Subprogram (Spg));

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Value)),
               Parameter_Type      =>
                 Make_Pointer_Type
                   (Map_C_Defining_Identifier (Parent_Component (E)))),
            Parameters);

         N :=
           Make_Function_Specification
             (Defining_Identifier => Map_C_Feature_Subprogram (E),
              Parameters          => Parameters,
              Return_Type         => New_Node (CTN.K_Void));
         return N;
      end Feature_Spg_Spec;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id; C : Node_Id := No_Node) is
      begin
         if C /= No_Node then
            C_Root := C;
         end if;

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
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when CC_Device =>
               Visit_Device_Instance (E);

            when CC_Abstract =>
               Visit_Abstract_Instance (E);

            when CC_Bus =>
               Visit_Bus_Instance (E);

            when CC_Virtual_Bus =>
               Visit_Virtual_Bus_Instance (E);

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ------------------------
      -- Visit_Bus_Instance --
      ------------------------

      procedure Visit_Bus_Instance (E : Node_Id) is
         SC : Node_Id;
      begin
         if not AINU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC

               Visit (Corresponding_Instance (SC));

               SC := Next_Node (SC);
            end loop;
         end if;
      end Visit_Bus_Instance;

      -----------------------------
      -- Visit_Abstract_Instance --
      -----------------------------

      procedure Visit_Abstract_Instance (E : Node_Id) is
         SC       : Node_Id;
         Instance : Node_Id;
      begin
         if not AINU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC
               Instance := Corresponding_Instance (SC);
               Visit (Instance);

               SC := Next_Node (SC);
            end loop;
         end if;
      end Visit_Abstract_Instance;

      --------------------------------
      -- Visit_Virtual_Bus_Instance --
      --------------------------------

      procedure Visit_Virtual_Bus_Instance (E : Node_Id) is
         SC : Node_Id;
      begin
         if Get_Implementation (E) /= No_Node then
            Visit (Get_Implementation (E));
         end if;

         if not AINU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC

               Visit (Corresponding_Instance (SC));

               SC := Next_Node (SC);
            end loop;
         end if;
      end Visit_Virtual_Bus_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         U              : Node_Id;
         P              : Node_Id;
         Implementation : Node_Id;
         S              : Node_Id;
      begin
         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node (Backend_Node (Identifier (E))));

            P := CTN.Entity (U);

            Push_Entity (P);
            Push_Entity (U);
         end if;

         Implementation := Get_Implementation (E);

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop

                  if Get_Current_Backend_Kind = PolyORB_Kernel_C then
                     if Get_Category_Of_Component (S) = CC_Process then
                        Visit_Process_Instance
                          (Corresponding_Instance (S),
                           False);
                     end if;
                  else
                     Visit (Corresponding_Instance (S));
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;

         end if;

         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            Pop_Entity; -- U
            Pop_Entity; -- P
         end if;
      end Visit_Device_Instance;

      ------------------------------
      -- Visit_Processor_Instance --
      ------------------------------

      procedure Visit_Processor_Instance (E : Node_Id) is
         S : Node_Id;
         A : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         A := CTN.Deployment_Node (Backend_Node (Identifier (E)));
         Push_Entity (C_Root);
         Push_Entity (A);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity;
         Pop_Entity;
      end Visit_Processor_Instance;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
         Processes : List_Id;
         S         : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            S         := AIN.First_Node (Processes);
            while Present (S) loop
               Visit (AIN.Item (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_Virtual_Processor_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         Data_Representation   : Supported_Data_Representation;
         N                     : Node_Id;
         M                     : Node_Id;
         S                     : Node_Id;
         R                     : Node_Id;
         Data_Size             : Size_Type;
         Data_Array_Size       : constant ULL_Array := Get_Dimension (E);
         Number_Representation : constant Supported_Number_Representation :=
           Get_Number_Representation (E);
         Is_Signed : constant Boolean := Number_Representation = Signed;

         Type_Uint8       : Node_Id;
         Type_Int8        : Node_Id;
         Type_Uint16      : Node_Id;
         Type_Int16       : Node_Id;
         Type_Uint32      : Node_Id;
         Type_Int32       : Node_Id;
         Type_Uint64      : Node_Id;
         Type_Int64       : Node_Id;
         Type_Source_Name : Name_Id;

         Actual_Data_Size : Unsigned_Long_Long;
         Struct_Members   : constant List_Id :=
           New_List (CTN.K_Enumeration_Literals);
         Protected_Struct_Members : constant List_Id :=
           New_List (CTN.K_Enumeration_Literals);
      begin
         if No (Get_Handling (E, By_Name, H_C_Type_Spec)) then
            if Get_Current_Backend_Kind = PolyORB_HI_C then
               Add_Include (PHR.RH (RH_PO_HI_Types));
               Type_Uint8  := PHR.RE (RE_Uint8_T);
               Type_Int8   := PHR.RE (RE_Int8_T);
               Type_Uint16 := PHR.RE (RE_Uint16_T);
               Type_Int16  := PHR.RE (RE_Int16_T);
               Type_Uint32 := PHR.RE (RE_Uint32_T);
               Type_Int32  := PHR.RE (RE_Int32_T);
               Type_Uint64 := PHR.RE (RE_Uint64_T);
               Type_Int64  := PHR.RE (RE_Int64_T);
            elsif Get_Current_Backend_Kind = PolyORB_Kernel_C then
               if Use_ARINC653_API = False then
                  Add_Include (PKR.RH (RH_Types));
               end if;
               Type_Uint8  := PKR.RE (RE_Uint8_T);
               Type_Int8   := PKR.RE (RE_Int8_T);
               Type_Uint16 := PKR.RE (RE_Uint16_T);
               Type_Int16  := PKR.RE (RE_Int16_T);
               Type_Uint32 := PKR.RE (RE_Uint32_T);
               Type_Int32  := PKR.RE (RE_Int32_T);
               Type_Uint64 := PKR.RE (RE_Uint64_T);
               Type_Int64  := PKR.RE (RE_Int64_T);
            end if;

            Data_Representation := Get_Data_Representation (E);
            Data_Size           := Get_Data_Size (E);
            Actual_Data_Size    := To_Bytes (Data_Size);

            Type_Source_Name := Get_Type_Source_Name (E);

            --
            --  If the user specifies Type_Source_Name property
            --  on the data type, then, the generator takes
            --  it over everything else. This is a way to
            --  override some assumptions from the code generator.
            --

            if Type_Source_Name /= No_Name then
               Data_Representation := Data_None;
            end if;

            case Data_Representation is
               when Data_Boolean =>
                  N :=
                    Make_Full_Type_Declaration
                      (Defining_Identifier => Map_C_Defining_Identifier (E),
                       Type_Definition     => PHR.RE (RE_Bool_T));
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

               when Data_Float =>
                  if Data_Size.S = 0 then
                     --  If no size info is given, we default to float
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     =>
                            Make_Defining_Identifier (TN (T_Float)));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 4 then
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => PHR.RE (RE_Float32_T));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 8 then
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => PHR.RE (RE_Float64_T));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  else
                     Display_Located_Error
                       (Loc (E),
                        "Unsupported data size: " & Actual_Data_Size'Img,
                        Fatal => True);
                  end if;

               when Data_Integer =>
                  if Data_Size.S = 0 then
                     --  If no size info is given, we default to int

                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     =>
                            Make_Defining_Identifier (TN (T_Int)));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 1 and then Is_Signed then
                     R := Type_Int8;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 1 and then not Is_Signed then
                     R := Type_Uint8;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 2 and then Is_Signed then
                     R := Type_Int16;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 2 and then not Is_Signed then
                     R := Type_Uint16;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 4 and then Is_Signed then
                     R := Type_Int32;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 4 and then not Is_Signed then
                     R := Type_Uint32;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 8 and then Is_Signed then
                     R := Type_Int64;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  elsif Actual_Data_Size = 8 and then not Is_Signed then
                     R := Type_Uint64;
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     => R);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  else
                     Display_Located_Error
                       (Loc (E),
                        "Unsupported data size" & Actual_Data_Size'Img,
                        Fatal => True);
                  end if;

               when Data_Struct | Data_With_Accessors =>
                  declare
                     C : Node_Id := No_Node;
                  begin
                     if No (Subcomponents (E)) then
                        C := No_Node;
                     else
                        C := First_Node (Subcomponents (E));
                     end if;
                     --  Build the component list

                     while Present (C) loop
                        --  Generate the C type corresponding to the
                        --  subcomponents.

                        if AINU.Is_Data (Corresponding_Instance (C)) then
                           Visit (Corresponding_Instance (C));

                           --  Make the record or private type component

                           N :=
                             Make_Member_Declaration
                               (Defining_Identifier =>
                                  Map_C_Defining_Identifier (C),
                                Used_Type =>
                                  Map_C_Data_Type_Designator
                                    (Corresponding_Instance (C)));
                           Append_Node_To_List (N, Struct_Members);
                        end if;

                        C := Next_Node (C);
                     end loop;

                     if Data_Representation = Data_Struct then
                        --  Record type

                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_C_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Struct_Aggregate
                                 (Members => Struct_Members));
                        Append_Node_To_List
                          (N,
                           CTN.Declarations (Current_File));

                     else
                        --  Protected type
                        Append_Node_To_List
                          (Make_Member_Declaration
                             (Used_Type           => RE (RE_Protected_T),
                              Defining_Identifier =>
                                Make_Defining_Identifier
                                  (MN (M_Protected_Id))),
                           Protected_Struct_Members);

                        if not Is_Empty (Struct_Members) then
                           S := CTN.First_Node (Struct_Members);
                           while Present (S) loop
                              Append_Node_To_List
                                (S,
                                 Protected_Struct_Members);
                              S := CTN.Next_Node (S);
                           end loop;
                        end if;

                        N :=
                          Make_Full_Type_Declaration
                            (Defining_Identifier =>
                               Map_C_Defining_Identifier (E),
                             Type_Definition =>
                               Make_Struct_Aggregate
                                 (Members => Protected_Struct_Members));

                        Append_Node_To_List
                          (N,
                           CTN.Declarations (Current_File));

                        S := First_Node (Features (E));

                        while Present (S) loop
                           --  We are sure that S is of kind
                           --  K_Subprogram_Spec_Instance. Otherwise,
                           --  an error whould be raised when trying
                           --  to find the data type.

                           --  Build a subprogram spec and append it
                           --  to the visible part of the protected
                           --  type.

                           M := Feature_Spg_Spec (S);
                           Bind_AADL_To_Feature_Subprogram (Identifier (S), M);
                           Append_Node_To_List
                             (M,
                              CTN.Declarations (Current_File));

                           S := Next_Node (S);
                        end loop;
                     end if;
                  end;

               when Data_Enum =>
                  declare
                     Enumerators  : constant Name_Array := Get_Enumerators (E);
                     Enum_Members : constant List_Id    :=
                       New_List (CTN.K_Enumeration_Literals);
                  begin
                     for J in Enumerators'Range loop
                        Append_Node_To_List
                          (Make_Defining_Identifier
                             (Map_C_Enum_Name (E, Enumerators (J))),
                           Enum_Members);
                     end loop;

                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     =>
                            Make_Enum_Aggregate (Enum_Members));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));
                  end;

               when Data_Array =>
                  Visit_Data_Instance
                    (ATN.Entity (ATN.First_Node (Get_Base_Type (E))));

                  N :=
                    Make_Full_Type_Declaration
                      (Defining_Identifier =>
                         Make_Array_Declaration
                           (Defining_Identifier =>
                              Map_C_Defining_Identifier (E),
                            Array_Size =>
                              Make_Literal
                                (CV.New_Int_Value
                                   (Data_Array_Size (1),
                                    0,
                                    10))),
                       Type_Definition =>
                         Map_C_Defining_Identifier
                           (ATN.Entity (ATN.First_Node (Get_Base_Type (E)))));
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

               when Data_Bounded_Array =>
                  Visit_Data_Instance
                    (ATN.Entity (ATN.First_Node (Get_Base_Type (E))));

                  --  In the case of a bounded array, we generate a C
                  --  struct whose member are
                  --
                  --  1) an anonymous array whose size is the upper
                  --     bound of the array, from "Dimension" property

                  N :=
                    Make_Member_Declaration
                      (Defining_Identifier =>
                         Make_Array_Declaration
                           (Defining_Identifier =>
                              Make_Defining_Identifier (PN (P_Data)),
                            Array_Size =>
                              Make_Literal
                                (CV.New_Int_Value
                                   (Data_Array_Size (1),
                                    0,
                                    10))),
                       Used_Type =>
                         Map_C_Defining_Identifier
                           (ATN.Entity (ATN.First_Node (Get_Base_Type (E)))));
                  Append_Node_To_List (N, Struct_Members);

                  --  2) length, giving the actual size used as a
                  --     32bits integer

                  N :=
                    Make_Member_Declaration
                      (Defining_Identifier =>
                         Make_Defining_Identifier (PN (P_Length)),
                       Used_Type =>
                         PHR.RE (RE_Int32_T));
                  Append_Node_To_List (N, Struct_Members);

                  N :=
                    Make_Full_Type_Declaration
                    (Defining_Identifier =>
                       Map_C_Defining_Identifier (E),
                     Type_Definition =>
                       Make_Struct_Aggregate
                       (Members => Struct_Members));
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

               when Data_String =>
                  N :=
                    Make_Full_Type_Declaration
                      (Defining_Identifier =>
                         Make_Array_Declaration
                           (Defining_Identifier =>
                              Map_C_Defining_Identifier (E),
                            Array_Size =>
                              Make_Literal
                                (CV.New_Int_Value
                                   (Data_Array_Size (1),
                                    0,
                                    10))),
                       Type_Definition =>
                         Make_Defining_Identifier (TN (T_Char)));
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

               when Data_Character =>
                  N :=
                    Make_Full_Type_Declaration
                      (Defining_Identifier => Map_C_Defining_Identifier (E),
                       Type_Definition     =>
                         Make_Defining_Identifier (TN (T_Char)));
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

               when Data_Wide_Character |
                 Data_Fixed             |
                 Data_Wide_String       |
                 Data_Union             =>
                  Display_Located_Error
                    (Loc (E),
                     "unsupported data type (" &
                     Supported_Data_Representation'Image
                       (Data_Representation) &
                     ")",
                     Fatal => True);

               when Data_None =>
                  if Type_Source_Name /= No_Name then
                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     =>
                            CTU.Make_Defining_Identifier
                              (Type_Source_Name,
                               False));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                     declare
                        Source_Files : constant Name_Array :=
                          Get_Source_Text (E);
                        To_Include : Name_Id;
                     begin
                        if Source_Files'Length > 1 then
                           Display_Located_Error
                             (Loc (E),
                              "More than 1 header file for a C type",
                              Fatal => True);
                        elsif Source_Files'Length = 1 then
                           To_Include := Source_Files (Source_Files'First);
                           Get_Name_String (To_Include);
                           if Name_Buffer (Name_Len - 3 .. Name_Len) =
                             ".asn"
                           then
                              Name_Len   := Name_Len - 4;
                              To_Include := Name_Find;
                           end if;

                           Add_Include
                             (Make_Include_Clause
                                (Make_Defining_Identifier (To_Include, False)),
                              Preserve_Case => True);
                        end if;
                     end;

                     if Get_Source_Language (E) = Language_Simulink then
                        Add_Include
                          (Make_Include_Clause
                             (Make_Defining_Identifier
                                (Get_String_Name ("rtwtypes"),
                                 False),
                              False),
                           True);
                     end if;
                  elsif Get_Concurrency_Protocol (E) /= None_Specified then

                     --  Protected type that does not have struct members.
                     --  This piece of code is made to handle declaration
                     --  such as:
                     --  data implementation foo.i
                     --  *** NOTHING ***
                     --  properties
                     --     Concurrency_Control_Protocol => Protected_Access;
                     --  end foo.i;

                     --  Other protected objects are data components that have
                     --  subprogram accesses.

                     Append_Node_To_List
                       (Make_Member_Declaration
                          (Used_Type           => RE (RE_Protected_T),
                           Defining_Identifier =>
                             Make_Defining_Identifier (MN (M_Protected_Id))),
                        Protected_Struct_Members);

                     if not Is_Empty (Struct_Members) then
                        S := CTN.First_Node (Struct_Members);
                        while Present (S) loop
                           Append_Node_To_List (S, Protected_Struct_Members);
                           S := CTN.Next_Node (S);
                        end loop;
                     end if;

                     N :=
                       Make_Full_Type_Declaration
                         (Defining_Identifier => Map_C_Defining_Identifier (E),
                          Type_Definition     =>
                            Make_Struct_Aggregate
                              (Members => Protected_Struct_Members));

                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                  else
                     Display_Located_Error
                       (Loc (E),
                        "unspecified data representation",
                        Fatal => True);
                  end if;
            end case;

            --  Mark the data type as being handled and append it to
            --  the handled list.

            Set_Handling (E, By_Name, H_C_Type_Spec, N);

            --  In the case of a data type with accessor, visit the
            --  parameters of its features subprograms. It is
            --  important to do this *after* marking the type as
            --  handled, to avoid endless loops and *before* adding
            --  the type declaration to the package statements because
            --  the declaration order of type is important.

            if Get_Current_Backend_Kind = PolyORB_HI_C then
               if Data_Representation = Data_With_Accessors then
                  S := First_Node (Features (E));

                  while Present (S) loop
                     if AINU.Is_Data (Corresponding_Instance (S)) then
                        Visit (Corresponding_Instance (S));
                     end if;

                     S := Next_Node (S);
                  end loop;
               end if;
            end if;
         end if;

         Bind_AADL_To_Type_Definition
           (Identifier (E),
            Get_Handling (E, By_Name, H_C_Type_Spec));
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
        (E            : Node_Id;
         Real_Process : Boolean := True)
      is
         S           : Node_Id;
         C           : Node_Id;
         F           : Node_Id;
         I           : Node_Id;
         J           : Node_Id;
         D           : Node_Id;
         U           : Node_Id;
         P           : Node_Id;
         Feature     : Node_Id;
         Parent      : Node_Id;
         Src         : Node_Id;
         Dst         : Node_Id;
         Declaration : Node_Id;
         The_System  : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Remote_Process : Node_Id;
      begin
         if Real_Process then
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);

            Push_Entity (P);
            Push_Entity (U);
         end if;

         Set_Types_Header;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         if Get_Current_Backend_Kind = PolyORB_Kernel_C
           and then not AINU.Is_Empty (Subcomponents (E))
         then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               if AINU.Is_Data (Corresponding_Instance (S))
                 and then Is_Protected_Data (Corresponding_Instance (S))
               then
                  Add_Include (PKR.RH (RH_Deployment));

                  if POK_Flavor = ARINC653 then
                     Declaration :=
                       Make_Extern_Entity_Declaration
                         (Make_Variable_Declaration
                            (Make_Defining_Identifier
                               (Map_Associated_Locking_Entity_Name (S)),
                             PKR.RE (RE_Semaphore_Id_Type)));
                  else
                     Declaration :=
                       Make_Extern_Entity_Declaration
                         (Make_Variable_Declaration
                            (Make_Defining_Identifier
                               (Map_Associated_Locking_Entity_Name (S)),
                             PKR.RE (RE_Pok_Sem_Id_T)));
                  end if;

                  Append_Node_To_List
                    (Declaration,
                     CTN.Declarations (Current_File));
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if Get_Current_Backend_Kind = PolyORB_HI_C
           and then not AINU.Is_Empty (Subcomponents (The_System))
         then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AINU.Is_Device (Corresponding_Instance (C))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (C)) =
                   Get_Bound_Processor (E)
               then
                  --  Build the enumerator corresponding to the device
                  --  Note: we reuse the process name XXX
                  Visit_Device_Instance (Corresponding_Instance (C));

               --  We also visit ALL processes of the system to be
               --  sure that ALL types used in the distributed system
               --  are generated (and not only the ones related to the
               --  current node). This would ensure that ALL types used
               --  in the request_t type
               --  (see ocarina-backends-po_hi_c-request.adb) are used.

               elsif AINU.Is_Process (Corresponding_Instance (C)) then
                  if Present (Subcomponents (Corresponding_Instance (C))) then
                     S :=
                       First_Node (Subcomponents (Corresponding_Instance (C)));
                     while Present (S) loop
                        Visit_Component_Instance (Corresponding_Instance (S));
                        S := Next_Node (S);
                     end loop;
                  end if;
               end if;
               C := Next_Node (C);
            end loop;
         end if;

         --  We visit all the data of connected components, because
         --  we may need them in case of a distributed system.

         if not AINU.Is_Empty (Features (E)) then
            C := First_Node (Features (E));

            while Present (C) loop
               if Kind (C) = K_Port_Spec_Instance
                 and then not AINU.Is_Empty (Destinations (C))
               then
                  D := First_Node (Destinations (C));
                  I := Item (D);

                  if Get_Category_Of_Component (Parent_Component (I)) =
                    CC_Process
                  then
                     Remote_Process := Parent_Component (I);

                     if not AINU.Is_Empty (Subcomponents (Remote_Process)) then
                        S := First_Node (Subcomponents (Remote_Process));
                        while Present (S) loop
                           Visit (Corresponding_Instance (S));
                           S := Next_Node (S);
                        end loop;
                     end if;
                  end if;

                  if Present (I)
                    and then Kind (I) = K_Port_Spec_Instance
                    and then not AINU.Is_Empty (Destinations (I))
                  then
                     F := First_Node (Get_Destination_Ports (I));

                     while Present (F) loop
                        J := Item (F);

                        if Present (J) then
                           Visit (Parent_Component (J));
                        end if;
                        F := Next_Node (F);
                     end loop;
                  end if;
                  D := Next_Node (D);
               end if;

               C := Next_Node (C);
            end loop;

            C := First_Node (Features (E));

            while Present (C) loop
               if Kind (C) = K_Port_Spec_Instance
                 and then not AINU.Is_Empty (Sources (C))
               then
                  D := First_Node (Get_Source_Ports (C));
                  I := Item (D);

                  if Get_Category_Of_Component (Parent_Component (I)) =
                    CC_Process
                  then
                     Remote_Process := Parent_Component (I);

                     if not AINU.Is_Empty (Subcomponents (Remote_Process)) then
                        S := First_Node (Subcomponents (Remote_Process));

                        while Present (S) loop
                           Visit (Corresponding_Instance (S));
                           S := Next_Node (S);
                        end loop;
                     end if;
                  end if;

                  if Present (I)
                    and then Kind (I) = K_Port_Spec_Instance
                    and then not AINU.Is_Empty (Get_Source_Ports (I))
                  then
                     F := First_Node (Sources (I));
                     while Present (F) loop
                        J := Item (F);
                        if Present (J) then
                           Visit (Parent_Component (J));
                        end if;

                        F := Next_Node (F);
                     end loop;
                  end if;
                  D := Next_Node (D);
               end if;

               C := Next_Node (C);
            end loop;
         end if;

         --  Now, we visit all the features again
         --  to inspect the associated buses and virtual buses.

         if not AINU.Is_Empty (Features (E)) then
            Feature := First_Node (Features (E));

            while Present (Feature) loop
               if not AINU.Is_Empty (Sources (Feature)) then
                  Src := First_Node (Get_Source_Ports (Feature));

                  while Present (Src) loop
                     Parent := Parent_Component (Item (Src));

                     if AINU.Is_Process (Parent) and then Parent /= E then
                        if Present (Extra_Item (Src))
                          and then Present
                            (Get_Provided_Virtual_Bus_Class (Extra_Item (Src)))
                        then
                           Visit
                             (Get_Provided_Virtual_Bus_Class
                                (Extra_Item (Src)));
                        end if;
                     end if;

                     Src := Next_Node (Src);
                  end loop;
               end if;

               --  The destinations of F

               if not AINU.Is_Empty (Destinations (Feature)) then
                  Dst := First_Node (Get_Destination_Ports (Feature));

                  while Present (Dst) loop
                     Parent := Parent_Component (Item (Dst));

                     if AINU.Is_Process (Parent) and then Parent /= E then
                        if Get_Provided_Virtual_Bus_Class (Extra_Item (Dst)) /=
                          No_Node
                        then
                           Visit
                             (Get_Provided_Virtual_Bus_Class
                                (Extra_Item (Dst)));
                        end if;
                     end if;

                     Dst := Next_Node (Dst);
                  end loop;
               end if;

               Feature := Next_Node (Feature);
            end loop;
         end if;

         --  Unmark all the marked types

         Reset_Handlings;

         if Real_Process then
            Pop_Entity; -- U
            Pop_Entity; -- P
         end if;
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         F        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;

      begin
         --  Declare all necessary data types

         if not AINU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  Display_Located_Error
                    (Loc (F),
                     "Port features in subprogram are not supported",
                     Fatal => True);
               end if;

               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the subprogram

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            Push_Entity (C_Root);
         end if;

         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S. We don't parse directly process
               --  components for the POK backend. Instead, we parse
               --  all processes components from their runtime
               --  (processor or virtual processor).

               if Get_Current_Backend_Kind = PolyORB_Kernel_C
                 and then AINU.Is_Process_Or_Device
                   (Corresponding_Instance (S))
               then
                  null;
               else
                  if Get_Category_Of_Component (Corresponding_Instance (S)) /=
                    CC_Device
                  then
                     Visit (Corresponding_Instance (S));
                  end if;
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            Pop_Entity; --  C_Root
         end if;
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         F        : Node_Id;
      begin
         if not AINU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then AIN.Is_Data (F) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the thread

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Header_File;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);

      function Feature_Spg_Body (E : Node_Id) return Node_Id;
      --  Builds a body for a protected object procedure from an AADL
      --  subprogram spec E.

      ----------------------
      -- Feature_Spg_Body --
      ----------------------

      function Feature_Spg_Body (E : Node_Id) return Node_Id is
         N            : Node_Id;
         Call_Profile : List_Id;
         Param        : Node_Id;
         C_Access     : Node_Id;
         D            : Node_Id;
         Statements   : constant List_Id := New_List (CTN.K_Statement_List);
         Spg          : Node_Id;
      begin
         pragma Assert
           (Kind (E) = K_Subprogram_Spec_Instance
            or else Kind (E) = K_Subcomponent_Access_Instance);

         Spg := Corresponding_Instance (E);

         pragma Assert (AINU.Is_Subprogram (Spg));

         --  Make the call to __po_hi_protected_lock

         Call_Profile := New_List (CTN.K_Parameter_Profile);
         Append_Node_To_List
           (Make_Member_Designator
              (Make_Defining_Identifier (MN (M_Protected_Id)),
               Make_Defining_Identifier (PN (P_Value)),
               Is_Pointer => True),
            Call_Profile);

         Append_Node_To_List
           (Make_Call_Profile
              (Defining_Identifier => RE (RE_Protected_Lock),
               Parameters          => Call_Profile),
            Statements);

         Call_Profile := New_List (CTN.K_Parameter_Profile);

         if not AINU.Is_Empty (Features (Spg)) then
            Param := First_Node (Features (Spg));

            while Present (Param) loop
               if Kind (Param) = K_Parameter_Instance then
                  --  Create a parameter association

                  N := Map_C_Defining_Identifier (Param);
                  Append_Node_To_List (N, Call_Profile);
               end if;

               Param := Next_Node (Param);
            end loop;
         end if;

         --  2 - The list of all record fields given

         --  FIXME: Respect the mapping rules by setting the correct
         --  parameter orientation. For now all parameter are
         --  considered IN OUT. Provide all necessary routines
         --  (passing through intermediate variables, to prevent the
         --  user from cheating).

         if not AINU.Is_Empty (Features (Spg)) then
            C_Access := First_Node (Features (Spg));

            while Present (C_Access) loop
               if Kind (C_Access) = K_Subcomponent_Access_Instance then
                  D := Corresponding_Instance (C_Access);

                  if not AINU.Is_Empty (Subcomponents (D)) then
                     Param := First_Node (Subcomponents (D));

                     while Present (Param) loop
                        --  Create a parameter association
                        if AINU.Is_Data (Corresponding_Instance (Param)) then
                           N :=
                             Make_Variable_Address
                               (Make_Member_Designator
                                  (Defining_Identifier =>
                                     Map_C_Defining_Identifier (Param),
                                   Aggregate_Name =>
                                     Make_Defining_Identifier (PN (P_Value)),
                                   Is_Pointer => True));
                           Append_Node_To_List (N, Call_Profile);
                        end if;

                        Param := Next_Node (Param);
                     end loop;
                  end if;
               end if;

               C_Access := Next_Node (C_Access);
            end loop;
         end if;

         --  Add an include subprograms.h in types.c
         --  If we use subprograms

         if Get_Current_Backend_Kind = PolyORB_HI_C then
            Add_Include (PKR.RH (RH_Subprograms));
         end if;

         N :=
           Make_Call_Profile
             (CTN.Defining_Identifier
                (CTN.Subprogram_Node
                   (Backend_Node (Identifier (Corresponding_Instance (E))))),
              Call_Profile);
         Append_Node_To_List (N, Statements);

         --  Make the call to __po_hi_protected_unlock

         Call_Profile := New_List (CTN.K_Parameter_Profile);
         Append_Node_To_List
           (Make_Member_Designator
              (Make_Defining_Identifier (MN (M_Protected_Id)),
               Make_Defining_Identifier (PN (P_Value)),
               Is_Pointer => True),
            Call_Profile);

         Append_Node_To_List
           (Make_Call_Profile
              (Defining_Identifier => RE (RE_Protected_Unlock),
               Parameters          => Call_Profile),
            Statements);

         --  Build the subprogram implementation

         N :=
           Make_Function_Implementation
             (CTN.Feature_Subprogram_Node (Backend_Node (Identifier (E))),
              No_List,
              Statements);

         return N;
      end Feature_Spg_Body;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id; C : Node_Id := No_Node) is
      begin
         if C /= No_Node then
            C_Root := C;
         end if;

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
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ------------------------------
      -- Visit_Processor_Instance --
      ------------------------------

      procedure Visit_Processor_Instance (E : Node_Id) is
         S : Node_Id;
         A : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         A := CTN.Deployment_Node (Backend_Node (Identifier (E)));
         Push_Entity (C_Root);
         Push_Entity (A);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity;
         Pop_Entity;
      end Visit_Processor_Instance;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
         Processes : List_Id;
         S         : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            S         := AIN.First_Node (Processes);
            while Present (S) loop
               Visit (AIN.Item (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_Virtual_Processor_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         N                   : Node_Id;
         Data_Representation : Supported_Data_Representation;
      begin
         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            return;
         end if;

         --  At this time, we don't need to visit data instance for the
         --  POK generator.

         Data_Representation := Get_Data_Representation (E);

         if Get_Current_Backend_Kind = PolyORB_HI_C then
            Add_Include (PHR.RH (PHR.RH_Types));
         end if;

         if Data_Representation = Data_With_Accessors then
            if No (Get_Handling (E, By_Name, H_C_Type_Body)) then
               declare
                  C : Node_Id := First_Node (Subcomponents (E));
                  S : Node_Id;
               begin
                  --  Visit the subcomponents

                  while Present (C) loop
                     if AINU.Is_Data (Corresponding_Instance (C)) then
                        Visit (Corresponding_Instance (C));
                     end if;

                     C := Next_Node (C);
                  end loop;

                  --  Protected type

                  S := First_Node (Features (E));

                  while Present (S) loop
                     --  Build a subprogram spec and append it to
                     --  the visible part of the protected type.

                     N := Feature_Spg_Body (S);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                     S := Next_Node (S);
                  end loop;

                  --  Mark the data type as being handled

                  Set_Handling (E, By_Name, H_C_Type_Body, N);

                  --  FIXME : Handle correctly data with accessor
                  --  marshalling.

               end;
            end if;
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P           : constant Node_Id := CTN.Entity (U);
         S           : Node_Id;
         Declaration : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Types_Source;

         Start_Recording_Handlings;

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               if Get_Current_Backend_Kind = PolyORB_Kernel_C
                 and then AINU.Is_Data (Corresponding_Instance (S))
                 and then Is_Protected_Data (Corresponding_Instance (S))
               then

                  Add_Include (PKR.RH (RH_Deployment));

                  if POK_Flavor = ARINC653 then
                     Declaration :=
                       Make_Variable_Declaration
                         (Make_Defining_Identifier
                            (Map_Associated_Locking_Entity_Name (S)),
                          PKR.RE (RE_Semaphore_Id_Type));
                  else
                     Declaration :=
                       Make_Variable_Declaration
                         (Make_Defining_Identifier
                            (Map_Associated_Locking_Entity_Name (S)),
                          PKR.RE (RE_Pok_Sem_Id_T));
                  end if;

                  Append_Node_To_List
                    (Declaration,
                     CTN.Declarations (Current_File));
               else
                  Visit (Corresponding_Instance (S));
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         F : Node_Id;
      begin
         --  Declare all necessary data types

         if not AINU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  Display_Located_Error
                    (Loc (F),
                     "Port features in subprogram are not supported",
                     Fatal => True);
               end if;

               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            Push_Entity (C_Root);
         end if;

         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.
               if Get_Current_Backend_Kind = PolyORB_Kernel_C
                 and then
                   Get_Category_Of_Component (Corresponding_Instance (S)) =
                   CC_Process
               then
                  null;
               else
                  Visit (Corresponding_Instance (S));
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            Pop_Entity; --  C_Root
         end if;
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
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Source_File;

end Ocarina.Backends.C_Common.Types;
