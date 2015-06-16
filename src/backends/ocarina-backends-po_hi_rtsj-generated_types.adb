------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BACKENDS.PO_HI_RTSJ.GENERATED_TYPES                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

--  with Namet;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.RTSJ_Tree.Nutils;
--  with Ocarina.Backends.RTSJ_Values;
with Ocarina.Backends.PO_HI_RTSJ.Mapping;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;

package body Ocarina.Backends.PO_HI_RTSJ.Generated_Types is

   --  use Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   --  use Ocarina.Backends.RTSJ_Values;
   use Ocarina.Backends.PO_HI_RTSJ.Mapping;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;

   --  Global variables declaration
   Main_Class    : Node_Id;
   Types_Classes : List_Id;

   -----------------
   -- Source_File --
   -----------------
   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);

      -----------
      -- Visit --
      -----------
      procedure Visit (E : Node_Id) is
      begin
         case AIN.Kind (E) is
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

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------
      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         --  Visit all the subcomponents of the system
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_System_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------
      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           RTN.Distributed_Application_Unit
             (RTN.Deployment_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := RTN.Entity (U);
         S : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Generated_Types_Source;

         --  Global variables initialization
         Types_Classes := New_List (K_Class_List);

         --  Start recording the handling since they have to be reset
         --  for each node.
         Start_Recording_Handlings;

         --  Visit all the subcomponents of the process
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));
            while Present (S) loop

               --  Visit the component instance corresponding to the
               --  subcomponent S.
               Visit (Corresponding_Instance (S));
               S := AIN.Next_Node (S);

            end loop;
         end if;

         --  Unmark all the marked types
         Reset_Handlings;

         --  Main class declaration
         Main_Class :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Generated_Types)),
              Classes => Types_Classes);
         RTU.Append_Node_To_List (Main_Class, RTN.Statements (Current_File));

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------
      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         P        : Node_Id;
      begin
         --  Declare all necessary data types. We cannot rely only on
         --  subprogram calls to generate necessary data type becaus
         --  threads may not contain subprogram calls.
         if not AINU.Is_Empty (Features (E)) then
            P := AIN.First_Node (Features (E));

            while Present (P) loop
               if AIN.Kind (P) = K_Port_Spec_Instance then
                  Visit (Corresponding_Instance (P));
               end if;
               P := AIN.Next_Node (P);
            end loop;
         end if;

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

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------
      procedure Visit_Subprogram_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         P        : Node_Id;
      begin
         --  Declare all necessary data types
         if not AINU.Is_Empty (Features (E)) then
            P := AIN.First_Node (Features (E));

            while Present (P) loop
               if Present (Corresponding_Instance (P)) then
                  Visit (Corresponding_Instance (P));
               end if;
               P := AIN.Next_Node (P);
            end loop;
         end if;

         --  Visit all the call sequences of the subprogram
         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := AIN.First_Node (Calls (E));

            while Present (Call_Seq) loop

               --  For each call sequence, visit all the called
               --  subprograms
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

      -------------------------
      -- Visit_Data_Instance --
      -------------------------
      procedure Visit_Data_Instance (E : Node_Id) is
         Data_Representation : Supported_Data_Representation;
         N                   : Node_Id;
         Spec                : Node_Id;
         Impl                : Node_Id;
         Param               : Node_Id;
         Statement           : Node_Id;
         --  Put_Method : Node_Id;
         --  Get_Method : Node_Id;
         Name              : Name_Id;
         Language_Type     : Supported_Source_Language;
         Class_Attributes  : constant List_Id := New_List (K_Attribute_List);
         Method_Statements : constant List_Id := New_List (K_Statement_List);
         Class_Methods     : constant List_Id := New_List (K_Method_List);
      begin
         --  Do not generate RTSJ type more than once
         if No (Get_Handling (E, By_Name, H_RTSJ_Type)) then

            Language_Type := Get_Source_Language (E);

            if Language_Type = Language_RTSJ then
               --  If the type is defined through a RTSJ type, then
               --  we simply drag a dependency onto the RTSJ file
               --  that hosts this type

               Name := Get_Type_Source_Name (E);

               if Name = No_Name then

                  Display_Located_Error
                    (AIN.Loc (E),
                     "RTSJ opaque types require the definition of the " &
                     "'Type_Source_Name' property",
                     Fatal => True);
               end if;

            else

               Data_Representation := Get_Data_Representation (E);

               case Data_Representation is

                  when Data_Integer =>
                     --  Put_Method := RE (RE_Put_Int);
                     --  Get_Method := RE (RE_Get_Int);
                     N :=
                       Make_Variable_Declaration
                         (Visibility          => No_List,
                          Used_Type           => New_Node (K_Int),
                          Defining_Identifier =>
                            (Make_Defining_Identifier (VN (V_Value))));
                     RTU.Append_Node_To_List (N, Class_Attributes);

                  when others =>
                     null;
               end case;

               --  Constructor
               Param :=
                 Make_Parameter_Specification
                   (Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_Value)),
                    Parameter_Type => New_Node (K_Int));
               Spec :=
                 Make_Function_Specification
                   (Visibility          => Make_List_Id (RE (RE_Public)),
                    Defining_Identifier => Map_RTSJ_Defining_Identifier (E),
                    Parameters          => Make_List_Id (Param));
               Statement :=
                 Make_Assignment_Statement
                   (Defining_Identifier =>
                      Make_Pointed_Notation
                        (Left_Member  => RE (RE_This),
                         Right_Member =>
                           Make_Defining_Identifier (VN (V_Value))),
                    Expression => Make_Defining_Identifier (VN (V_Value)));
               Impl :=
                 Make_Function_Implementation
                   (Specification => Spec,
                    Statements    => Make_List_Id (Statement));
               RTU.Append_Node_To_List (Impl, Class_Methods);

               --  Store method
               Param :=
                 Make_Parameter_Specification
                   (Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_Msg)),
                    Parameter_Type =>
                      Make_Defining_Identifier (ON (O_Message)));
               Spec :=
                 Make_Function_Specification
                   (Visibility          => Make_List_Id (RE (RE_Public)),
                    Return_Type         => New_Node (K_Void),
                    Defining_Identifier => RE (RE_Store),
                    Parameters          => Make_List_Id (Param));
               Statement :=
                 Make_Pointed_Notation
                   (Left_Member  => Make_Defining_Identifier (VN (V_Msg)),
                    Right_Member =>
                      Make_Call_Function
                        (Defining_Identifier => RE (RE_Put_Int),
                         Parameters          =>
                           Make_List_Id
                             (Make_Defining_Identifier (VN (V_Value)))));
               Impl :=
                 Make_Function_Implementation
                   (Specification => Spec,
                    Statements    => Make_List_Id (Statement));
               RTU.Append_Node_To_List (Impl, Class_Methods);

               --  Set method
               Param :=
                 Make_Parameter_Specification
                   (Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_Msg)),
                    Parameter_Type =>
                      Make_Defining_Identifier (ON (O_Message)));
               Spec :=
                 Make_Function_Specification
                   (Visibility          => Make_List_Id (RE (RE_Public)),
                    Return_Type         => New_Node (K_Void),
                    Defining_Identifier => RE (RE_Set),
                    Parameters          => Make_List_Id (Param));
               Statement :=
                 Make_Assignment_Statement
                   (Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_Value)),
                    Expression =>
                      Make_Expression
                        (Make_Pointed_Notation
                           (Left_Member =>
                              Make_Defining_Identifier (VN (V_Msg)),
                            Right_Member =>
                              Make_Call_Function
                                (Defining_Identifier => RE (RE_Get_Int)))));
               Impl :=
                 Make_Function_Implementation
                   (Specification => Spec,
                    Statements    => Make_List_Id (Statement));
               RTU.Append_Node_To_List (Impl, Class_Methods);

               --  Copy method
               Param :=
                 Make_Parameter_Specification
                   (Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_New_Value)),
                    Parameter_Type =>
                      Make_Defining_Identifier (ON (O_Generated_Type)));
               Spec :=
                 Make_Function_Specification
                   (Visibility          => Make_List_Id (RE (RE_Public)),
                    Return_Type         => New_Node (K_Void),
                    Defining_Identifier => RE (RE_Copy),
                    Parameters          => Make_List_Id (Param));
               Statement :=
                 Make_Variable_Declaration
                   (Used_Type           => Map_RTSJ_Defining_Identifier (E),
                    Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_Tmp)),
                    Value =>
                      Make_Cast_Statement
                        (Cast_Type => Map_RTSJ_Defining_Identifier (E),
                         Defining_Identifier =>
                           Make_Defining_Identifier (VN (V_New_Value))));
               RTU.Append_Node_To_List (Statement, Method_Statements);
               Statement :=
                 Make_Assignment_Statement
                   (Defining_Identifier =>
                      Make_Pointed_Notation
                        (Left_Member  => RE (RE_This),
                         Right_Member =>
                           Make_Defining_Identifier (VN (V_Value))),
                    Expression =>
                      Make_Pointed_Notation
                        (Left_Member  => Make_Defining_Identifier (VN (V_Tmp)),
                         Right_Member =>
                           Make_Defining_Identifier (VN (V_Value))));
               RTU.Append_Node_To_List (Statement, Method_Statements);
               Impl :=
                 Make_Function_Implementation
                   (Specification => Spec,
                    Statements    => Method_Statements);
               RTU.Append_Node_To_List (Impl, Class_Methods);

               N :=
                 Make_Class_Statement
                   (Visibility =>
                      Make_List_Id (RE (RE_Public), RE (RE_Static)),
                    Defining_Identifier =>
                      Map_RTSJ_Defining_Identifier (E, True),
                    Implements =>
                      Make_List_Id
                        (Make_Defining_Identifier (ON (O_Generated_Type))),
                    Attributes => Class_Attributes,
                    Methods    => Class_Methods);
               RTU.Append_Node_To_List (N, Types_Classes);

               --  Mark the data type as being handled
               Set_Handling (E, By_Name, H_RTSJ_Type, N);

            end if;
         end if;
      end Visit_Data_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_RTSJ.Generated_Types;
