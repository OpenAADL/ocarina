------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . P O _ H I _ C . R E Q U E S T      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2016 ESA & ISAE.      --
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
with Ocarina.Backends.Utils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Values;

with Ocarina.Backends.Properties;

package body Ocarina.Backends.PO_HI_C.Request is
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Utils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.Properties;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CV renames Ocarina.Backends.C_Values;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);

      --  Global variables for the generated entities. Note that it is
      --  safe to use global variable in this case because there is
      --  only one distributed application node and it is visited only
      --  once in this package.

      Request_Struct       : List_Id;
      Request_Union_List   : List_Id;
      Ports_Names_Array    : Node_Id;
      Operation_Identifier : Unsigned_Long_Long;
      Request_Declared     : Boolean;

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

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         S : Node_Id;
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P          : constant Node_Id := CTN.Entity (U);
         N          : Node_Id;
         C          : Node_Id;
         D          : Node_Id;
         F          : Node_Id;
         J          : Node_Id;
         I          : Node_Id;
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Device_Implementation : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Request_Header;

         --  Create the global lists

         Request_Struct := New_List (CTN.K_Enumeration_Literals);

         Operation_Identifier := 0;
         Request_Declared     := False;

         if not AINU.Is_Empty (Subcomponents (The_System)) then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AINU.Is_Device (Corresponding_Instance (C))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (C)) =
                   Get_Bound_Processor (E)
               then
                  Device_Implementation :=
                    Get_Implementation (Corresponding_Instance (C));

                  if Device_Implementation /= No_Node then
                     if not AINU.Is_Empty
                         (AIN.Subcomponents (Device_Implementation))
                     then
                        N :=
                          First_Node (Subcomponents (Device_Implementation));
                        while Present (N) loop
                           Visit_Component_Instance
                             (Corresponding_Instance (N));
                           N := Next_Node (N);
                        end loop;
                     end if;
                  end if;
               end if;
               C := Next_Node (C);
            end loop;
         end if;

         if not AINU.Is_Empty (Features (E)) then
            C := First_Node (Features (E));

            while Present (C) loop
               if Kind (C) = K_Port_Spec_Instance
                 and then Is_Out (C)
                 and then not AINU.Is_Empty (Destinations (C))
               then
                  D := First_Node (Get_Destination_Ports (C));
                  while Present (D) loop
                     I := Item (D);

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
                  end loop;
               end if;

               C := Next_Node (C);
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

         if Request_Declared then

            --  Create the enumeration type for all the operations of
            --  the distributed application.

            N :=
              Message_Comment
                ("Enumeration type for all the operations" &
                 " in the distributed application.");
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Member_Declaration
                (Defining_Identifier => Make_Defining_Identifier (MN (M_Port)),
                 Used_Type           => RE (RE_Port_T));
            Append_Node_To_List (N, Request_Struct);

            N :=
              Make_Member_Declaration
                (Defining_Identifier => Make_Defining_Identifier (MN (M_Vars)),
                 Used_Type           =>
                   Make_Union_Aggregate (Members => Request_Union_List));
            Append_Node_To_List (N, Request_Struct);

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Request_T),
                 Type_Definition     =>
                   Make_Struct_Aggregate (Members => Request_Struct));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            Bind_AADL_To_Request_Type (Identifier (E), N);

            N :=
              Make_Define_Statement
                (Defining_Identifier => RE (RE_Nb_Operations),
                 Value               =>
                   Make_Literal
                     (CV.New_Int_Value (Operation_Identifier, 1, 10)));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            Bind_AADL_To_Request (Identifier (E), Ports_Names_Array);
         else
            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Request_T),
                 Type_Definition     => Make_Defining_Identifier (TN (T_Int)));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            Bind_AADL_To_Request_Type (Identifier (E), N);
         end if;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (C_Root);

         Request_Union_List := New_List (CTN.K_Enumeration_Literals);

         Ports_Names_Array := Make_Array_Values;
         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  C_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         F              : Node_Id;
         N              : Node_Id;
         V              : Node_Id;
         Struct_Members : List_Id;
      begin
         if Has_Ports (E) then
            F                := First_Node (Features (E));
            Request_Declared := True;
            Add_Include (RH (RH_Types));
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then No (Get_Handling (F, By_Node, H_C_Request_Spec))
               then
                  Set_Handling (F, By_Node, H_C_Request_Spec, F);
                  Request_Declared := True;

                  if Is_Data (F) then
                     V :=
                       Map_C_Data_Type_Designator (Corresponding_Instance (F));
                  else
                     V := RE (RE_Bool_T);
                  end if;

                  if V /= No_Node then
                     Struct_Members := New_List (CTN.K_Enumeration_Literals);
                     Append_Node_To_List
                       (Make_Member_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (Map_C_Enumerator_Name (F)),
                           Used_Type =>
                             Make_Struct_Aggregate
                               (Members => Struct_Members)),
                        Request_Union_List);

                     N :=
                       Make_Member_Declaration
                         (Defining_Identifier =>
                            Make_Defining_Identifier
                              (Map_C_Enumerator_Name (F)),
                          Used_Type => V);
                     Append_Node_To_List (N, Struct_Members);

                     if No (Backend_Node (Identifier (F)))
                       or else
                       (Present (Backend_Node (Identifier (F)))
                        and then No
                          (CTN.Request_Type_Node
                             (Backend_Node (Identifier (F)))))
                     then
                        N :=
                          Make_Literal
                            (CV.New_Pointed_Char_Value
                               (Map_C_Enumerator_Name (F)));
                        Append_Node_To_List
                          (N,
                           CTN.Values (Ports_Names_Array));

                        Bind_AADL_To_Request_Type (Identifier (F), N);
                     end if;
                  end if;
               end if;
               F := Next_Node (F);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Header_File;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);

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

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := CTN.Entity (U);
         S : Node_Id;
         C : Node_Id;
         D : Node_Id;
         F : Node_Id;
         J : Node_Id;
         I : Node_Id;
         N : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Request_Source;

         Start_Recording_Handlings;

         if not AINU.Is_Empty (Features (E)) then
            C := First_Node (Features (E));

            while Present (C) loop
               if Kind (C) = K_Port_Spec_Instance
                 and then not AINU.Is_Empty (Destinations (C))
               then
                  D := First_Node (Destinations (C));
                  I := Item (D);

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

         if Present (Backend_Node (Identifier (E)))
           and then Present (CTN.Request_Node (Backend_Node (Identifier (E))))
         then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Ports_Names),
                           Array_Size          => RE (RE_Nb_Ports)),
                      Used_Type =>
                        Make_Constant_Type
                          (Make_Pointer_Type
                             (Make_Defining_Identifier (TN (T_Char))))),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   CTN.Request_Node (Backend_Node (Identifier (E))));

            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (C_Root);

         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  C_Root
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

end Ocarina.Backends.PO_HI_C.Request;
