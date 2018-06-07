------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . B A C K E N D S . E X P A N D E R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Locations;     use Locations;
with Ocarina.Namet; use Ocarina.Namet;
with Utils;         use Utils;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Instances.Properties;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;

package body Ocarina.Backends.Expander is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Instances.Properties;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AIU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   procedure Expand_Architecture_Instance (E : Node_Id);
   procedure Expand_Component_Instance (E : Node_Id);
   procedure Expand_Data_Instance (E : Node_Id);
   procedure Expand_Process_Instance (E : Node_Id);
   procedure Expand_Subprogram_Instance (E : Node_Id);
   procedure Expand_System_Instance (E : Node_Id);
   procedure Expand_Thread_Instance (E : Node_Id);

   Instance_Root : Node_Id := No_Node;

   function Internal_Expanded_Name (E : Node_Id) return Name_Id;
   function Expanded (E : Node_Id) return Boolean;
   procedure Set_Expanded (E : Node_Id);
   --  Routines to control the expansion of component instances

   ------------
   -- Expand --
   ------------

   procedure Expand (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Expand_Architecture_Instance (E);

         when K_Component_Instance =>
            Expand_Component_Instance (E);

         when others =>
            null;
      end case;

      if No (E) then
         Display_Error
           ("Cannot expand AADL models",
            Fatal   => True,
            Warning => False);
      end if;
   end Expand;

   ----------------------------------
   -- Expand_Architecture_Instance --
   ----------------------------------

   procedure Expand_Architecture_Instance (E : Node_Id) is
   begin
      Instance_Root := E;
      Expand (Root_System (E));
      Instance_Root := No_Node;
   end Expand_Architecture_Instance;

   -------------------------------
   -- Expand_Component_Instance --
   -------------------------------

   procedure Expand_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
      N        : Node_Id;
      I        : Node_Id;
      F        : Node_Id;
      K        : Node_Id;
      L        : Node_Id;
   begin
      if Expanded (E) then
         return;
      else
         Set_Expanded (E);
      end if;

      --  This part of the code is considered as unstable
      --  at this time.

      if not Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Feature_Group_Spec_Instance then
               if not Is_Empty (Features (F)) then
                  I := First_Node (Features (F));

                  while Present (I) loop
                     if Category = CC_Subprogram then
                        N :=
                          Ocarina.ME_AADL.AADL_Instances.Nutils.New_Node
                            (K_Parameter_Instance,
                             AIN.Loc (I));
                     else
                        N :=
                          Ocarina.ME_AADL.AADL_Instances.Nutils.New_Node
                            (AIN.Kind (I),
                             AIN.Loc (I));
                        AIN.Set_Identifier (N, AIU.Copy_Node (Identifier (I)));
                     end if;

                     if Kind (I) = K_Port_Spec_Instance then
                        AIN.Set_Is_Event (N, AIN.Is_Event (I));
                        AIN.Set_Is_Data (N, AIN.Is_Data (I));
                        AIN.Set_Is_In (N, AIN.Is_In (I));
                        AIN.Set_Is_Out (N, AIN.Is_Out (I));
                     end if;

                     AIN.Set_Corresponding_Instance
                       (N,
                        AIN.Corresponding_Instance (I));
                     AIN.Set_Sources (N, AIN.Sources (I));
                     AIN.Set_Destinations (N, AIN.Destinations (I));
                     AIN.Set_Parent_Component (N, AIN.Parent_Component (F));

                     if not Is_Empty (Sources (F)) then

                        K := First_Node (Sources (F));

                        L := No_Node;

                        while Present (K) loop
                           L :=
                             Find_Name_In_List
                               (Name (Identifier (I)),
                                Features (Item (K)));

                           if L /= No_Node then
                              exit;
                           end if;

                           K := Next_Node (K);
                        end loop;

                        if L = No_Node then
                           Display_Error
                             ("Cannot expand port/feature group",
                              Fatal   => True,
                              Warning => False);
                        end if;

                        Append_Node_To_List
                          (Make_Node_Container (L, Extra_Item (K)),
                           Sources (N));

                        Append_Node_To_List
                          (Make_Node_Container (N, Extra_Item (K)),
                           Destinations (L));
                     end if;

                     if not Is_Empty (Destinations (F)) then
                        K := First_Node (Destinations (F));

                        L := No_Node;

                        while Present (K) loop
                           L :=
                             Find_Name_In_List
                               (Name (Identifier (I)),
                                Features (Item (K)));

                           if L /= No_Node then
                              exit;
                           end if;

                           K := Next_Node (K);
                        end loop;

                        if L = No_Node then
                           Display_Error
                             ("Cannot expand port/feature group",
                              Fatal   => True,
                              Warning => False);
                        end if;

                        Append_Node_To_List
                          (Make_Node_Container (L, Extra_Item (K)),
                           Destinations (N));

                        Append_Node_To_List
                          (Make_Node_Container (N, Extra_Item (K)),
                           Sources (L));
                     end if;

                     AIU.Append_Node_To_List (N, Features (E));

                     I := Next_Node (I);
                  end loop;
               end if;
               I := Next_Node (F);
               AIU.Remove_Node_From_List (F, Features (E));
               F := I;
            else
               F := Next_Node (F);
            end if;
         end loop;
      end if;

      case Category is
         when CC_Data =>
            Expand_Data_Instance (E);

         when CC_Process =>
            Expand_Process_Instance (E);

         when CC_Subprogram =>
            Expand_Subprogram_Instance (E);

         when CC_System =>
            Expand_System_Instance (E);

         when CC_Thread =>
            Expand_Thread_Instance (E);

         when others =>
            null;
      end case;
   end Expand_Component_Instance;

   --------------------------
   -- Expand_Data_Instance --
   --------------------------

   procedure Expand_Data_Instance (E : Node_Id) is
      Data_Representation : Supported_Data_Representation;
      S                   : Node_Id;

   begin
      --  Expand all the subcomponents of the data

      if not Is_Empty (AIN.Subcomponents (E)) then
         S := AIN.First_Node (AIN.Subcomponents (E));
         while Present (S) loop
            --  Expand the component instance corresponding to the
            --  subcomponent S.

            Expand (Corresponding_Instance (S));
            S := AIN.Next_Node (S);
         end loop;
      end if;

      Data_Representation := Get_Data_Representation (E);

      case Data_Representation is
         when Data_Struct | Data_With_Accessors =>

            --  There are two ways to define data structure in AADL:
            --
            --  1) using subcomponents, or
            --  2) using the "Element_Names" property.
            --
            --  Before giving the instance tree to the generators, we
            --  modify all data structures defined using the
            --  "Element_Names" property into ones using the
            --  subcomponents.

            declare
               Fields : constant Name_Array := Get_Element_Names (E);
               Types  : constant List_Id    := Get_Base_Type (E);
               F      : Node_Id;
               N      : Node_Id;
            begin
               if Fields'Length = 0 then
                  --  The data component has no Element_Names property
                  --  defined, hence we have nothing to do.

                  return;
               end if;

               if AIN.Subcomponents (E) = No_List then
                  AIN.Set_Subcomponents (E, New_List (K_List_Id, No_Location));
               else
                  Display_Located_Error
                    (AIN.Loc (E),
                     "A data component cannot have both subcomponents" &
                     " and Element_Names",
                     Fatal => False);
               end if;

               F := ATN.First_Node (Types);

               for J in Fields'Range loop
                  N := New_Node (K_Subcomponent_Instance, AIN.Loc (E));
                  AIN.Set_Identifier
                    (N,
                     Make_Identifier
                       (AIN.Loc (E),
                        To_Lower (Fields (J)),
                        Fields (J),
                        N));
                  Set_Corresponding_Declaration (N, No_Node);
                  Set_Destinations (N, New_List (K_List_Id, No_Location));
                  Set_Corresponding_Instance
                    (N,
                     Ocarina.ME_AADL.AADL_Tree.Nodes.Entity (F));

                  Append_Node_To_List (N, AIN.Subcomponents (E));

                  F := ATN.Next_Node (F);
               end loop;
            end;

            --  Expand the subcomponents of the data that have been
            --  created

            S := AIN.First_Node (AIN.Subcomponents (E));
            while Present (S) loop
               --  Expand the component instance corresponding to the
               --  subcomponent S.

               Expand (Corresponding_Instance (S));
               S := AIN.Next_Node (S);
            end loop;

         when Data_Array =>

            --  There are two ways to define data array in AADL:
            --
            --  1) using subcomponents, or
            --  2) using the "data_model::base_type" property.
            --
            --  Before giving the instance tree to the generators, we
            --  modify all data structures defined using the
            --  "base_name" property into ones using the
            --  subcomponents.

            declare
               Types : constant List_Id := Get_Base_Type (E);
               F     : Node_Id;
               N     : Node_Id;
            begin
               if AIN.Subcomponents (E) = No_List then
                  AIN.Set_Subcomponents (E, New_List (K_List_Id, No_Location));
               else
                  Display_Located_Error
                    (AIN.Loc (E),
                     "A data component cannot have both subcomponents" &
                     " and Element_Names",
                     Fatal => False);
               end if;

               F := ATN.First_Node (Types);

               while Present (F) loop
                  N := New_Node (K_Subcomponent_Instance, AIN.Loc (E));
                  AIN.Set_Identifier
                    (N,
                     Ocarina.ME_AADL.AADL_Instances.Nodes.Identifier (E));
                  Set_Corresponding_Declaration (N, No_Node);
                  Set_Destinations (N, New_List (K_List_Id, No_Location));
                  Set_Corresponding_Instance
                    (N,
                     Ocarina.ME_AADL.AADL_Tree.Nodes.Entity (F));

                  Append_Node_To_List (N, AIN.Subcomponents (E));

                  F := ATN.Next_Node (F);
               end loop;
            end;

            --  Expand the subcomponents of the data that have been
            --  created

            S := AIN.First_Node (AIN.Subcomponents (E));
            while Present (S) loop
               --  Expand the component instance corresponding to the
               --  subcomponent S.

               Expand (Corresponding_Instance (S));
               S := AIN.Next_Node (S);
            end loop;

         when others =>
            null;

      end case;
   end Expand_Data_Instance;

   -----------------------------
   -- Expand_Process_Instance --
   -----------------------------

   procedure Expand_Process_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      --  Expand all the subcomponents of the process

      if not Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Expand the component instance corresponding to the
            --  subcomponent S.

            Expand (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Expand_Process_Instance;

   --------------------------------
   -- Expand_Subprogram_Instance --
   --------------------------------

   procedure Expand_Subprogram_Instance (E : Node_Id) is
      Call_Seq : Node_Id;
      Spg_Call : Node_Id;
      F        : Node_Id;
   begin
      --  Expand all data types in the features

      if not Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) /= K_Feature_Group_Spec_Instance
              and then Present (Corresponding_Instance (F))
            then
               Expand (Corresponding_Instance (F));
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      --  Expand all the call sequences of the subprogram

      if not Is_Empty (Calls (E)) then
         Call_Seq := First_Node (Calls (E));

         while Present (Call_Seq) loop
            --  For each call sequence expand all the called
            --  subprograms.

            if not Is_Empty (Subprogram_Calls (Call_Seq)) then
               Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

               while Present (Spg_Call) loop
                  Expand (Corresponding_Instance (Spg_Call));

                  Spg_Call := Next_Node (Spg_Call);
               end loop;
            end if;

            Call_Seq := Next_Node (Call_Seq);
         end loop;
      end if;
   end Expand_Subprogram_Instance;

   ----------------------------
   -- Expand_System_Instance --
   ----------------------------

   procedure Expand_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      --  Expand all the subcomponents of the system

      if not Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Expand the component instance corresponding to the
            --  subcomponent S.

            Expand (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Expand_System_Instance;

   ----------------------------
   -- Expand_Thread_Instance --
   ----------------------------

   procedure Expand_Thread_Instance (E : Node_Id) is
      Call_Seq : Node_Id;
      Spg_Call : Node_Id;
      F        : Node_Id;
      P        : constant Supported_Thread_Dispatch_Protocol :=
        Get_Thread_Dispatch_Protocol (E);
      K : constant Supported_Thread_Implementation :=
        Get_Thread_Implementation_Kind (E);
      N : Node_Id;
      G : Node_Id;
   begin
      --  Expand all data types

      if not Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance
              and then Ocarina.ME_AADL.AADL_Instances.Nodes.Is_Data (F)
            then
               Expand (Corresponding_Instance (F));
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      --  Expand all the call sequences of the thread

      if not Is_Empty (Calls (E)) then
         Call_Seq := First_Node (Calls (E));

         while Present (Call_Seq) loop
            --  For each call sequence expand all the called
            --  subprograms.

            if not Is_Empty (Subprogram_Calls (Call_Seq)) then
               Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

               while Present (Spg_Call) loop
                  Expand (Corresponding_Instance (Spg_Call));

                  Spg_Call := Next_Node (Spg_Call);
               end loop;
            end if;

            Call_Seq := Next_Node (Call_Seq);
         end loop;
      end if;

      --  If the thread is a Hybrid thread, we add a new fake event
      --  port called Period_Event_Ü. This port will receive the
      --  messages from the hybrid tasks driver.

      if P = Thread_Hybrid then
         N := New_Node (K_Port_Spec_Instance, No_Location);
         Set_Is_In (N, True);
         Set_Is_Out (N, False);
         Set_Is_Event (N, True);
         Set_Is_Data (N, False);
         Set_Identifier
           (N,
            Make_Identifier
              (No_Location,
               Get_String_Name ("period_event_ü"),
               Get_String_Name ("Period_Event_Ü"),
               N));
         Set_Sources (N, New_List (K_List_Id, No_Location));
         Set_Destinations (N, New_List (K_List_Id, No_Location));
         Set_Parent_Component (N, E);

         --  If the thread implementation kind is
         --  Thread_With_Port_Compute_Entrypoint, then, the port
         --  must also have a compute entry point. We relocate the
         --  compute entrypoint of the thread to be the one of the
         --  extra added port.

         if K = Thread_With_Port_Compute_Entrypoint then
            declare
               use Ocarina.Backends.Utils;
               use ATN;

               Mode    : Name_Id;
               CE      : Node_Id;
               C       : Node_Id := No_Node;
               Success : Boolean := True;
               M       : Node_Id;
            begin
               AIN.Set_Properties (N, New_List (AIN.K_List_Id, No_Location));

               if Has_Modes (E) then

                  --  FIXME
                  --  Create a new property for each mode
                  --  instead of replacing it

                  G := AIN.First_Node (AIN.Modes (E));
                  while Present (G) loop
                     Mode := AIN.Name (AIN.Identifier (G));
                     CE   := Get_Thread_Compute_Entrypoint (E, Mode);
                     if Present (CE) then
                        C := CE;

                        pragma Assert (Present (Instance_Root));

                        Success :=
                          Add_Property_Instance
                            (Instance_Root,
                             N,
                             AIN.Corresponding_Declaration (CE),
                             Override_Mode => False);

                        M := Get_Port_Compute_Entrypoint (N, Mode);

                        if ATN.Kind
                            (ATN.Expanded_Single_Value
                               (AIN.Property_Association_Value (M))) =
                          ATN.K_Reference_Term
                        then
                           --  FIXME :
                           --  Should be do by the replace_property_instance

                           ATN.Set_Entity
                             (ATN.Reference_Term
                                (ATN.Expanded_Single_Value
                                   (AIN.Property_Association_Value (M))),
                              (ATN.Entity
                                 (ATN.Reference_Term
                                    (ATN.Expanded_Single_Value
                                       (AIN.Property_Association_Value
                                          (CE))))));
                        end if;
                        Remove_Node_From_List (CE, AIN.Properties (E));

                     end if;

                     G := AIN.Next_Node (G);
                  end loop;
               else
                  CE := Get_Thread_Compute_Entrypoint (E);
                  C  := CE;

                  AIN.Set_Properties
                    (N,
                     New_List (AIN.K_List_Id, No_Location));

                  pragma Assert (Present (Instance_Root));

                  Success :=
                    Replace_Property_Instance
                      (Instance_Root,
                       N,
                       CE,
                       Override_Mode => True);

                  M := Get_Port_Compute_Entrypoint (N);

                  if ATN.Kind
                      (ATN.Expanded_Single_Value
                         (AIN.Property_Association_Value (M))) =
                    ATN.K_Reference_Term
                  then
                     --  FIXME :
                     --  Should be do by the replace_property_instance

                     ATN.Set_Entity
                       (ATN.Reference_Term
                          (ATN.Expanded_Single_Value
                             (AIN.Property_Association_Value (M))),
                        (ATN.Entity
                           (ATN.Reference_Term
                              (ATN.Expanded_Single_Value
                                 (AIN.Property_Association_Value (CE))))));
                  end if;
               end if;

               if not Success then
                  raise Program_Error;
               end if;

               if No (C) then
                  --  Display an error if the user did not provide a
                  --  compute entrypoint.

                  Display_Located_Error
                    (AIN.Loc (E),
                     "Missing compute entrypoint for this hybrid thread",
                     Fatal => True);
               end if;
            end;
         end if;

         if not Is_Empty (Features (E)) then
            Append_Node_To_List (N, Features (E));
         end if;
      end if;
   end Expand_Thread_Instance;

   ----------------------------
   -- Internal_Expanded_Name --
   ----------------------------

   function Internal_Expanded_Name (E : Node_Id) return Name_Id is
      Expansion_Annotation_Name : constant String := "%expanded%";
   begin
      Set_Str_To_Name_Buffer (Expansion_Annotation_Name);
      Add_Str_To_Name_Buffer (E'Img);
      return Name_Find;
   end Internal_Expanded_Name;

   --------------
   -- Expanded --
   --------------

   function Expanded (E : Node_Id) return Boolean is
   begin
      return Get_Name_Table_Byte (Internal_Expanded_Name (E)) = 1;
   end Expanded;

   ------------------
   -- Set_Expanded --
   ------------------

   procedure Set_Expanded (E : Node_Id) is
   begin
      Set_Name_Table_Byte (Internal_Expanded_Name (E), 1);
   end Set_Expanded;

end Ocarina.Backends.Expander;
