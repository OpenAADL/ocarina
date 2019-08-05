------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                    O C A R I N A . I N S T A N C E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2019 ESA & ISAE.      --
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

with Errors;
with Locations;
with Utils;
with Ocarina.Namet;
with Ocarina.Output;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Debug;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.Printers;

with Ocarina.Options;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.Processor.Properties;
with Ocarina.Instances.Components;
with Ocarina.Instances.Namespaces;
with Ocarina.Instances.Processor.Properties;

package body Ocarina.Instances is

   use Errors;
   use Locations;
   use Utils;
   use Ocarina.Namet;
   use Ocarina.Output;

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.Printers;

   use Ocarina.Options;
   use Ocarina.Analyzer.AADL.Finder;
   use Ocarina.Processor.Properties;
   use Ocarina.Instances.Components;
   use Ocarina.Instances.Namespaces;
   use Ocarina.Instances.Processor.Properties;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;

   function Build_Instance_Internal_Name (N : Node_Id) return Name_Id;
   --  Factorize code between (Get/Set)_Instance

   ----------------------------------
   -- Append_To_Namespace_Instance --
   ----------------------------------

   procedure Append_To_Namespace_Instance
     (Instance_Root   : Node_Id;
      Entity_Instance : Node_Id)
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert
        (Kind (Entity_Instance) = K_Component_Instance
         or else Kind (Entity_Instance) = K_Subcomponent_Instance);

      Namespace_Model       : Node_Id;
      Namespace_Instance    : Node_Id;
      Component_Instance    : Node_Id;
      Subcomponent_Instance : Node_Id;

   begin
      --  Set namespaces

      if Kind (Entity_Instance) = K_Component_Instance then
         Component_Instance := Entity_Instance;
      elsif Kind (Entity_Instance) = K_Subcomponent_Instance then
         Component_Instance := Corresponding_Instance (Entity_Instance);
      else
         raise Program_Error;
      end if;

      Namespace_Model :=
        ATN.Namespace (Corresponding_Declaration (Component_Instance));
      Namespace_Instance :=
        Instantiate_Namespace (Instance_Root, Namespace_Model);

      --  Append the subcomponents before the component itself since
      --  the AADL model may be converted into a language that
      --  requires entities to be declared in order to be used.

      if not Is_Empty (AIN.Subcomponents (Component_Instance)) then
         Subcomponent_Instance :=
           AIN.First_Node (AIN.Subcomponents (Component_Instance));

         while Present (Subcomponent_Instance) loop
            Append_To_Namespace_Instance
              (Instance_Root,
               Subcomponent_Instance);
            Subcomponent_Instance := AIN.Next_Node (Subcomponent_Instance);
         end loop;
      end if;

      --  Append the component itself

      if No
          (Get_First_Contained_Homonym_Instance
             (AIN.Declarations (Namespace_Instance),
              Component_Instance))
      then
         Append_Node_To_List
           (Make_Node_Container (Component_Instance),
            AIN.Declarations (Namespace_Instance));
      end if;
   end Append_To_Namespace_Instance;

   -----------------------
   -- Instantiate_Model --
   -----------------------

   function Instantiate_Model (Root : Node_Id;
                               Exit_If_Error : Boolean := True)
                              return Node_Id
   is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      Root_Systems  : Node_List;
      Instance_Root : Node_Id;
      Root_System   : Node_Id := No_Node;
      List_Node     : Node_Id;
      L1            : Node_List;

      procedure Report_Root_Systems_To_User;

      procedure Report_Root_Systems_To_User is
      begin
         Error_Loc (1) := No_Location;
         DE ("Please select a root system among ");
         List_Node := Root_Systems.First;
         while Present (List_Node) loop
            Error_Loc (1)  := ATN.Loc (List_Node);
            Error_Name (1) := ATE.Get_Name_Of_Entity
                    (List_Node, False, True);
            DE ("%");
            List_Node := ATN.Next_Entity (List_Node);
         end loop;
         Exit_On_Error (True, "Cannot instantiate AADL model");
      end Report_Root_Systems_To_User;

   begin
      Errors.Initialize;
      N_Errors   := 0;
      N_Warnings := 0;

      --  Find all the root systems, i.e. AADL system implementations
      --  not connected to any other AADL entity.

      Root_Systems := Find_All_Root_Systems (Root);

      if Root_Systems.First /= Root_Systems.Last then
         --  If the user provided a particular root system to
         --  instantiate, find this system in the root system
         --  list. Otherwise, display an error message indicating that
         --  there are too many systems eligible for instantiation.

         if Root_System_Name /= No_Name then
            List_Node := Root_Systems.First;
            while Present (List_Node) loop
               exit when
                 (To_Lower (ATE.Get_Name_Of_Entity (List_Node, False))
                    = To_Lower (Root_System_Name)
                    or else
                    To_Lower (ATE.Get_Name_Of_Entity (List_Node, False, True))
                    = To_Lower (Root_System_Name));

               List_Node := ATN.Next_Entity (List_Node);
            end loop;

            if Present (List_Node) then
               --  We found a root system matching user name, use it
               Root_System := List_Node;
            else
               --  Else report error to user
               Report_Root_Systems_To_User;
            end if;

         else
            --  User provided no root system, there are multiple ones,
            --  report and exit
            Report_Root_Systems_To_User;
         end if;

      else
         --  If there is only one root system, choose it

         Root_System := Root_Systems.First;

         if No (Root_System) then
            Error_Loc (1) := No_Location;
            Exit_On_Error (Exit_If_Error, "Cannot find a root system");
            return No_Node;
         end if;

         if Root_System_Name /= No_Name
           and then
           To_Lower (ATE.Get_Name_Of_Entity (Root_System, False)) /=
           To_Lower (Root_System_Name)
           and then
           To_Lower (ATE.Get_Name_Of_Entity (Root_System, False, True)) /=
           To_Lower (Root_System_Name)
         then
            Error_Name (1) := Root_System_Name;
            Error_Name (2) := ATE.Get_Name_Of_Entity
              (Root_System, False, True);
            DE ("system % is not a root system, use %");
            Root_System := No_Node;
            Exit_On_Error (Exit_If_Error, "Cannot instantiate AADL model");
         end if;
      end if;

      --  If the chosen top level system does not correspond to the
      --  system name given by the user, complain by displaying an
      --  error and aborting instantiation.

      if No (Root_System) then
         Instance_Root := No_Node;
         Exit_On_Error (Exit_If_Error,
                        "Cannot instantiate full model, exit now");
         return No_Node;
      else
         --  The first step of the instantiation consist of propagate the
         --  properties declared in the AADL packages to the AADL
         --  component they can apply to.

         Diffuse_Package_Properties_To_Entities (Root);

         Instance_Root := New_Node (K_Architecture_Instance, No_Location);
         Set_Namespaces (Instance_Root, New_List (K_List_Id, No_Location));
         Set_Unnamed_Namespace (Instance_Root, No_Node);

         --  Begin the instantiation

         Set_Root_System
           (Instance_Root,
            Instantiate_Component (Instance_Root, Root_System));
         Compute_Property_Instance_Values (Instance_Root);
      end if;

      if N_Errors /= 0 or else N_Warnings /= 0 then
         Error_Loc (1) := No_Location;
         Error_Int (1) := N_Errors;
         Error_Int (2) := N_Warnings;

         Set_Str_To_Name_Buffer ("error");

         if N_Errors > 1 then
            Add_Char_To_Name_Buffer ('s');
         end if;

         Error_Name (1) := Name_Find;

         Set_Str_To_Name_Buffer ("warning");

         if N_Warnings > 1 then
            Add_Char_To_Name_Buffer ('s');
         end if;

         Error_Name (2) := Name_Find;
         DE ("Total: $%and $%");
      end if;

      if No (Instance_Root) then
         Set_Standard_Error;
         Write_Line ("Cannot instantiate AADL models");

      elsif Debug_Mode and then Get_Current_Action = Instantiate_Model then
         Print_AADL_Tree
           (Instance_Root,
            Ocarina.ME_AADL.AADL_Instances.Debug.W_Node_Id'Access);
      end if;

      --  XXX: In some cases there are some AADL Entities that are not
      --  intanciated. For example a subprogram or a data that is used
      --  only in the scope of a BA annex : For instance a BA variable
      --  that have as Classifier_Ref a Data component that is not
      --  used in other AADL components, An other case, a Subprogram
      --  component called only in the scope of one or more Behavior
      --  specification of other components.  In these cases, we
      --  instanciate these components to able to use them in the code
      --  generation backend.

      --  This step is probably too large, and could be restricted to
      --  subprogram and data component types only.

      L1 := Find_All_Declarations (Root,
                                   (ATN.K_Component_Type,
                                    ATN.K_Component_Implementation));
      List_Node := L1.First;
      while Present (List_Node) loop
         if No (Default_Instance (List_Node)) then
            if ATE.Get_Category_Of_Component (List_Node) = CC_Subprogram
              or else ATE.Get_Category_Of_Component (List_Node) = CC_Data
            then
               Set_Instance (List_Node,
                             Instantiate_Component (Instance_Root, List_Node));
            end if;
         end if;
         List_Node := ATN.Next_Entity (List_Node);
      end loop;

      return Instance_Root;
   end Instantiate_Model;

   -----------------------
   -- Get_First_Homonym --
   -----------------------

   function Get_First_Homonym
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id
   is
      List_Node : Node_Id;
   begin
      if Name_Of_Declaration = No_Name or else Declaration_List = No_List then
         return No_Node;
      end if;

      List_Node := ATN.First_Node (Declaration_List);

      while Present (List_Node) loop
         exit when ATE.Get_Name_Of_Entity (List_Node, False) =
           Name_Of_Declaration;
         List_Node := ATN.Next_Node (List_Node);
      end loop;

      return List_Node;
   end Get_First_Homonym;

   --------------------------------
   -- Get_First_Homonym_Instance --
   --------------------------------

   function Get_First_Homonym_Instance
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id
   is
      List_Node : Node_Id;
   begin
      if Name_Of_Declaration = No_Name or else Declaration_List = No_List then
         return No_Node;
      end if;

      List_Node := AIN.First_Node (Declaration_List);

      while Present (List_Node) loop
         exit when Get_Name_Of_Entity (List_Node, False) = Name_Of_Declaration;
         List_Node := AIN.Next_Node (List_Node);
      end loop;

      return List_Node;
   end Get_First_Homonym_Instance;

   ---------------------------------
   -- Get_First_Contained_Homonym --
   ---------------------------------

   function Get_First_Contained_Homonym
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id
   is
      List_Node : Node_Id;
   begin
      if Name_Of_Declaration = No_Name or else Declaration_List = No_List then
         return No_Node;
      end if;

      List_Node := AIN.First_Node (Declaration_List);

      while Present (List_Node) loop
         exit when Get_Name_Of_Entity (AIN.Item (List_Node), False) =
           Name_Of_Declaration;
         List_Node := AIN.Next_Node (List_Node);
      end loop;

      if No (List_Node) then
         return List_Node;
      else
         return AIN.Item (List_Node);
      end if;
   end Get_First_Contained_Homonym;

   ------------------------------------------
   -- Get_First_Contained_Homonym_Instance --
   ------------------------------------------

   function Get_First_Contained_Homonym_Instance
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id
   is
      List_Node : Node_Id;
   begin
      if Name_Of_Declaration = No_Name or else Declaration_List = No_List then
         return No_Node;
      end if;

      List_Node := AIN.First_Node (Declaration_List);

      while Present (List_Node) loop
         exit when Get_Name_Of_Entity (AIN.Item (List_Node), False) =
           Name_Of_Declaration;
         List_Node := AIN.Next_Node (List_Node);
      end loop;

      if No (List_Node) then
         return List_Node;
      else
         return AIN.Item (List_Node);
      end if;
   end Get_First_Contained_Homonym_Instance;

   -----------------------
   -- Get_First_Homonym --
   -----------------------

   function Get_First_Homonym
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id
   is
      pragma Assert (Present (Declaration));
   begin
      return Get_First_Homonym
          (Declaration_List,
           Get_Name_Of_Entity (Declaration, False));
   end Get_First_Homonym;

   --------------------------------
   -- Get_First_Homonym_Instance --
   --------------------------------

   function Get_First_Homonym_Instance
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id
   is
      pragma Assert (Present (Declaration));
   begin
      return Get_First_Homonym_Instance
          (Declaration_List,
           ATE.Get_Name_Of_Entity (Declaration, False));
   end Get_First_Homonym_Instance;

   ---------------------------------
   -- Get_First_Contained_Homonym --
   ---------------------------------

   function Get_First_Contained_Homonym
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id
   is
      pragma Assert (Present (Declaration));
   begin
      return Get_First_Contained_Homonym
          (Declaration_List,
           ATE.Get_Name_Of_Entity (Declaration, False));
   end Get_First_Contained_Homonym;

   ------------------------------------------
   -- Get_First_Contained_Homonym_Instance --
   ------------------------------------------

   function Get_First_Contained_Homonym_Instance
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id
   is
      pragma Assert (Present (Declaration));
   begin
      return Get_First_Contained_Homonym_Instance
          (Declaration_List,
           Get_Name_Of_Entity (Declaration, False));
   end Get_First_Contained_Homonym_Instance;

   ----------------------------------
   -- Build_Instance_Internal_Name --
   ----------------------------------

   function Build_Instance_Internal_Name (N : Node_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("%Instance%");
      Add_Nat_To_Name_Buffer (Nat (N));
      return Name_Find;
   end Build_Instance_Internal_Name;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance (N : Node_Id) return Node_Id is
      I_Name : constant Name_Id := Build_Instance_Internal_Name (N);
   begin
      return Node_Id (Get_Name_Table_Info (I_Name));
   end Get_Instance;

   ------------------
   -- Set_Instance --
   ------------------

   procedure Set_Instance (N : Node_Id; E : Node_Id) is
      I_Name : constant Name_Id := Build_Instance_Internal_Name (N);
   begin
      Set_Name_Table_Info (I_Name, Int (E));
   end Set_Instance;

end Ocarina.Instances;
