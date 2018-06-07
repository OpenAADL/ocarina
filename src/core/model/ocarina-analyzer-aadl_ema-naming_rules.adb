------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.ANALYZER.AADL_EMA.NAMING_RULES                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2016-2018 ESA & ISAE.                    --
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

with Ocarina.Namet;   use Ocarina.Namet;
with Ada.Text_IO;     use Ada.Text_IO;

with Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
with Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Analyzer.AADL_EMA.Finder;
with Ocarina.Analyzer.AADL_EMA.Links;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Analyzer.AADL_EMA.Naming_Rules is

   use Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
   use Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
   use Ocarina.Analyzer.AADL_EMA.Finder;
   use Ocarina.Analyzer.AADL_EMA.Links;

   package EMATN renames Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   function Check_All_Error_Type_Library_List
     (Root : Node_Id;
      EMA_Root : Node_Id;
      Package_Spec : Node_Id := No_Node)
     return Boolean;
   function Check_Error_Type_Library_Extended
      (AADL_Root           : Node_Id;
       EMA_Root            : Node_Id;
       Error_Type_Local_Id : Node_Id)
     return Boolean;
   function Check_Conflict_Local_Identifiers_Extends_Clause
     (AADL_Root           : Node_Id;
      List_Extended       : List_Id;
      Error_Type_Local_Id : Node_Id)
     return Boolean;
   procedure Check_Error_Type_Library_List
     (Error_Type_Library_List   : List_Id;
      Root                      : Node_Id;
      Package_Spec              : Node_Id;
      Test_Renamed_Package      : Boolean := False;
      Package_Renamed_First     : in out Node_Id;
      Package_Renamed_Last      : in out Node_Id;
      Package_Spec_First        : in out Node_Id;
      Package_Spec_Last         : in out Node_Id;
      Check_Renamed_Package     : Boolean := False;
      Test_Exist_Error_Type_Lib : Boolean;
      Final_Result              : out Boolean);
   function Test_Extended_Library_Redundancy
      (Error_Model           : Node_Id;
       Package_Spec_First    : Node_Id;
       Package_Renamed_First : Node_Id)
      return Boolean;
   function Exist_Of_Error_Type_Library
      (Root         : Node_Id;
       Package_Spec : Node_Id)
     return Boolean;
   function Check_Identifiers_Error_Type_Library_List
        (List_Used     : List_Id;
         List_Extended : List_Id) return Boolean;
   procedure Affiche (Pckg_Name : Node_Id;
                      Is_AADL   : Boolean);
   procedure Check_Unique_Identifier_Error_Type_Library_Element
      (EMA_Root      : Node_Id;
       Id_First_Node : in out Node_Id;
       Id_Last_Node  : in out Node_Id;
       Success       : out Boolean);
   function Check_Unique_Identifier
      (First_Node_List : Node_Id)
      return Boolean;
   function Check_All_Error_Type_Reference
      (AADL_Root    : Node_Id;
       EMA_Root     : Node_Id;
       Package_Spec : Node_Id)
      return Boolean;
   function Check_Error_Type_Reference_Of_Error_Type_Library
      (AADL_Root     : Node_Id;
       EMA_Root      : Node_Id;
       Package_Spec  : Node_Id)
      return Boolean;
   function Check_Error_Type_Set_Reference_Of_Error_Type_Library
      (AADL_Root     : Node_Id;
       EMA_Root      : Node_Id;
       Package_Spec  : Node_Id)
      return Boolean;
   function Check_Referenced_Error_Type_Common_Function
      (AADL_Root          : Node_Id;
       EMA_Root           : Node_Id;
       Parent_Nodes_First : Node_Id;
       Package_Spec       : Node_Id;
       Is_Set             : Boolean)
      return Boolean;
   function Check_Error_Type_Reference
       (AADL_Root               : Node_Id;
        Package_Container       : Node_Id := No_Node;
        Parent_Node             : Node_Id;
        List_Used               : List_Id;
        Is_Set                  : Boolean;
        Error_Type_Library_Node : Node_Id := No_Node)
       return Boolean;
   function Test_Local_Reference_Error_Type_Library
       (Error_Type_Library_Node : Node_Id;
        Parent_Node             : Node_Id;
        Reference_Identifier    : Node_Id;
        Is_Set                  : Boolean)
      return Boolean;
   function Compare_Reference_With_Local
      (Parent_Node_References : Node_Id;
       Id                     : Node_Id;
       Parent_Node            : Node_Id;
       Reference_Identifier   : Node_Id)
     return Boolean;
   function Check_All_Error_Type_Set_Reference
      (AADL_Root    : Node_Id;
       EMA_Root     : Node_Id;
       Package_Spec : Node_Id)
      return Boolean;
   procedure Search_Reference_In_List
     (Id              : Node_Id;
      List_First_Node : Node_Id;
      Node_Referenced : out Node_Id);
   function Check_Referenced_Error_Type_In_Package
         (AADL_Root     : Node_Id;
          Pckg_Spec     : Node_Id;
          Error_Type_Id : Node_Id;
          Is_Set        : Boolean)
      return Boolean;
   function Search_Reference_In_List_Node
     (Id              : Node_Id;
      List_First_Node : Node_Id)
     return Boolean;

   ---------------------------------------
   -- Check_All_Error_Type_Library_List --
   ---------------------------------------

   --  In error_model_library :
   --  the node error_type_library cannot use or extend
   --  the same package
   --  but other nodes in error_model_library can use types in the
   --  same_package

   function Check_All_Error_Type_Library_List
     (Root : Node_Id;
      EMA_Root : Node_Id;
      Package_Spec : Node_Id := No_Node)
     return Boolean
   is
      pragma Assert
        (Kind (EMA_Root) = K_Annex_Library
         or else Kind (EMA_Root) = K_Annex_Subclause);

      Parent : Node_Id;
      List : List_Id;
      List_Used : List_Id;
      List_Extended : List_Id;
      Success : Boolean := True;

      Sub_Node : Node_Id := No_Node;
      Sub_List : List_Id := No_List;
      Use_Error_Types_Node : Node_Id;

      Package_Renamed_First : Node_Id := No_Node;
      Package_Renamed_Last : Node_Id := No_Node;
      Package_Spec_First : Node_Id := No_Node;
      Package_Spec_Last : Node_Id := No_Node;
      Final_Result : Boolean;
      Check_Renamed_Package : Boolean := False;
      Test_Renamed_Package : Boolean := False;
      Test_Exist_Error_Type_Lib : Boolean := True;
   begin
      case Kind (EMA_Root) is

         when K_Annex_Library =>
            --  K_Annex_Library
            --     Check if packages are correct
            Parent := Error_Type_Library (EMA_Root);

            if Present (Parent) then
               List_Used := Error_Type_Library_List_Used (Parent);
               List_Extended := Error_Type_Library_List_Extended (Parent);

               if not Is_Empty (List_Used) and then
                  not Is_Empty (List_Extended)
               then
                  Test_Renamed_Package := True;
               end if;

               Check_Error_Type_Library_List
               (List_Used, Root, Package_Spec,
                Test_Renamed_Package,
                Package_Renamed_First,
                Package_Renamed_Last,
                Package_Spec_First,
                Package_Spec_Last,
                False,
                True,
                Final_Result);
               Success := Final_Result;

               if Test_Renamed_Package then
                  Check_Renamed_Package := True;
               else
                  Check_Renamed_Package := False;
               end if;

               Test_Exist_Error_Type_Lib := False;

               Check_Error_Type_Library_List
               (List_Extended, Root, Package_Spec,
                Test_Renamed_Package,
                Package_Renamed_First,
                Package_Renamed_Last,
                Package_Spec_First,
                Package_Spec_Last,
                Check_Renamed_Package,
                Test_Exist_Error_Type_Lib,
                Final_Result);
               Success := Success and then Final_Result;

               --     Check redundant packages
               Success := Success and then
                          Check_Identifiers_Error_Type_Library_List
                          (List_Used, List_Extended);

            end if;

            --  Initialize variables
            Test_Renamed_Package := False;
            Check_Renamed_Package := False;
            Test_Exist_Error_Type_Lib := True;

            --  K_Error_Behavior_State_Machine
            Sub_List := Error_Behavior_State_Machine_List (EMA_Root);
            if not Is_Empty (Sub_List) then
               Sub_Node := First_Node (Sub_List);
            end if;

            while Present (Sub_Node) loop
               List := Error_Type_Library_List (Sub_Node);
               Check_Error_Type_Library_List
               (List, Root, No_Node,
                Test_Renamed_Package,
                Package_Renamed_First,
                Package_Renamed_Last,
                Package_Spec_First,
                Package_Spec_Last,
                Check_Renamed_Package,
                Test_Exist_Error_Type_Lib,
                Final_Result);
               Success := Success and then Final_Result;
               Sub_Node := Next_Node (Sub_Node);
            end loop;

            --  K_Error_Type_Mappings
            Sub_List := Error_Type_Mappings_List (EMA_Root);
            if not Is_Empty (Sub_List) then
               Sub_Node := First_Node (Sub_List);
            end if;

            while Present (Sub_Node) loop
               List := Error_Type_Library_List (Sub_Node);
               Check_Error_Type_Library_List
               (List, Root, No_Node,
                Test_Renamed_Package,
                Package_Renamed_First,
                Package_Renamed_Last,
                Package_Spec_First,
                Package_Spec_Last,
                Check_Renamed_Package,
                Test_Exist_Error_Type_Lib,
                Final_Result);
               Success := Success and then Final_Result;
               Sub_Node := Next_Node (Sub_Node);
            end loop;

            --  K_Use_Error_Types
            Sub_List := Error_Type_Transformations_List (EMA_Root);
            if not Is_Empty (Sub_List) then
               Sub_Node := First_Node (Sub_List);
            end if;

            while Present (Sub_Node) loop
               Use_Error_Types_Node := Use_Error_Types (Sub_Node);
               if Present (Use_Error_Types_Node) then
                  List := Error_Type_Library_List (Use_Error_Types_Node);
                  Check_Error_Type_Library_List
                  (List, Root, No_Node,
                   Test_Renamed_Package,
                   Package_Renamed_First,
                   Package_Renamed_Last,
                   Package_Spec_First,
                   Package_Spec_Last,
                   Check_Renamed_Package,
                   Test_Exist_Error_Type_Lib,
                   Final_Result);
                  Success := Success and then Final_Result;
               end if;
               Sub_Node := Next_Node (Sub_Node);
            end loop;

         when others =>
            --  K_Annex_Subclause
            List := Error_Type_Library_List (EMA_Root);
            Check_Error_Type_Library_List
            (List, Root, No_Node,
             Test_Renamed_Package,
             Package_Renamed_First,
             Package_Renamed_Last,
             Package_Spec_First,
             Package_Spec_Last,
             Check_Renamed_Package,
             Test_Exist_Error_Type_Lib,
             Final_Result);
            Success := Success and then Final_Result;
      end case;

      return Success;
   end Check_All_Error_Type_Library_List;

   ---------------------------------------
   -- Check_Error_Type_Library_Extended --
   ---------------------------------------

   function Check_Error_Type_Library_Extended
      (AADL_Root           : Node_Id;
       EMA_Root            : Node_Id;
       Error_Type_Local_Id : Node_Id)
     return Boolean
   is
      pragma Assert
        (Kind (EMA_Root) = K_Annex_Library);

      List_Extended : List_Id;
      Parent : Node_Id;
      Error_Model : Node_Id;
      Package_Spec : Node_Id;
      Error_Type_Library_Node : Node_Id;
      Error_Type_Extends_Id : Node_List;

      Success : Boolean := True;
   begin
      Parent := Error_Type_Library (EMA_Root);

      if Present (Parent) then
         List_Extended := Error_Type_Library_List_Extended (Parent);

         if Is_Empty (List_Extended) then
            return True;
         else
            if not
            Is_Empty (Error_Type_Library_Element_List (Parent))
            then
               --  Check if there is a conflict of identifiers
               --  with the new error_type declared and others in
               --  the package mentioned in extends
               Success := Check_Conflict_Local_Identifiers_Extends_Clause
                  (AADL_Root, List_Extended, Error_Type_Local_Id);
               --  it is useless to link the packages with the new
               --  error_types defined because there are never referenced
               --  with that package
            else
               Success := True;
            end if;

            --  The occurrence of each identifier in each package
            --  of List_Extended must be : one
            Error_Model := First_Node (List_Extended);
            while Present (Error_Model) loop
               Package_Spec := AADL_Package_Reference (Error_Model);
               Error_Type_Library_Node := Search_Package_Annex_Root
               (AADL_Root, Package_Spec);

               if No (Error_Type_Library_Node) or else
                  Is_Empty (Error_Type_Library_Element_List
                  (Error_Type_Library_Node))
               then
                  Success := Success and then True;
               else
                  Find_Error_Type_Library_Element
                   (Error_Type_Library_Node,
                    Error_Type_Extends_Id.First,
                    Error_Type_Extends_Id.Last);
               end if;

               Error_Model := Next_Node (Error_Model);
            end loop;

            if Present (Error_Type_Extends_Id.First) then
               Success := Success and then Check_Unique_Identifier
                          (Error_Type_Extends_Id.First);
            end if;

         end if;

      end if;

      return Success;
   end Check_Error_Type_Library_Extended;

   -----------------------------------------------------
   -- Check_Conflict_Local_Identifiers_Extends_Clause --
   -----------------------------------------------------

   --  Test if there is any conflict with local error_types identifiers

   function Check_Conflict_Local_Identifiers_Extends_Clause
     (AADL_Root           : Node_Id;
      List_Extended       : List_Id;
      Error_Type_Local_Id : Node_Id)
     return Boolean
   is
      Element_Id : Node_Id;
      Error_Model : Node_Id;
      Local_Error_Type_Extends : Node_List;
      Error_Type_Library_Node : Node_Id;
      Id : Node_Id;
      Node : Node_Id;
      Package_Spec : Node_Id;

      Success : Boolean := True;
   begin
      Element_Id := Error_Type_Local_Id;
      Error_Model := First_Node (List_Extended);
      while Present (Error_Model) loop
         --  Search local identifiers for the package referenced in
         --  error_model_library
         Package_Spec := AADL_Package_Reference (Error_Model);
         Error_Type_Library_Node := Search_Package_Annex_Root
           (AADL_Root, Package_Spec);

         if No (Error_Type_Library_Node) or else
            Is_Empty (Error_Type_Library_Element_List
            (Error_Type_Library_Node))
         then
            return True;
         end if;

         Find_Error_Type_Library_Element
             (Error_Type_Library_Node,
              Local_Error_Type_Extends.First,
              Local_Error_Type_Extends.Last);

         Id := Local_Error_Type_Extends.First;
         Node := Id;

         --  Compare identifiers to verify conflicts
         while Present (Element_Id) loop

            Id := Node;

            while Present (Id) loop
               if Get_Name_String (Name (Id)) =
                  Get_Name_String (Name (Element_Id))
               then
                  Put ("Conflict error_type with local error_type (" &
                  Get_Name_String (Name (Id)) &
                  ") in ");
                  Affiche (Package_Spec, True);
                  Success := False;
               end if;
               Id := Next_Node (Id);
            end loop;

            Element_Id := Next_Node (Element_Id);
         end loop;

         Error_Model := Next_Node (Error_Model);
      end loop;

      return Success;
   end Check_Conflict_Local_Identifiers_Extends_Clause;

   -------------
   -- Affiche --
   -------------

   procedure Affiche (Pckg_Name : Node_Id;
                      Is_AADL   : Boolean)
   is
      Identifier_Node : Node_Id;
      Pckg : Node_Id;
   begin
      if Is_AADL then
         Pckg := ATN.Package_Name (Pckg_Name);
         Identifier_Node := ATN.First_Node
                  (ATN.Identifiers (Pckg));
         while Present (Identifier_Node) loop
            Put (Get_Name_String (ATN.Name (Identifier_Node)));
            Identifier_Node := ATN.Next_Node
                  (Identifier_Node);
            if Present (Identifier_Node) then
               Put ("::");
            else
               Put (" ");
            end if;
         end loop;
         New_Line;
      else
         Identifier_Node := EMATN.First_Node
                  (EMATN.Identifiers (Pckg_Name));
         while Present (Identifier_Node) loop
            Put (Get_Name_String (EMATN.Name (Identifier_Node)));
            Identifier_Node := EMATN.Next_Node
                  (Identifier_Node);
            if Present (Identifier_Node) then
               Put ("::");
            else
               Put (" ");
            end if;
         end loop;
      end if;
   end Affiche;

   -----------------------------------
   -- Check_Error_Type_Library_List --
   -----------------------------------

   procedure Check_Error_Type_Library_List
     (Error_Type_Library_List   : List_Id;
      Root                      : Node_Id;
      Package_Spec              : Node_Id;
      Test_Renamed_Package      : Boolean := False;
      Package_Renamed_First     : in out Node_Id;
      Package_Renamed_Last      : in out Node_Id;
      Package_Spec_First        : in out Node_Id;
      Package_Spec_Last         : in out Node_Id;
      Check_Renamed_Package     : Boolean := False;
      Test_Exist_Error_Type_Lib : Boolean;
      Final_Result              : out Boolean)
   is

      Success : Boolean := False;
      EMLR : Node_List;
      Error_Model : Node_Id;
      AADL_Package_Referenced : Node_Id;

      Identifier_Node : Node_Id;
      Pckg_Originale_Name : Node_Id;

      Test_Error_Type_Library : Boolean;
      Test : Boolean := True;

      Not_Allowed_Reference_Itself : Boolean;
   begin
      Final_Result := True;
      if Is_Empty (Error_Type_Library_List) then
         return;
      end if;

      Select_Nodes (Error_Type_Library_List,
                    (1 => K_Error_Model_Library_Reference),
                     EMLR.First,
                     EMLR.Last);
      Error_Model := EMLR.First;

      while Present (Error_Model) loop
         Test_With_Package_Name_Alias
            (Root, Error_Model, Package_Spec,
             Success, AADL_Package_Referenced,
             Identifier_Node, Pckg_Originale_Name,
             Not_Allowed_Reference_Itself);
         if not Success then
            if Not_Allowed_Reference_Itself then
               Put_Line ("Cannot use types or extend the package itself in" &
                 " error_type_library");
            else
               Affiche (Error_Model, False);
               Put ("is not a package or alias or does not contain" &
                 " error_model_library");
               if Present (Package_Spec) then
                  Put (" in ");
                  Affiche (Package_Spec, True);
               else
                  New_Line;
               end if;
            end if;
            Final_Result := False;
         else
            --  Test if the referenced package contains error_type_library
            if Test_Exist_Error_Type_Lib then
               Test_Error_Type_Library :=
                   Exist_Of_Error_Type_Library (Root, AADL_Package_Referenced);
               if not Test_Error_Type_Library then
                  Final_Result := False;
               end if;
            end if;

            --  Link between the package and error_model_library_reference
            Link_Error_Type_Library_List
                (AADL_Package_Referenced, Error_Model);

            --  FIXME : if use types clause call the same package
            --  with the renamed package and the original name
            --  it is accepted (without warning)
            if Test_Renamed_Package then
               if Present (Identifier_Node) and then
                  Present (Pckg_Originale_Name)
               then

                  --  Preseve in 2 list_node : package_renamed
                  --  and package original name
                  Package_Spec_First := No_Node;
                  Package_Renamed_First := No_Node;
                  Ocarina.Analyzer.AADL_EMA.Finder.Put_In_A_List_Node
                  (Package_Renamed_First,
                   Package_Renamed_Last,
                   Identifier_Node);
                  Ocarina.Analyzer.AADL_EMA.Finder.Put_In_A_List_Node
                  (Package_Spec_First,
                   Package_Spec_Last,
                   AADL_Package_Referenced);

               end if;
            end if;

            --  Test the library error_type_library extended
            if Check_Renamed_Package then

               Test := Test_Extended_Library_Redundancy
                       (Error_Model,
                        Package_Spec_First,
                        Package_Renamed_First);
               if Test then
                  Final_Result := False;
               end if;
            end if;

         end if;

         Error_Model := EMATN.Next_Node (Error_Model);
      end loop;

   end Check_Error_Type_Library_List;

   --------------------------------------
   -- Test_Extended_Library_Redundancy --
   --------------------------------------

   function Test_Extended_Library_Redundancy
      (Error_Model           : Node_Id;
       Package_Spec_First    : Node_Id;
       Package_Renamed_First : Node_Id)
      return Boolean
   is
      Package_Spec : Node_Id;
      Pckg_Name : Node_Id;
      Id : Node_Id;
      Id_Spec : Node_Id;
      Package_Renamed : Node_Id;

      Redundant : Boolean := False;
   begin
      if No (Package_Spec_First) or else No (Package_Renamed_First)
      then
         return False;
      end if;

      --  Test if we are using the renamed package
      Package_Renamed := Package_Renamed_First;
      while Present (Package_Renamed) loop
         Id := EMATN.First_Node (EMATN.Identifiers (Error_Model));
         if Get_Name_String (ATN.Name (Package_Renamed)) =
            Get_Name_String (EMATN.Name (Id)) and then
            No (EMATN.Next_Node (Id))
         then
            Put_Line ("Conflict with packages in use types" &
            " and extends clause : use of the new name of the package" &
            " and the original name of the package");

            return True;
         end if;
         Package_Renamed := ATN.Next_Node (Package_Renamed);
      end loop;

      --  Test if we are using the original name of the package
      Package_Spec := Package_Spec_First;
      while Present (Package_Spec) loop
         Pckg_Name := ATN.Package_Name (Package_Spec);

         Redundant := False;

         --  Test the length of both lists of identifiers
         if Ocarina.ME_AADL.AADL_Tree.Nutils.Length
            (ATN.Identifiers (Pckg_Name)) =
            Ocarina.ME_AADL_EMA.EMA_Tree.Nutils.Length
            (EMATN.Identifiers (Error_Model))
         then
            Id_Spec := ATN.First_Node (ATN.Identifiers (Pckg_Name));
            Id := EMATN.First_Node (EMATN.Identifiers (Error_Model));

            while Present (Id_Spec) and then
                  Present (Id)
            loop
               if Get_Name_String (ATN.Name (Id_Spec)) /=
                  Get_Name_String (EMATN.Name (Id))
               then
                  exit;
               end if;

               Id_Spec := ATN.Next_Node (Id_Spec);
               Id := EMATN.Next_Node (Id);

               if No (Id) and then No (Id_Spec)
               then
                  Redundant := True;
                  Put_Line ("Conflict with packages in use types" &
                  " and extends clause :");
                  Put_Line ("use of the new name of the package" &
                  " and the original name of the package");
               end if;
            end loop;

         end if;

         exit when Redundant;
         Package_Spec := ATN.Next_Node (Package_Spec);
      end loop;

      return Redundant;
   end Test_Extended_Library_Redundancy;

   ---------------------------------
   -- Exist_Of_Error_Type_Library --
   ---------------------------------

   function Exist_Of_Error_Type_Library
      (Root         : Node_Id;
       Package_Spec : Node_Id)
     return Boolean
   is
      Error_Type_Library_Node : Node_Id;
      Success : Boolean := True;
   begin
      Error_Type_Library_Node := Search_Package_Annex_Root
                                 (Root, Package_Spec);
      if No (Error_Type_Library_Node) then
         Success := False;
         Put ("Useless use of: ");
         Affiche (Package_Spec, True);
         Put_Line (" the package does not contain error_type_library");
      end if;

      return Success;
   end Exist_Of_Error_Type_Library;

   -----------------------------------------------
   -- Check_Identifiers_Error_Type_Library_List --
   -----------------------------------------------

   function Check_Identifiers_Error_Type_Library_List
        (List_Used     : List_Id;
         List_Extended : List_Id) return Boolean
   is
      Node_Used : Node_Id;
      Identifier_Used : Node_Id;

      Node_Extended : Node_Id;
      Identifier_Extended : Node_Id;

      Are_Identical : Boolean := False;
      Success : Boolean := True;
   begin
      if Is_Empty (List_Used) or else Is_Empty (List_Extended)
      then
         return True;
      end if;

      Node_Used := First_Node (List_Used);
      Node_Extended := First_Node (List_Extended);
      while Present (Node_Used) loop

         Are_Identical := False;

         while Present (Node_Extended) loop
            --  Test the length of both lists of identifiers
            if Length (Identifiers (Node_Used)) =
               Length (Identifiers (Node_Extended))
            then
               Identifier_Used := First_Node (Identifiers (Node_Used));
               Identifier_Extended := First_Node (Identifiers (Node_Extended));

               while Present (Identifier_Used) and then
                     Present (Identifier_Extended)
                  loop
                     if Get_Name_String (Name (Identifier_Used)) /=
                        Get_Name_String (Name (Identifier_Extended))
                     then
                        exit;
                     end if;

                     Identifier_Used := Next_Node (Identifier_Used);
                     Identifier_Extended := Next_Node (Identifier_Extended);

                     if No (Identifier_Used) and then No (Identifier_Extended)
                     then
                        Are_Identical := True;
                        Put_Line ("Erreur : Conflict with packages in use " &
                             "types and extends clause");
                        Success := False;
                     end if;
               end loop;

            end if;

            exit when Are_Identical;
            Node_Extended := Next_Node (Node_Extended);
         end loop;

         Node_Used := Next_Node (Node_Used);
      end loop;

      return Success;
   end Check_Identifiers_Error_Type_Library_List;

   --------------------------------------------------------
   -- Check_Unique_Identifier_Error_Type_Library_Element --
   --------------------------------------------------------

   --  Id_First_Node and Id_Last_Node are used for ulterior motive :
   --  compare these identifiers with others from extended
   --  packages

   procedure Check_Unique_Identifier_Error_Type_Library_Element
      (EMA_Root      : Node_Id;
       Id_First_Node : in out Node_Id;
       Id_Last_Node  : in out Node_Id;
       Success       : out Boolean)
   is
      Error_Type_Library_Node : Node_Id;

      Element_List : List_Id;
      Element_Node : Node_Id;

      ET_Definition : Node_Id;
      ET_Alias : Node_Id;
      ET_Set_Definition : Node_Id;
      ET_Set_Alias : Node_Id;

      Id_First_Node_1 : Node_Id := No_Node;
      Id_Last_Node_1  : Node_Id := No_Node;
      Id_First_Node_2 : Node_Id := No_Node;
      Id_Last_Node_2  : Node_Id := No_Node;
      Id_First_Node_3 : Node_Id := No_Node;
      Id_Last_Node_3  : Node_Id := No_Node;
      Id_First_Node_4 : Node_Id := No_Node;
      Id_Last_Node_4  : Node_Id := No_Node;
   begin
      Error_Type_Library_Node := Error_Type_Library (EMA_Root);
      if No (Error_Type_Library_Node) then
         Success := True;
         return;
      end if;

      Element_List := Error_Type_Library_Element_List
                      (Error_Type_Library_Node);
      if Is_Empty (Element_List) then
         Success := True;
         return;
      else
         Element_Node := First_Node (Element_List);
         while Present (Element_Node) loop

            --  Test identifiers of Error_Type_Definition
            ET_Definition := Error_Type_Definition (Element_Node);
            Put_In_A_List_Node (Id_First_Node_1,
                                Id_Last_Node_1,
                                ET_Definition,
                                True);
            Put_In_A_List_Node (Id_First_Node,
                                Id_Last_Node,
                                ET_Definition,
                                True);

            --  Test identifiers of Error_Type_Alias
            ET_Alias := Error_Type_Alias (Element_Node);
            Put_In_A_List_Node (Id_First_Node_2,
                                Id_Last_Node_2,
                                ET_Alias,
                                True);
            Put_In_A_List_Node (Id_First_Node,
                                Id_Last_Node,
                                ET_Alias,
                                True);

            --  Test identifiers of Error_Type_Set_Definition
            ET_Set_Definition := Error_Type_Set_Definition (Element_Node);
            Put_In_A_List_Node (Id_First_Node_3,
                                Id_Last_Node_3,
                                ET_Set_Definition,
                                True);
            Put_In_A_List_Node (Id_First_Node,
                                Id_Last_Node,
                                ET_Set_Definition,
                                True);

            --  Test identifiers of Error_Type_Set_Alias
            ET_Set_Alias := Error_Type_Set_Alias (Element_Node);
            Put_In_A_List_Node (Id_First_Node_4,
                                Id_Last_Node_4,
                                ET_Set_Alias,
                                True);
            Put_In_A_List_Node (Id_First_Node,
                                Id_Last_Node,
                                ET_Set_Alias,
                                True);

            Element_Node := Next_Node (Element_Node);
         end loop;
      end if;

      Success := Check_Unique_Identifier
                    (Id_First_Node_1);
      Success := Success and then Check_Unique_Identifier
                    (Id_First_Node_2);
      Success := Success and then Check_Unique_Identifier
                    (Id_First_Node_3);
      Success := Success and then Check_Unique_Identifier
                    (Id_First_Node_4);

      if not Success then
         Id_First_Node := No_Node;
         Id_Last_Node := No_Node;
      end if;

   end Check_Unique_Identifier_Error_Type_Library_Element;

   ------------------------
   -- Put_In_A_List_Node --
   ------------------------

   procedure Put_In_A_List_Node
      (Id_First_Node : in out Node_Id;
       Id_Last_Node  : in out Node_Id;
       Parent_Node   : Node_Id;
       Is_Identifier : Boolean := False)
   is
      Node : Node_Id;
   begin
      if Present (Parent_Node) then

         if Is_Identifier then
            Node := Identifier (Parent_Node);
         else
            Node := Parent_Node;
         end if;

         if No (Id_First_Node) then
            Id_First_Node := Node;
            Id_Last_Node := Node;
         else
            Set_Next_Node (Id_Last_Node, Node);
            Set_Next_Node (Node, No_Node);
            Id_Last_Node := Node;
         end if;

      end if;
   end Put_In_A_List_Node;

   -----------------------------
   -- Check_Unique_Identifier --
   -----------------------------

   function Check_Unique_Identifier
      (First_Node_List : Node_Id)
      return Boolean
   is
      --  Id is the identifier that we compare with
      Id : Node_Id;
      --  Next is the rest of identifiers in the node list
      Next : Node_Id;

      Success : Boolean := True;
   begin
      if Present (First_Node_List) then

         Id := First_Node_List;
         while Present (Id) loop
            Next := Next_Node (Id);
            while Present (Next) loop
               if Get_Name_String (Name (Id)) =
                  Get_Name_String (Name (Next))
               then
                  Put_Line ("Duplicate identifier : " &
                            Get_Name_String (Name (Id)));
                  Success := False;
                  exit;
               end if;
               Next := Next_Node (Next);
            end loop;
            Id := Next_Node (Id);
         end loop;

      else
         return True;
      end if;

      return Success;
   end Check_Unique_Identifier;

   ------------------------------------
   -- Check_All_Error_Type_Reference --
   ------------------------------------

   function Check_All_Error_Type_Reference
      (AADL_Root    : Node_Id;
       EMA_Root     : Node_Id;
       Package_Spec : Node_Id)
      return Boolean
   is
      Success : Boolean;
   begin
      Success := Check_Error_Type_Reference_Of_Error_Type_Library
                 (AADL_Root, EMA_Root, Package_Spec);

      --  add other nodes test's about Error_Type_Reference
      --  it is exactly the same as Error_Type_Library

      return Success;
   end Check_All_Error_Type_Reference;

   ------------------------------------------------------
   -- Check_Error_Type_Reference_Of_Error_Type_Library --
   ------------------------------------------------------

   function Check_Error_Type_Reference_Of_Error_Type_Library
      (AADL_Root     : Node_Id;
       EMA_Root      : Node_Id;
       Package_Spec  : Node_Id)
      return Boolean
   is
      Parent_Nodes : Node_List;
      Success : Boolean;
   begin
      Get_Error_Type_Reference_Of_Error_Type_Library
         (EMA_Root, Parent_Nodes.First, Parent_Nodes.Last);

      Success := Check_Referenced_Error_Type_Common_Function
        (AADL_Root, EMA_Root, Parent_Nodes.First, Package_Spec,
         False);

      return Success;

   end Check_Error_Type_Reference_Of_Error_Type_Library;

   ----------------------------------------------------------
   -- Check_Error_Type_Set_Reference_Of_Error_Type_Library --
   ----------------------------------------------------------

   function Check_Error_Type_Set_Reference_Of_Error_Type_Library
      (AADL_Root     : Node_Id;
       EMA_Root      : Node_Id;
       Package_Spec  : Node_Id)
      return Boolean
   is
      Parent_Nodes : Node_List;
      Success : Boolean;
   begin
      Get_Error_Type_Set_Reference_Of_Error_Type_Library
         (EMA_Root, Parent_Nodes.First, Parent_Nodes.Last);

      Success := Check_Referenced_Error_Type_Common_Function
        (AADL_Root, EMA_Root, Parent_Nodes.First, Package_Spec,
         True);

      return Success;
   end Check_Error_Type_Set_Reference_Of_Error_Type_Library;

   -------------------------------------------------
   -- Check_Referenced_Error_Type_Common_Function --
   -------------------------------------------------

   function Check_Referenced_Error_Type_Common_Function
      (AADL_Root          : Node_Id;
       EMA_Root           : Node_Id;
       Parent_Nodes_First : Node_Id;
       Package_Spec       : Node_Id;
       Is_Set             : Boolean)
      return Boolean
   is
      Success : Boolean := True;

      Parent_Node : Node_Id;

      Error_Type_Library_Node : Node_Id;
      List_Used : List_Id;
   begin
      --  Search for List_Used
      Error_Type_Library_Node := Error_Type_Library (EMA_Root);
      if No (Error_Type_Library_Node) then
         return True;
      end if;

      List_Used := Error_Type_Library_List_Used
                   (Error_Type_Library_Node);

      Parent_Node := Parent_Nodes_First;
      --  initialize Success
      Success := Check_Error_Type_Reference
                 (AADL_Root, Package_Spec,
                  Parent_Node, List_Used,
                  Is_Set,
                  Error_Type_Library_Node);
      if Present (Parent_Node) then
         Parent_Node := EMATN.Next_Node (Parent_Node);
         while Present (Parent_Node) loop
            Success := Success and then
                Check_Error_Type_Reference
                (AADL_Root, Package_Spec,
                 Parent_Node, List_Used,
                 Is_Set,
                 Error_Type_Library_Node);
            Parent_Node := EMATN.Next_Node (Parent_Node);
         end loop;
      end if;

      return Success;

   end Check_Referenced_Error_Type_Common_Function;

   --------------------------------
   -- Check_Error_Type_Reference --
   --------------------------------

   --  Package referenced exists in use types clause only

   --  error_types , alias ... in extends clause become accessible
   --  without the package name : as local identifiers
   --  ( wich explains : no conflict with other extended libraries and
   --    local identifiers)

   --  In error_type_library we have to check if the new error_type_identifier
   --  or the new error_type_set_identifier is not the same referenced
   --  at the same time

   --  à revoir ce cas
   --  Error_Type_Product is List_Id others are Node_Id

   --  Is_Set : we are using the same function to check Error_Type_Reference
   --  and Error_Type_Set_Reference (same treatments are done for both with
   --  little differences)
   --  Is_Set = True => Error_Type_Set_Reference

   --  If we are testing the references in Error_Type_Library then
   --  Error_Type_Library_Node <> No_Node

   function Check_Error_Type_Reference
       (AADL_Root               : Node_Id;
        Package_Container       : Node_Id := No_Node;
        Parent_Node             : Node_Id;
        List_Used               : List_Id;
        Is_Set                  : Boolean;
        Error_Type_Library_Node : Node_Id := No_Node)
       return Boolean
   is
      ETL_Ref : Node_Id;
      Error_Model_Library_Ref : Node_Id;
      Error_Type_Id : Node_Id;

      Exist_Package_Ref : Boolean := True;

      Not_Allowed_Reference : Boolean;

      Success : Boolean := True;

      Node_Referenced : Node_Id;

      Local_Identifiers : Node_List;
      Local_Id : Node_Id;
      Library_Used : Node_Id;
      Exist_Error_Type : Boolean := False;

      Choose_Node : Boolean := False;
   begin
      if No (Parent_Node) then
         return True;
      end if;

      if not Is_Set then
         if Kind (Parent_Node) = K_Error_Type_Reference then
            ETL_Ref := Parent_Node;
         else
            ETL_Ref := Error_Type_Reference (Parent_Node);
            if No (ETL_Ref) then
               return True;
            end if;
         end if;
      else
         ETL_Ref := Error_Type_Set_Reference (Parent_Node);
         if No (ETL_Ref) then
            return True;
         end if;
      end if;

      --  The parser does not do the difference between
      --  error_type_set_reference and error_type_reference in this node
      if Kind (Parent_Node) = K_Error_Type_Or_Set_Reference then
         Choose_Node := True;
      end if;

      --  Check existance of referenced element
      Error_Model_Library_Ref := Error_Model_Library_Reference (ETL_Ref);
      Error_Type_Id := Identifier (ETL_Ref);

      -----------------------------------
      --  A REVOIR : supprimer ce message
      New_Line;
      if Present (Error_Model_Library_Ref) then
         Put_Line ("1111111111 : " & Get_Name_String (Name
         (First_Node (Identifiers (Error_Model_Library_Ref)))));
      end if;
      Put_Line ("222222222 : " & Get_Name_String (Name (Error_Type_Id)));
      New_Line;
      -----------------------------------

      if Present (Error_Model_Library_Ref) then

         --  Check if the error_model_library_reference exists
         --  in use types clause
         if Is_Empty (List_Used) then
            Put_Line ("There is no package mentioned in use types clause");
            return False;
         end if;
         if not Is_Empty (List_Used) then
            Search_Reference_In_List (Error_Model_Library_Ref,
            First_Node (List_Used), Node_Referenced);
         end if;

         if No (Node_Referenced) then
            Put ("Error : The package ");
            Affiche (Error_Model_Library_Ref, False);
            Put_Line ("does not exist in use types or extends clause");
            return False;
         else
            --  Link the package found with the other one
            Link_Error_Type_Reference
              (Error_Model_Library_Ref, Node_Referenced);

            --  Check if the identifier is defined in the referenced
            --  package
            -------------------------------------
            --  à revoir : à éliminer ce message
            New_Line;
            Put_Line (" ~~~ " &
            "The package found is linked with aadl package ~~~");
            Put (Get_Name_String (ATN.Name (ATN.First_Node
            (ATN.Identifiers (ATN.Package_Name (AADL_Package_Reference
            (Node_Referenced)))))));
            Put_Line (" is the referenced package");
            Put_Line ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
            New_Line;
            -------------------------------------
            Success := Check_Referenced_Error_Type_In_Package
                       (AADL_Root,
                        AADL_Package_Reference (Node_Referenced),
                        Error_Type_Id,
                        Is_Set);
            if not Success then
               Put_Line ("The error type : " & Get_Name_String
               (Name (Error_Type_Id)) & " is not mentioned in the "
               & " package " & Get_Name_String (Name (First_Node
               (Identifiers (Node_Referenced)))));
               return False;
            end if;
         end if;

      else
         Exist_Package_Ref := False;
      end if;

      --  Check if the error type is local error_type
      --  (error_type_library_element / extended library)
      --  or is mentioned in the package of use types clause
      if not Exist_Package_Ref then
         --  Local identifiers
         Get_Error_Type_Id
         (AADL_Root,
          Package_Container,
          Local_Identifiers.First,
          Local_Identifiers.Last,
          Is_Set);
         Local_Id := Local_Identifiers.First;
         --  Search if the error_type is a local error_type
         if Present (Local_Id) then
            Exist_Error_Type := Search_Reference_In_List_Node
                  (Error_Type_Id, Local_Id);

            --  Test if the error_type or error_type_set referenced
            --  is the same as the new one defined : this test is only
            --  for error_type_library
            if Exist_Error_Type and then
               Present (Error_Type_Library_Node)
            then

               Not_Allowed_Reference := Test_Local_Reference_Error_Type_Library
                   (Error_Type_Library_Node, Parent_Node,
                    Error_Type_Id, Is_Set);

               if Not_Allowed_Reference then
                  Put_Line (Get_Name_String (Name (Error_Type_Id)) &
                  " cannot reference itself");
                  return False;
               end if;

            end if;
         end if;

         --  identifiers in use types
         if not Exist_Error_Type and then
            not Is_Empty (List_Used)
         then
            Local_Identifiers.First := No_Node;
            Library_Used := First_Node (List_Used);
            while Present (Library_Used) loop
               Get_Error_Type_Id
               (AADL_Root,
                AADL_Package_Reference (Library_Used),
                Local_Identifiers.First,
                Local_Identifiers.Last,
                Is_Set);
               Library_Used := Next_Node (Library_Used);
            end loop;
            Local_Id := Local_Identifiers.First;
            --  Search if the error_type is in a used type package
            if Present (Local_Id) then
               Exist_Error_Type := Search_Reference_In_List_Node
                  (Error_Type_Id, Local_Id);
            end if;
         end if;

         Success := Exist_Error_Type;
         if not Success then
            if Choose_Node then
               if not Is_Set then
                  --  Unset the node from error_type_reference
                  Set_Error_Type_Reference (Parent_Node, No_Node);
                  Success := True;
               else
                  --  Unset the node from error_type_set_reference
                  Set_Error_Type_Set_Reference (Parent_Node, No_Node);
                  --  Test if it is error_type_reference
                  if No (Error_Type_Reference (Parent_Node)) then
                     Put_Line (Get_Name_String (Name (Error_Type_Id)) &
                     " is nor error_type_set_reference neither " &
                     "error_type_reference");
                     Success := False;
                  else
                     Success := True;
                  end if;
               end if;

               return Success;
            end if;

            if Is_Set then
               Put_Line ("The error_type_set_reference ( " &
               Get_Name_String (Name (Error_Type_Id)) & " ) does not exist");
            else
               Put_Line ("The error_type_reference ( " & Get_Name_String
               (Name (Error_Type_Id)) & " ) does not exist");
            end if;
         else
            ----------------------------
            --  à revoir : à éliminer ce message
            if Is_Set then
               Put_Line (Get_Name_String
               (Name (Error_Type_Id)) & " (error_type_Set) is found");
            else
               Put_Line (Get_Name_String
               (Name (Error_Type_Id)) & " (error_type) is found");
            end if;
            --------------------------
         end if;

      end if;

      return Success;
   end Check_Error_Type_Reference;

   ---------------------------------------------
   -- Test_Local_Reference_Error_Type_Library --
   ---------------------------------------------

   --  This Test is only used if we are testing references
   --  in error_type_library

   function Test_Local_Reference_Error_Type_Library
       (Error_Type_Library_Node : Node_Id;
        Parent_Node             : Node_Id;
        Reference_Identifier    : Node_Id;
        Is_Set                  : Boolean)
      return Boolean
   is
      Defined_Parent_Node : Node_Id;
      New_Parents : Node_List;
      Parent_Node_References : Node_Id;
      Id : Node_Id;

      Error_Type_Set_List : List_Id;
      Type_Set_Elt : Node_Id;
      Parent_List : List_Id;

      Not_Allowed : Boolean;
   begin
      Find_Error_Type_Library_Element
       (Error_Type_Library_Node,
        New_Parents.First, New_Parents.Last,
        False);

      Defined_Parent_Node := New_Parents.First;

      while Present (Defined_Parent_Node) loop
         --  Search the identifier
         Id := Identifier (Defined_Parent_Node);
         --  Search the parent_node of the referenced error_type
         if Kind (Defined_Parent_Node) = K_Error_Type_Set_Definition then
            if Is_Set then
               ------
               Error_Type_Set_List := Error_Type_Set (Defined_Parent_Node);
               if not Is_Empty (Error_Type_Set_List) then
                  Type_Set_Elt := First_Node (Error_Type_Set_List);
                  while Present (Type_Set_Elt) loop
                     Parent_Node_References :=
                        Error_Type_Or_Set_Reference (Type_Set_Elt);

                     Not_Allowed := Compare_Reference_With_Local
                         (Parent_Node_References, Id,
                          Parent_Node, Reference_Identifier);

                     if Not_Allowed then
                        return True;
                     end if;

                     Type_Set_Elt := Next_Node (Type_Set_Elt);
                  end loop;
               end if;
               --------
            else
               ----
               Error_Type_Set_List := Error_Type_Set (Defined_Parent_Node);
               if not Is_Empty (Error_Type_Set_List) then
                  Type_Set_Elt := First_Node (Error_Type_Set_List);
                  while Present (Type_Set_Elt) loop
                     Parent_Node_References := Error_Type_Or_Set_Reference
                                               (Type_Set_Elt);

                     Not_Allowed := Compare_Reference_With_Local
                         (Parent_Node_References, Id,
                          Parent_Node, Reference_Identifier);

                     if Not_Allowed then
                        return True;
                     end if;

                     --  error_type_product
                     Parent_List := Error_Type_Product (Type_Set_Elt);
                     if not Is_Empty (Parent_List) then
                        Parent_Node_References := First_Node (Parent_List);
                        while Present (Parent_Node_References) loop
                           Not_Allowed := Compare_Reference_With_Local
                             (Parent_Node_References, Id,
                              Parent_Node, Reference_Identifier);

                           if Not_Allowed then
                              return True;
                           end if;
                           Parent_Node_References := Next_Node
                              (Parent_Node_References);
                        end loop;
                     end if;

                     Type_Set_Elt := Next_Node (Type_Set_Elt);
                  end loop;
               end if;
               ----
            end if;
         else
            Parent_Node_References := Defined_Parent_Node;
            Not_Allowed := Compare_Reference_With_Local
                (Parent_Node_References, Id,
                 Parent_Node, Reference_Identifier);
         end if;

         exit when Not_Allowed;
         Defined_Parent_Node := Next_Node (Defined_Parent_Node);

      end loop;

      return Not_Allowed;
   end Test_Local_Reference_Error_Type_Library;

   ----------------------------------
   -- Compare_Reference_With_Local --
   ----------------------------------

   function Compare_Reference_With_Local
      (Parent_Node_References : Node_Id;
       Id                     : Node_Id;
       Parent_Node            : Node_Id;
       Reference_Identifier   : Node_Id)
     return Boolean
   is
      Not_Allowed : Boolean := False;
   begin
      if No (Parent_Node_References) then
         return False;
      end if;

      if Parent_Node = Parent_Node_References and then
         Get_Name_String (Name (Id)) =
         Get_Name_String (Name (Reference_Identifier))
      then
         Not_Allowed := True;
      end if;

      return Not_Allowed;
   end Compare_Reference_With_Local;

   ----------------------------------------
   -- Check_All_Error_Type_Set_Reference --
   ----------------------------------------

   function Check_All_Error_Type_Set_Reference
      (AADL_Root    : Node_Id;
       EMA_Root     : Node_Id;
       Package_Spec : Node_Id)
      return Boolean
   is
      Success : Boolean;
   begin
      Success := Check_Error_Type_Set_Reference_Of_Error_Type_Library
                 (AADL_Root, EMA_Root, Package_Spec);

      --  add other nodes test's about Error_Type_Reference
      --  it is exactly the same as Error_Type_Library

      return Success;
   end Check_All_Error_Type_Set_Reference;

   --------------------------------------------
   -- Check_Referenced_Error_Type_In_Package --
   --------------------------------------------

   --  search in local error_type_identifiers of the package
   --  and extended error_type_identifiers

   function Check_Referenced_Error_Type_In_Package
         (AADL_Root     : Node_Id;
          Pckg_Spec     : Node_Id;
          Error_Type_Id : Node_Id;
          Is_Set        : Boolean)
      return Boolean
   is
      Identifiers_List : Node_List;
      Identifier_Node : Node_Id;
   begin
      Get_Error_Type_Id
           (AADL_Root, Pckg_Spec,
            Identifiers_List.First, Identifiers_List.Last,
            Is_Set);

      --  Compare error_type identifiers found with
      --  the error_type referenced
      Identifier_Node := Identifiers_List.First;
      while Present (Identifier_Node) loop
         if Get_Name_String (Name (Identifier_Node)) =
            Get_Name_String (Name (Error_Type_Id))
         then
            return True;
         end if;

         Identifier_Node := Next_Node (Identifier_Node);
      end loop;

      return False;
   end Check_Referenced_Error_Type_In_Package;

   ------------------------------
   -- Search_Reference_In_List --
   ------------------------------

   procedure Search_Reference_In_List
     (Id              : Node_Id;
      List_First_Node : Node_Id;
      Node_Referenced : out Node_Id)
   is
      Node : Node_Id;
      Identifier_Node_List : Node_Id;
      Identifier_Searched : Node_Id;

      Are_Identical : Boolean := False;
   begin
      Node_Referenced := No_Node;

      Node := List_First_Node;
      while Present (Node) loop

         Are_Identical := False;
         Node_Referenced := No_Node;

         --  Test the length of both lists of identifiers
         if Length (Identifiers (Node)) =
            Length (Identifiers (Id))
         then
            Identifier_Node_List := First_Node (Identifiers (Node));
            Identifier_Searched := First_Node (Identifiers (Id));

            while Present (Identifier_Node_List) and then
                  Present (Identifier_Searched)
            loop
               if Get_Name_String (Name (Identifier_Node_List)) /=
                  Get_Name_String (Name (Identifier_Searched))
               then
                  Node_Referenced := No_Node;
                  exit;
               else
                  Node_Referenced := Node;
               end if;

               Identifier_Node_List := Next_Node (Identifier_Node_List);
               Identifier_Searched := Next_Node (Identifier_Searched);

               if No (Identifier_Node_List) and then No (Identifier_Searched)
               then
                  Are_Identical := True;
               end if;
            end loop;

         end if;

         exit when Are_Identical;

         Node := Next_Node (Node);
      end loop;

   end Search_Reference_In_List;

   -----------------------------------
   -- Search_Reference_In_List_Node --
   -----------------------------------

   function Search_Reference_In_List_Node
     (Id              : Node_Id;
      List_First_Node : Node_Id)
     return Boolean
   is
      Node : Node_Id;

      Are_Identical : Boolean := False;
   begin
      Node := List_First_Node;

      while Present (Node) loop

         if Get_Name_String (Name (Id)) =
            Get_Name_String (Name (Node))
         then
            Are_Identical := True;
            exit;
         end if;

         Node := Next_Node (Node);
      end loop;

      return Are_Identical;
   end Search_Reference_In_List_Node;

   ----------------------------
   -- Check_Names_In_Library --
   ----------------------------

   function Check_Names_In_Library
     (Root : Node_Id;
      EMA_Root : Node_Id;
      Package_Spec : Node_Id)
     return Boolean is

      pragma Assert
             (Kind (EMA_Root) = K_Annex_Library);

      Success : Boolean := True;
      Test : Boolean;
      Id_Node : Node_List;

      EMA_Pckg_Container : Node_Id;
   begin
      Success := Check_All_Error_Type_Library_List
                 (Root, EMA_Root, Package_Spec);

      Check_Unique_Identifier_Error_Type_Library_Element
         (EMA_Root, Id_Node.First, Id_Node.Last, Test);
      Success := Success and then Test;

      Success := Success and then
                 Check_Error_Type_Library_Extended
                 (Root, EMA_Root, Id_Node.First);

      --  Search the pakage container of the annex
      EMA_Pckg_Container := Find_Package_Annex_Library
                 (Root, EMA_Root);

      Success := Success and then
         Check_All_Error_Type_Reference
         (Root, EMA_Root, EMA_Pckg_Container);

      Success := Success and then
         Check_All_Error_Type_Set_Reference
         (Root, EMA_Root, EMA_Pckg_Container);

      return Success;
   end Check_Names_In_Library;

   ------------------------------
   -- Check_Names_In_Subclause --
   ------------------------------

   function Check_Names_In_Subclause
     (Root : Node_Id;
      EMA_Root : Node_Id;
      Package_Spec : Node_Id)
     return Boolean is

      pragma Assert
             (Kind (EMA_Root) = K_Annex_Subclause);
   begin
      return Check_All_Error_Type_Library_List (Root, EMA_Root, Package_Spec);
   end Check_Names_In_Subclause;

end Ocarina.Analyzer.AADL_EMA.Naming_Rules;
