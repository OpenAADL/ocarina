------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . A N A L Y Z E R . A A D L _ E M A . F I N D E R      --
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

with Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL_EMA.EMA_Tokens;
with Ocarina.Analyzer.AADL_EMA.Naming_Rules;

with Utils;
with Ocarina.Namet;

package body Ocarina.Analyzer.AADL_EMA.Finder is

   use Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
   use Ocarina.ME_AADL_EMA.EMA_Tree.Nutils;
   use Ocarina.Analyzer.AADL.Finder;

   use Ocarina.Namet;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package EMAT renames Ocarina.ME_AADL_EMA.EMA_Tokens;
   package EMATN renames Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
   package Naming_Rules renames Ocarina.Analyzer.AADL_EMA.Naming_Rules;

   function Test_With_Package_Name
     (Pckg_Specification   : Node_Id;
      Package_EMA          : Node_Id) return Boolean;

   function Not_Allowed_Container_Package
      (Pckg_EMA          : Node_Id;
       Package_Container : Node_Id)
   return Boolean;

   procedure Test_With_Package_Alias
     (AADL_Root            : Node_Id;
      Pckg_Specification   : Node_Id;
      Package_EMA          : Node_Id;
      Success              : out Boolean;
      Identifier_Node      : out Node_Id;
      Pckg_Original_Name   : out Node_Id);

   procedure Exist_Annex_Library
     (AADL_Root     : Node_Id;
      Package_Node  : Node_Id;
      Exist_Library : out Boolean);

   procedure Get_Package_Specification
     (AADL_Root               :        Node_Id;
      Package_Spec_First      : in out Node_Id;
      Package_Spec_Last       : in out Node_Id);

   function Test_Has_Library
      (AADL_Root          : Node_Id;
       Pckg_Specification : Node_Id)
      return Boolean;

   function Search_In_List_Composed_Identifiers
      (Id_1 : Node_Id;
       Id_2 : Node_Id)
     return Boolean;
   procedure Find_Error_Type_Identifiers
       (Error_Type_Library_Node : Node_Id;
        List_First_Node         : in out Node_Id;
        List_Last_Node          : in out Node_Id;
        Is_Set                  : Boolean);

   --------------------------
   -- Get_EMA_Annexes_List --
   --------------------------

   procedure Get_EMA_Annexes_List
     (AADL_Root          :        Node_Id;
      List_First_Node    : in out Node_Id;
      List_Last_Node     : in out Node_Id;
      Package_Spec_First : in out Node_Id;
      Package_Spec_Last  : in out Node_Id)
   is
      use ATN;
      use EMAT;

      L1 : Node_List;
      L2 : Node_List;
      N1 : Node_Id;
      N2 : Node_Id;
      EMA_Root : Node_Id;

      Language : Name_Id;
      Is_Library : Boolean := False;

      Package_Spec : Node_Id;
   begin
      L1 := Find_All_Declarations (AADL_Root,
                                   (ATN.K_Component_Type,
                                    ATN.K_Component_Implementation,
                                    ATN.K_Feature_Group_Type,
                                    ATN.K_Annex_Library));
      N1 := L1.First;
      while Present (N1) loop
         if ATN.Kind (N1) = ATN.K_Annex_Library then
            Is_Library := True;
            Package_Spec := Container_Package (N1);
            Put_In_A_List_Node (Package_Spec_First,
                                Package_Spec_Last,
                                Package_Spec);
         else
            L2 := Find_All_Subclauses (N1, (1 => ATN.K_Annex_Subclause));
         end if;

         if Is_Library then
            Language := Utils.To_Lower
            (ATN.Name (ATN.Identifier (N1)));
            if Get_Name_String (Language) = EMAT.Language
               and then Present (ATN.Corresponding_Annex (N1))
            then
               EMA_Root := ATN.Corresponding_Annex (N1);
               if No (List_First_Node) then
                  List_First_Node := EMA_Root;
                  List_Last_Node := EMA_Root;
               else
                  EMATN.Set_Next_Node (List_Last_Node, EMA_Root);
                  EMATN.Set_Next_Node (EMA_Root, No_Node);
                  List_Last_Node := EMA_Root;
               end if;
            end if;
         else
            N2 := L2.First;

            while Present (N2) loop
               Language := Utils.To_Lower
               (ATN.Name (ATN.Identifier (N2)));
               if Get_Name_String (Language) = EMAT.Language
                  and then Present (ATN.Corresponding_Annex (N2))
               then
                  EMA_Root := ATN.Corresponding_Annex (N2);
                  if No (List_First_Node) then
                     List_First_Node := EMA_Root;
                     List_Last_Node := EMA_Root;
                  else
                     EMATN.Set_Next_Node (List_Last_Node, EMA_Root);
                     EMATN.Set_Next_Node (EMA_Root, No_Node);
                     List_Last_Node := EMA_Root;
                  end if;
               end if;
               N2 := ATN.Next_Entity (N2);
            end loop;

         end if;

         N1 := ATN.Next_Entity (N1);

      end loop;

   end Get_EMA_Annexes_List;

   ------------------
   -- Select_Nodes --
   ------------------

   procedure Select_Nodes
     (Parent_Node       :        List_Id;
      Kinds             :        Node_Kind_Array;
      List_First_Node   : in out Node_Id;
      List_Last_Node    : in out Node_Id)

   is
      Success         : Boolean;
      Local_List_Node : Node_Id;
   begin
      if not Is_Empty (Parent_Node) then
         Local_List_Node :=
           First_Node (Parent_Node);

         while Present (Local_List_Node) loop
            Success := False;

            for K in Kinds'Range loop
               Success := Success
                 or else (Kind (Local_List_Node) = Kinds (K));
            end loop;

            if Success then
               if No (List_First_Node) then
                  List_First_Node := Local_List_Node;
                  List_Last_Node := Local_List_Node;
               else
                  Set_Next_Node (List_Last_Node, Local_List_Node);
                  Set_Next_Node (Local_List_Node, No_Node);
                  List_Last_Node := Local_List_Node;
               end if;
            end if;

            Local_List_Node := Next_Node (Local_List_Node);
         end loop;
      end if;
   end Select_Nodes;

   --------------------------------------
   -- Select_Nodes_Of_List_Identifiers --
   --------------------------------------

   procedure Select_Nodes_Of_List_Identifiers
     (Parent_Node       :        Node_Id;
      Kinds             :        Node_Kind_Array;
      List_First_Node   : in out Node_Id;
      List_Last_Node    : in out Node_Id)
   is
      Success         : Boolean;
      Local_List_Node : Node_Id;
   begin
      if Present (Parent_Node) then
         Local_List_Node :=
           First_Node (Identifiers (Parent_Node));

         while Present (Local_List_Node) loop
            Success := False;

            for K in Kinds'Range loop
               Success := Success
                 or else (Kind (Local_List_Node) = Kinds (K));
            end loop;

            if Success then
               if No (List_First_Node) then
                  List_First_Node := Local_List_Node;
                  List_Last_Node := Local_List_Node;
               else
                  Set_Next_Node (List_Last_Node, Local_List_Node);
                  Set_Next_Node (Local_List_Node, No_Node);
                  List_Last_Node := Local_List_Node;
               end if;
            end if;

            Local_List_Node := Next_Node (Local_List_Node);
         end loop;
      end if;
   end Select_Nodes_Of_List_Identifiers;

   ----------------------------------
   -- Test_With_Package_Name_Alias --
   ----------------------------------

   --  Kind (Pckg_Original_Name) is K_Alias_Declaration
   --  if the package is renamed we search the original
   --  name to reaturn K_Package_Specification

   procedure Test_With_Package_Name_Alias
     (AADL_Root                    : Node_Id;
      Package_EMA                  : Node_Id;
      Package_Spec                 : Node_Id;
      Success                      : in out Boolean;
      AADL_Package_Referenced      : out Node_Id;
      Identifier_Node              : out Node_Id;
      Pckg_Original_Name           : out Node_Id;
      Not_Allowed_Reference_Itself : out Boolean)
   is

      Package_Node : Node_Id;
      Package_Specif : Node_List;
      Node : Node_Id;

      Success1, Success2 : Boolean;
      Proceed : Boolean := True;
      Is_Package_Spec : Boolean := False;
   begin
      Get_Package_Specification
          (AADL_Root,
           Package_Specif.First,
           Package_Specif.Last);

      Package_Node := Package_Specif.First;

      while Present (Package_Node) loop
         --  Test if the package is referencing itself
         Not_Allowed_Reference_Itself := False;
         if Present (Package_Spec) then
            Proceed := Not_Allowed_Container_Package (Package_EMA,
                       Package_Spec);
            if not Proceed then
               Not_Allowed_Reference_Itself := True;
            else
               Not_Allowed_Reference_Itself := False;
            end if;
         end if;

         if Proceed then
            --  Test with package names
            Success1 := Test_With_Package_Name (Package_Node,
                                                Package_EMA);
            --  Test with package alias
            Test_With_Package_Alias (AADL_Root, Package_Node,
                Package_EMA, Success2, Identifier_Node, Pckg_Original_Name);

            Success := Success1 or else Success2;
         else
            Success := False;
         end if;

         if Success then
            if Present (Pckg_Original_Name) then
               Node := Package_Specif.First;
               while Present (Node) loop
                  Is_Package_Spec := Search_In_List_Composed_Identifiers
                  (ATN.First_Node (ATN.Identifiers
                  (ATN.Package_Name (Pckg_Original_Name))),
                  ATN.First_Node (ATN.Identifiers (ATN.Package_Name (Node))));
                  exit when Is_Package_Spec;
                  Node := ATN.Next_Node (Node);
               end loop;

               if Is_Package_Spec then
                  AADL_Package_Referenced := Node;
               end if;

            else
               AADL_Package_Referenced := Package_Node;
            end if;
         else
            AADL_Package_Referenced := No_Node;
         end if;

         exit when Success;

         Package_Node := ATN.Next_Node (Package_Node);

      end loop;

   end Test_With_Package_Name_Alias;

   -------------------------------
   -- Get_Package_Specification --
   -------------------------------

   --  Tests which packages contain error_model_library

   procedure Get_Package_Specification
     (AADL_Root               :        Node_Id;
      Package_Spec_First      : in out Node_Id;
      Package_Spec_Last       : in out Node_Id)
   is
      use ATN;

      Package_List : Node_List;
      Node : Node_Id;
      Pckg_Node : Node_Id;
   begin
      Package_Spec_First := No_Node;
      Package_Spec_Last := No_Node;

      --  À revoir : test the language

      Package_List := Find_All_Declarations
                      (AADL_Root,
                       (ATN.K_Annex_Library,
                        ATN.K_Annex_Library));
      Node := Package_List.First;
      while Present (Node) loop

         if ATN.Kind (Node) = ATN.K_Annex_Library then
            Pckg_Node := Container_Package (Node);
            Put_In_A_List_Node (Package_Spec_First,
                                Package_Spec_Last,
                                Pckg_Node);
         end if;
         Node := ATN.Next_Entity (Node);
      end loop;

   end Get_Package_Specification;

   ----------------------------
   -- Test_With_Package_Name --
   ----------------------------

   --  The package name may contain more than one identifier
   --  So there is the other list node containing the number
   --  of identifiers for each package

   function Test_With_Package_Name
     (Pckg_Specification   : Node_Id;
      Package_EMA          : Node_Id) return Boolean
   is
      use ATN;

      pragma Assert
             (ATN.Kind (Pckg_Specification) =
              ATN.K_Package_Specification);

      Pckg_Name : Node_Id;
      Identifier_EMA : Node_Id;
      Identifier_AADL : Node_Id;

      Success : Boolean := False;
   begin
      Pckg_Name := ATN.Package_Name (Pckg_Specification);

      --  Comapre the package found in ema with
      --  the aadl package
      Identifier_EMA := EMATN.First_Node
         (EMATN.Identifiers (Package_EMA));
      Identifier_AADL := ATN.First_Node
         (ATN.Identifiers (Pckg_Name));

      --  The initialisation of the value Success
      if Present (Identifier_EMA) and then
         Present (Identifier_AADL) and then
         Get_Name_String (ATN.Name (Identifier_AADL))
         = Get_Name_String (EMATN.Name (Identifier_EMA))
      then
         Success := True;
         Identifier_AADL := ATN.Next_Node (Identifier_AADL);
         Identifier_EMA := EMATN.Next_Node (Identifier_EMA);
      else
         return False;
      end if;

      while Present (Identifier_EMA) loop
         if Present (Identifier_AADL) then
            if Get_Name_String (ATN.Name (Identifier_AADL))
               = Get_Name_String (EMATN.Name (Identifier_EMA))
            then
               Success := Success and then True;
            else
               Success := Success and then False;
            end if;
         else
            Success := Success and then False;
         end if;

         if not Success then
            return False;
         end if;

         Identifier_AADL := ATN.Next_Node (Identifier_AADL);
         Identifier_EMA := EMATN.Next_Node (Identifier_EMA);
      end loop;

      return Success;
   end Test_With_Package_Name;

   -----------------------------------
   -- Not_Allowed_Container_Package --
   -----------------------------------

   function Not_Allowed_Container_Package
      (Pckg_EMA          : Node_Id;
       Package_Container : Node_Id)
   return Boolean
   is
      Pckg_Name : Node_Id;
      Identifier_1 : Node_Id;
      Identifier_2 : Node_Id;
   begin
      Pckg_Name := ATN.Package_Name (Package_Container);

      Identifier_1 := EMATN.First_Node
            (EMATN.Identifiers (Pckg_EMA));
      Identifier_2 := ATN.First_Node
            (ATN.Identifiers (Pckg_Name));
      while Present (Identifier_1) and then
            Present (Identifier_2)
      loop
         if Get_Name_String (EMATN.Name (Identifier_1)) /=
            Get_Name_String (ATN.Name (Identifier_2))
         then
            return True;
         end if;

         Identifier_1 := EMATN.Next_Node (Identifier_1);
         Identifier_2 := ATN.Next_Node (Identifier_2);

         if No (Identifier_1) and then No (Identifier_2)
         then
            return False;
         end if;
      end loop;

      return True;
   end Not_Allowed_Container_Package;

   -----------------------------
   -- Test_With_Package_Alias --
   -----------------------------

   procedure Test_With_Package_Alias
     (AADL_Root            : Node_Id;
      Pckg_Specification   : Node_Id;
      Package_EMA          : Node_Id;
      Success              : out Boolean;
      Identifier_Node      : out Node_Id;
      Pckg_Original_Name   : out Node_Id)
   is
      use ATN;
      pragma Assert
             (ATN.Kind (Pckg_Specification) =
              ATN.K_Package_Specification);

      Name_Visibility_Node : Node_Id;
      Has_Library : Boolean := False;
      Pckg_Alias : Node_Id;

      Identifier_EMA : Node_Id;
   begin
      --  Initialize out parameters
      Identifier_Node := No_Node;
      Pckg_Original_Name := No_Node;
      Success := False;

      Name_Visibility_Node := ATN.First_Node
           (ATN.Declarations (Pckg_Specification));
      while Present (Name_Visibility_Node) loop
         if ATN.Kind (Name_Visibility_Node) =
            ATN.K_Name_Visibility_Declaration
         then
            if ATN.List_Items (Name_Visibility_Node)
               /= No_List or else Present (ATN.First_Node
               (ATN.List_Items (Name_Visibility_Node)))
            then
               Pckg_Alias := ATN.First_Node
               (ATN.List_Items (Name_Visibility_Node));
               while Present (Pckg_Alias) loop
                  if ATN.Kind (Pckg_Alias) = ATN.K_Alias_Declaration
                  then
                     Has_Library := Test_Has_Library (AADL_Root,
                                    Pckg_Alias);

                     --  If the package specification contains model library
                     if Has_Library then
                        --  The package alias
                        Identifier_Node := ATN.Identifier (Pckg_Alias);
                        --  The aadl package that we renamed
                        if Present (Identifier_Node) then
                           Identifier_EMA := EMATN.First_Node
                              (EMATN.Identifiers (Package_EMA));
                           if Get_Name_String (ATN.Name (Identifier_Node))
                              = Get_Name_String (EMATN.Name (Identifier_EMA))
                           then
                              Success := True;
                              Pckg_Original_Name := Pckg_Alias;
                              return;
                           end if;
                        end if;
                     end if;

                  end if;
                  Pckg_Alias := ATN.Next_Node (Pckg_Alias);
               end loop;
            end if;

         end if;
         Name_Visibility_Node := ATN.Next_Node
              (Name_Visibility_Node);
      end loop;

   end Test_With_Package_Alias;

   ------------------------
   -- Put_In_A_List_Node --
   ------------------------

   procedure Put_In_A_List_Node (List_First_Node : in out Node_Id;
                                 List_Last_Node  : in out Node_Id;
                                 Pckg_Name : Node_Id)
   is
   begin

      if No (List_First_Node) then
         List_First_Node := Pckg_Name;
         List_Last_Node := Pckg_Name;
      else
         ATN.Set_Next_Node
         (List_Last_Node, Pckg_Name);
         ATN.Set_Next_Node (Pckg_Name, No_Node);
         List_Last_Node := Pckg_Name;
      end if;

   end Put_In_A_List_Node;

   -------------------------------
   -- Search_Package_Annex_Root --
   -------------------------------

   function Search_Package_Annex_Root
     (AADL_Root    : Node_Id;
      Package_Spec : Node_Id)
      return Node_Id
   is
      use ATN;

      List : Node_List;
      Annex_Root : Node_Id;

      Error_Type_Library_Node : Node_Id := No_Node;
   begin
      --  A revoir : répétition : Package_Spec
      List := Find_All_Declarations (AADL_Root,
                                     (ATN.K_Annex_Library,
                                      ATN.K_Annex_Library),
                                     Package_Spec);
      Annex_Root := List.First;
      if Present (Annex_Root) then
         Error_Type_Library_Node := EMATN.Error_Type_Library
         (ATN.Corresponding_Annex (Annex_Root));
      end if;

      return Error_Type_Library_Node;
   end Search_Package_Annex_Root;

   -----------------------
   -- Get_Error_Type_Id --
   -----------------------

   --  Search local identifiers (extend library
   --  and error_type_library_element) of Pckg_Spec

   procedure Get_Error_Type_Id
        (AADL_Root         : Node_Id;
         Pckg_Spec         : Node_Id;
         Identifiers_First : in out Node_Id;
         Identifiers_Last  : in out Node_Id;
         Is_Set            : Boolean)
   is
      Error_Type_Library_Node : Node_Id;
      Error_Model : Node_Id;
      List_Extended : List_Id;

      Success : Boolean := False;
      AADL_Package_Referenced : Node_Id;
      Identifier_Node : Node_Id;
      Pckg_Original_Name : Node_Id;
      Not_Allowed_Reference_Itself : Boolean;
   begin
      Error_Type_Library_Node := Search_Package_Annex_Root
          (AADL_Root, Pckg_Spec);
      if No (Error_Type_Library_Node) then
         return;
      end if;

      --  Local_Identifiers
      Find_Error_Type_Identifiers
       (Error_Type_Library_Node,
        Identifiers_First,
        Identifiers_Last,
        Is_Set);

      --  identifers in extended error_type_library
      List_Extended := Error_Type_Library_List_Extended
                       (Error_Type_Library_Node);
      if not Is_Empty (List_Extended) then

         Error_Model := First_Node (List_Extended);
         while Present (Error_Model) loop

            --  This function is only used to return the referenced packages
            --  in extends clause, other tests are done later in naming_rules
            Test_With_Package_Name_Alias
               (AADL_Root,
                Error_Model,
                No_Node,
                Success,
                AADL_Package_Referenced,
                Identifier_Node,
                Pckg_Original_Name,
                Not_Allowed_Reference_Itself);

            if Success then
               Get_Error_Type_Id
                (AADL_Root, AADL_Package_Referenced, Identifiers_First,
                 Identifiers_Last, Is_Set);
            end if;

            Error_Model := Next_Node (Error_Model);
         end loop;

      end if;

   end Get_Error_Type_Id;

   -------------------------------------
   -- Find_Error_Type_Library_Element --
   -------------------------------------

   procedure Find_Error_Type_Library_Element
       (Error_Type_Library_Node : Node_Id;
        List_First_Node         : in out Node_Id;
        List_Last_Node          : in out Node_Id;
        Record_Identifier       : Boolean := True)
   is
      Element_Node : Node_Id;
      Node : Node_Id;
   begin
      Element_Node := First_Node
      (Error_Type_Library_Element_List (Error_Type_Library_Node));

      while Present (Element_Node) loop

         Node := Error_Type_Definition (Element_Node);
         Naming_Rules.Put_In_A_List_Node (List_First_Node,
                             List_Last_Node,
                             Node,
                             Record_Identifier);

         Node := Error_Type_Alias (Element_Node);
         Naming_Rules.Put_In_A_List_Node (List_First_Node,
                             List_Last_Node,
                             Node,
                             Record_Identifier);

         Node := Error_Type_Set_Definition (Element_Node);
         Naming_Rules.Put_In_A_List_Node (List_First_Node,
                             List_Last_Node,
                             Node,
                             Record_Identifier);

         Node := Error_Type_Set_Alias (Element_Node);
         Naming_Rules.Put_In_A_List_Node (List_First_Node,
                             List_Last_Node,
                             Node,
                             Record_Identifier);

         Element_Node := Next_Node (Element_Node);
      end loop;
   end Find_Error_Type_Library_Element;

   ---------------------------------
   -- Find_Error_Type_Identifiers --
   ---------------------------------

   --  error_type_identifiers are :
   --  Error_Type_Definition_identifier and
   --  Error_Type_Alias_Identifier

   --  error_type_set_identifiers are :
   --  error_type_set_identifier and
   --  error_type_set_alias_identifier

   procedure Find_Error_Type_Identifiers
       (Error_Type_Library_Node : Node_Id;
        List_First_Node         : in out Node_Id;
        List_Last_Node          : in out Node_Id;
        Is_Set                  : Boolean)
   is
      Element_Node : Node_Id;
      Node : Node_Id;
   begin
      Element_Node := First_Node
      (Error_Type_Library_Element_List (Error_Type_Library_Node));

      while Present (Element_Node) loop

         if Is_Set then
            Node := Error_Type_Set_Definition (Element_Node);
            Naming_Rules.Put_In_A_List_Node
                    (List_First_Node,
                     List_Last_Node,
                     Node,
                     True);

            Node := Error_Type_Set_Alias (Element_Node);
            Naming_Rules.Put_In_A_List_Node
                    (List_First_Node,
                     List_Last_Node,
                     Node,
                     True);
         else
            Node := Error_Type_Definition (Element_Node);
            Naming_Rules.Put_In_A_List_Node
                    (List_First_Node,
                     List_Last_Node,
                     Node,
                     True);

            Node := Error_Type_Alias (Element_Node);
            Naming_Rules.Put_In_A_List_Node
                    (List_First_Node,
                     List_Last_Node,
                     Node,
                     True);
         end if;

         Element_Node := Next_Node (Element_Node);
      end loop;
   end Find_Error_Type_Identifiers;

   -------------------------
   -- Exist_Annex_Library --
   -------------------------

   procedure Exist_Annex_Library
     (AADL_Root     : Node_Id;
      Package_Node  : Node_Id;
      Exist_Library : out Boolean)
   is
      use ATN;
      use EMAT;

      L1 : Node_List;
      N1 : Node_Id;
      Container_Pckg : Node_Id;

      Language : Name_Id;
      Is_Library : Boolean := False;

      Identifier_1, Identifier_2 : Node_Id;
      Package_Name_1, Package_Name_2 : Node_Id;
   begin
      Exist_Library := False;

      L1 := Find_All_Declarations (AADL_Root,
                                   (ATN.K_Annex_Library,
                                    ATN.K_Annex_Library));
      N1 := L1.First;
      while Present (N1) loop
         if ATN.Kind (N1) = ATN.K_Annex_Library then
            Is_Library := True;
         end if;

         if Is_Library then
            Language := Utils.To_Lower
            (ATN.Name (ATN.Identifier (N1)));
            if Get_Name_String (Language) = EMAT.Language
               and then Present (ATN.Corresponding_Annex (N1))
            then

               --  Test the package name as string
               --  because the package can be a package
               --  specification or alias
               Container_Pckg := Container_Package (N1);
               if ATN.Kind (Package_Node) = ATN.K_Package_Specification
               then
                  if Container_Pckg = Package_Node then
                     Exist_Library := True;
                  end if;
               else
                  Package_Name_1 := ATN.Package_Name (Package_Node);
                  Package_Name_2 := ATN.Package_Name (Container_Pckg);
                  Identifier_1 := ATN.First_Node
                     (ATN.Identifiers (Package_Name_1));
                  Identifier_2 := ATN.First_Node
                     (ATN.Identifiers (Package_Name_2));
                  Exist_Library := Search_In_List_Composed_Identifiers
                     (Identifier_1, Identifier_2);
               end if;
            end if;
         end if;

         exit when Exist_Library;

         N1 := ATN.Next_Entity (N1);

      end loop;

   end Exist_Annex_Library;

   --------------------------------
   -- Find_Package_Annex_Library --
   --------------------------------

   function Find_Package_Annex_Library
     (AADL_Root     : Node_Id;
      EMA_Root      : Node_Id)
    return Node_Id
   is
      use ATN;
      use EMAT;

      L1 : Node_List;
      N1 : Node_Id;
      Container_Pckg : Node_Id := No_Node;

      Language : Name_Id;
      Is_Library : Boolean := False;
      Exist_Library : Boolean := False;
   begin
      Exist_Library := False;

      L1 := Find_All_Declarations (AADL_Root,
                                   (ATN.K_Annex_Library,
                                    ATN.K_Annex_Library));
      N1 := L1.First;
      while Present (N1) loop
         if ATN.Kind (N1) = ATN.K_Annex_Library and then
            ATN.Corresponding_Annex (N1) = EMA_Root
         then
            Is_Library := True;
         end if;

         if Is_Library then
            Language := Utils.To_Lower
            (ATN.Name (ATN.Identifier (N1)));
            if Get_Name_String (Language) = EMAT.Language
               and then Present (ATN.Corresponding_Annex (N1))
            then
               Container_Pckg := Container_Package (N1);
               Exist_Library := True;
            end if;
         end if;

         exit when Exist_Library;

         N1 := ATN.Next_Entity (N1);

      end loop;

      return Container_Pckg;

   end Find_Package_Annex_Library;

   -----------------------------------------
   -- Search_In_List_Composed_Identifiers --
   -----------------------------------------

   function Search_In_List_Composed_Identifiers
      (Id_1 : Node_Id;
       Id_2 : Node_Id)
     return Boolean
   is
      Success : Boolean := False;
      Identifier_1 : Node_Id;
      Identifier_2 : Node_Id;
   begin
      Identifier_1 := Id_1;
      Identifier_2 := Id_2;

      while Present (Identifier_1) and then
            Present (Identifier_2)
      loop
         if Get_Name_String (ATN.Name (Identifier_1)) /=
            Get_Name_String (ATN.Name (Identifier_2))
         then
            Success := False;
            exit;
         end if;

         Identifier_1 := ATN.Next_Node (Identifier_1);
         Identifier_2 := ATN.Next_Node (Identifier_2);

         if No (Identifier_1) and then No (Identifier_2)
         then
            Success := True;
         end if;
      end loop;

      return Success;
   end Search_In_List_Composed_Identifiers;

   ----------------------
   -- Test_Has_Library --
   ----------------------

   function Test_Has_Library
      (AADL_Root          : Node_Id;
       Pckg_Specification : Node_Id)
      return Boolean
   is
      use ATN;
      pragma Assert
             (ATN.Kind (Pckg_Specification) = ATN.K_Package_Specification
              or else ATN.Kind (Pckg_Specification) = ATN.K_Alias_Declaration);

      Exist_Library : Boolean;
   begin
      Exist_Annex_Library
         (AADL_Root, Pckg_Specification, Exist_Library);
      return Exist_Library;
   end Test_Has_Library;

   ----------------------------------------------------
   -- Get_Error_Type_Reference_Of_Error_Type_Library --
   ----------------------------------------------------

   procedure Get_Error_Type_Reference_Of_Error_Type_Library
      (EMA_Root        :        Node_Id;
       List_First_Node : in out Node_Id;
       List_Last_Node  : in out Node_Id)
   is
      Error_Type_Library_Node : Node_Id;
      Element_List : List_Id;
      Element_Node : Node_Id;

      Parent_List : List_Id;
      Parent_Node : Node_Id;
      Node : Node_Id;
      Error_Type_Set_List : List_Id;
      Type_Set_Elt : Node_Id;
   begin
      Error_Type_Library_Node := Error_Type_Library (EMA_Root);
      if No (Error_Type_Library_Node) then
         List_First_Node := No_Node;
         List_Last_Node := No_Node;
         return;
      end if;

      Element_List := Error_Type_Library_Element_List
                      (Error_Type_Library_Node);
      if Is_Empty (Element_List) then
         List_First_Node := No_Node;
         List_Last_Node := No_Node;
      else
         Element_Node := First_Node (Element_List);
         while Present (Element_Node) loop
            --  error_type_definition
            Parent_Node := Error_Type_Definition (Element_Node);
            Naming_Rules.Put_In_A_List_Node
              (List_First_Node, List_Last_Node, Parent_Node, False);

            --  error_type_alias
            Parent_Node := Error_Type_Alias (Element_Node);
            Naming_Rules.Put_In_A_List_Node
              (List_First_Node, List_Last_Node, Parent_Node, False);

            --  error_type_or_set_reference
            Parent_Node := Error_Type_Set_Definition (Element_Node);
            if Present (Parent_Node) then
               Error_Type_Set_List := Error_Type_Set (Parent_Node);
               if not Is_Empty (Error_Type_Set_List) then
                  Type_Set_Elt := First_Node (Error_Type_Set_List);
                  while Present (Type_Set_Elt) loop
                     Parent_Node := Error_Type_Or_Set_Reference (Type_Set_Elt);
                     Naming_Rules.Put_In_A_List_Node
                        (List_First_Node, List_Last_Node, Parent_Node, False);

                     --  error_type_product
                     Parent_List := Error_Type_Product (Type_Set_Elt);
                     if not Is_Empty (Parent_List) then
                        Node := First_Node (Parent_List);
                        while Present (Node) loop
                           Naming_Rules.Put_In_A_List_Node
                             (List_First_Node, List_Last_Node, Node, False);
                        end loop;
                     end if;

                     Type_Set_Elt := Next_Node (Type_Set_Elt);
                  end loop;
               end if;
            end if;

            Element_Node := Next_Node (Element_Node);
         end loop;
      end if;

   end Get_Error_Type_Reference_Of_Error_Type_Library;

   --------------------------------------------------------
   -- Get_Error_Type_Set_Reference_Of_Error_Type_Library --
   --------------------------------------------------------

   procedure Get_Error_Type_Set_Reference_Of_Error_Type_Library
      (EMA_Root        :        Node_Id;
       List_First_Node : in out Node_Id;
       List_Last_Node  : in out Node_Id)
   is
      Error_Type_Library_Node : Node_Id;
      Element_List : List_Id;
      Element_Node : Node_Id;

      Parent_Node : Node_Id;
      Error_Type_Set_List : List_Id;
      Type_Set_Elt : Node_Id;
   begin
      Error_Type_Library_Node := Error_Type_Library (EMA_Root);
      if No (Error_Type_Library_Node) then
         List_First_Node := No_Node;
         List_Last_Node := No_Node;
         return;
      end if;

      Element_List := Error_Type_Library_Element_List
                      (Error_Type_Library_Node);
      if Is_Empty (Element_List) then
         List_First_Node := No_Node;
         List_Last_Node := No_Node;
      else
         Element_Node := First_Node (Element_List);
         while Present (Element_Node) loop
            --  error_type_set_alias
            Parent_Node := Error_Type_Set_Alias (Element_Node);
            Naming_Rules.Put_In_A_List_Node
              (List_First_Node, List_Last_Node, Parent_Node, False);

            --  error_type_or_set_reference
            Parent_Node := Error_Type_Set_Definition (Element_Node);
            if Present (Parent_Node) then
               Error_Type_Set_List := Error_Type_Set (Parent_Node);
               if not Is_Empty (Error_Type_Set_List) then
                  Type_Set_Elt := First_Node (Error_Type_Set_List);
                  while Present (Type_Set_Elt) loop
                     Parent_Node := Error_Type_Or_Set_Reference (Type_Set_Elt);
                     Naming_Rules.Put_In_A_List_Node
                        (List_First_Node, List_Last_Node, Parent_Node, False);
                     Type_Set_Elt := Next_Node (Type_Set_Elt);
                  end loop;
               end if;
            end if;

            Element_Node := Next_Node (Element_Node);
         end loop;
      end if;

   end Get_Error_Type_Set_Reference_Of_Error_Type_Library;

end Ocarina.Analyzer.AADL_EMA.Finder;
