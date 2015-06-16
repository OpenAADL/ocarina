------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . I N S T A N C E S . N A M E S P A C E S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Locations;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

package body Ocarina.Instances.Namespaces is

   use Locations;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   ---------------------------
   -- Instantiate_Namespace --
   ---------------------------

   function Instantiate_Namespace
     (Instance_Root : Node_Id;
      Namespace     : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Instance_Root) = K_Architecture_Instance);
      pragma Assert
        (Kind (Namespace) = K_Package_Specification
         or else Kind (Namespace) = K_AADL_Specification);

      Namespace_Instance : Node_Id := No_Node;
   begin
      --  If the namespace has already been instantiateed, do not
      --  re-instantiate it and return the previously instantiateed
      --  value.

      if Kind (Namespace) = K_Package_Specification then
         Namespace_Instance :=
           Get_First_Homonym_Instance
             (AIN.Namespaces (Instance_Root),
              Namespace);

         if No (Namespace_Instance) then
            Namespace_Instance := New_Node (K_Namespace_Instance, No_Location);
            AIN.Set_Declarations
              (Namespace_Instance,
               New_List (K_List_Id, No_Location));
            AIN.Set_Identifier
              (Namespace_Instance,
               Duplicate_Identifier (ATN.Identifier (Namespace)));
            AIN.Set_Corresponding_Declaration (Namespace_Instance, Namespace);

            Append_Node_To_List
              (Namespace_Instance,
               AIN.Namespaces (Instance_Root));
         end if;

      else
         Namespace_Instance := Unnamed_Namespace (Instance_Root);

         if No (Namespace_Instance) then
            Namespace_Instance := New_Node (K_Namespace_Instance, No_Location);
            Set_Unnamed_Namespace (Instance_Root, Namespace_Instance);
            AIN.Set_Declarations
              (Namespace_Instance,
               New_List (K_List_Id, No_Location));
            AIN.Set_Identifier
              (Namespace_Instance,
               Make_Identifier
                 (ATN.Loc (Namespace),
                  No_Name,
                  No_Name,
                  Namespace));
            AIN.Set_Corresponding_Declaration (Namespace_Instance, Namespace);
         end if;
      end if;

      return Namespace_Instance;
   end Instantiate_Namespace;

end Ocarina.Instances.Namespaces;
