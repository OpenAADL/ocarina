------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           OCARINA.ME_AADL.AADL_INSTANCES.ENTITIES.PROPERTIES             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Namet;
with Charset;
with Utils;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;

package body Ocarina.Me_AADL.AADL_Instances.Entities.Properties is

   use Namet;
   use Charset;
   use Utils;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   ---------------------------------------
   -- Get_Value_Of_Property_Association --
   ---------------------------------------

   function Get_Value_Of_Property_Association
     (Property : Node_Id)
     return Value_Type
   is
      pragma Assert (Kind (Property) = K_Property_Association_Instance);
      pragma Assert
        (ATN.Kind (Single_Value
                     (AIN.Property_Association_Value (Property))) = K_Literal
         or else
         ATN.Kind (Number_Value (Single_Value (AIN.Property_Association_Value
                                           (Property)))) = K_Literal);
   begin
      if ATN.Kind (Single_Value
               (AIN.Property_Association_Value (Property))) = K_Literal
      then
         return Get_Value_Type
           (Value (Single_Value (AIN.Property_Association_Value (Property))));
      else
         return Get_Value_Type
           (Value
            (Number_Value
             (Single_Value
              (AIN.Property_Association_Value
               (Property)))));
      end if;
   end Get_Value_Of_Property_Association;

   -----------------------------------------
   -- Find_Property_Association_From_Name --
   -----------------------------------------

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : Name_Id;
      In_Mode       : Name_Id := No_Name)
     return Node_Id
   is
      List_Node     : Node_Id;
      M             : Node_Id;
      Mode_Name     : Name_Id;
      Lower_In_Mode : constant Name_Id := To_Lower (In_Mode);
   begin
      if Property_List /= No_List then
         List_Node := AIN.First_Node (Property_List);

         while List_Node /= No_Node loop
            if AIN.Name (AIN.Identifier (List_Node)) = Property_Name then
               --  Verify if the 'in mode' clause of this property
               --  matches with the In_Mode parameter value. There is
               --  a match if:

               --  1 - The 'in modes' list of the property association
               --      is empty.

               --  2 - The 'in_mode' parameter is invalid (No_Name)

               --  3 - The 'in modes' list of the property association
               --      is not empty, the 'in_mode' parameter is valid
               --      and its value corresponds to the name of one
               --      element of the list.

               if No (AIN.In_Modes (List_Node))
                 or else AINU.Is_Empty (ATN.Modes (AIN.In_Modes (List_Node)))
                 or else In_Mode = No_Name
               then
                  return List_Node;
               else
                  M := AIN.First_Node (ATN.Modes (AIN.In_Modes (List_Node)));

                  while Present (M) loop
                     --  Depending on the nature of the traversed tree
                     --  (model tree or instance tree), the structure
                     --  of the mode list of a property is not the
                     --  same.

                     case AIN.Kind (M) is
                        --  XXX TODO Remove this code if distcheck success
                        --  when K_Entity_Reference =>
                        --   Mode_Name := ATN.Name (ATN.Identifier (M));

                        when K_Node_Container =>
                           Mode_Name := AIN.Name (AIN.Identifier
                                                    (AIN.Item (M)));

                        when others =>
                           raise Program_Error;
                     end case;

                     if Mode_Name = Lower_In_Mode then
                        return List_Node;
                     end if;

                     M := AIN.Next_Node (M);
                  end loop;
               end if;
            end if;

            List_Node := AIN.Next_Node (List_Node);
         end loop;
      end if;

      return No_Node;
   end Find_Property_Association_From_Name;

   -----------------------------------------
   -- Find_Property_Association_From_Name --
   -----------------------------------------

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : String;
      In_Mode       : Name_Id := No_Name)
     return Node_Id
   is
      Name : Name_Id;
   begin
      Set_Str_To_Name_Buffer (To_Lower (Property_Name));
      Name := Name_Find;

      return Find_Property_Association_From_Name
        (Property_List, Name, In_Mode);
   end Find_Property_Association_From_Name;

   --------------------------------
   -- Type_Of_Property_Is_A_List --
   --------------------------------

   --  function Type_Of_Property_Is_A_List
   --  (Property : Node_Id)
   --  return Boolean
   --  is
   --   pragma Assert
   --     (Kind (Property) = K_Property_Association_Instance);
   --  begin
   --   case Kind (Property) is
   --      when K_Property_Association =>
   --         if Expanded_Single_Value
   --           (Property_Association_Value (Property)) = No_Node
   --           and then
   --           Expanded_Multi_Value
   --           (Property_Association_Value (Property)) = No_List
   --         then
   --            return Multi_Value
   --              (Property_Association_Value (Property)) /= No_List;

               --  If the property value has not been expanded yet, we
               --  use the raw property value.
   --        else
   --                        return Expanded_Multi_Value
   --              (Property_Association_Value (Property)) /= No_List;
   --         end if;

   --    when others =>
   --       return False;
   --    end case;
   --  end Type_Of_Property_Is_A_List;

end Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
