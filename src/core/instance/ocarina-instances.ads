------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                    O C A R I N A . I N S T A N C E S                     --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.Types; use Ocarina.Types;

package Ocarina.Instances is

   function Instantiate_Model
     (Root : Node_Id;
      Exit_If_Error : Boolean := True)
     return Node_Id;
   --  Instantiate the tree of the model and return the instantiated
   --  architecture. If Root_System_Name (from Ocarina.Options) is set
   --  and corresponds to a root system eligible for instantiation,
   --  then instantiate the model from this system.  If Exit_If_Error
   --  is set to true, exit immediatly in case of an error, otherwise
   --  return No_Node.
   --
   --  If Root_System_Name does not correspond to anything,
   --  instantiate nothing. Root_System_Name is to be used when
   --  several system implementations are electible.

private

   function Get_Instance (N : Node_Id) return Node_Id;
   procedure Set_Instance (N : Node_Id; E : Node_Id);
   --  To avoid useless re-instantiation

   function Get_First_Homonym
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id;
   --  Find if an entity of Declaration_List has the same name as
   --  Name_Of_Declaration. If so, return it, else return
   --  No_Node. Note that the list is NOT a list of identifiers, but a
   --  list of AADL entities. If Name_Of_Declaration is No_Name, then
   --  return No_Node. This is usefull for declaration refinements.

   function Get_First_Homonym_Instance
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id;
   --  XXX TODO Add comment or merge this function

   function Get_First_Homonym_Instance
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id;
   --  XXX TODO Add comment or merge this function

   function Get_First_Homonym
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id;
   --  Same as above but with an AADL declaration

   function Get_First_Contained_Homonym
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id;
   --  The same as above but assumes the list Declaration_List is a
   --  node container list.

   function Get_First_Contained_Homonym_Instance
     (Declaration_List    : List_Id;
      Name_Of_Declaration : Name_Id) return Node_Id;
   --  XXX TODO Add comment or merge this function

   function Get_First_Contained_Homonym
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id;
   --  Same as above but with an AADL declaration

   function Get_First_Contained_Homonym_Instance
     (Declaration_List : List_Id;
      Declaration      : Node_Id) return Node_Id;
   --  XXX TODO Add comment or merge this function

   procedure Append_To_Namespace_Instance
     (Instance_Root   : Node_Id;
      Entity_Instance : Node_Id);
   --  Append an entity instance in the namespace instance
   --  corresponding to the namespace declaration of its corresponding
   --  entity. If the entity instance has already been appended, do
   --  nothing.

end Ocarina.Instances;
