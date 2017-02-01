------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . A N A L Y Z E R . A A D L _ E M A . F I N D E R      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

with Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;

package Ocarina.Analyzer.AADL_EMA.Finder is

   type Node_Kind_Array is array (Positive range <>)
     of Ocarina.ME_AADL_EMA.EMA_Tree.Nodes.Node_Kind;

   type Int_Array is
      array (Integer range <>) of Integer;

   procedure Get_EMA_Annexes_List
     (AADL_Root          :        Node_Id;
      List_First_Node    : in out Node_Id;
      List_Last_Node     : in out Node_Id;
      Package_Spec_First : in out Node_Id;
      Package_Spec_Last  : in out Node_Id);

   procedure Select_Nodes
     (Parent_Node       :        List_Id;
      Kinds             :        Node_Kind_Array;
      List_First_Node   : in out Node_Id;
      List_Last_Node    : in out Node_Id);

   procedure Select_Nodes_Of_List_Identifiers
     (Parent_Node       :        Node_Id;
      Kinds             :        Node_Kind_Array;
      List_First_Node   : in out Node_Id;
      List_Last_Node    : in out Node_Id);

   procedure Test_With_Package_Name_Alias
     (AADL_Root                    : Node_Id;
      Package_EMA                  : Node_Id;
      Package_Spec                 : Node_Id;
      Success                      : in out Boolean;
      AADL_Package_Referenced      : out Node_Id;
      Identifier_Node              : out Node_Id;
      Pckg_Original_Name           : out Node_Id;
      Not_Allowed_Reference_Itself : out Boolean);

   procedure Get_Error_Type_Reference_Of_Error_Type_Library
      (EMA_Root        :        Node_Id;
       List_First_Node : in out Node_Id;
       List_Last_Node  : in out Node_Id);

   procedure Get_Error_Type_Set_Reference_Of_Error_Type_Library
      (EMA_Root        :        Node_Id;
       List_First_Node : in out Node_Id;
       List_Last_Node  : in out Node_Id);

   function Search_Package_Annex_Root
     (AADL_Root    : Node_Id;
      Package_Spec : Node_Id)
      return Node_Id;

   procedure Find_Error_Type_Library_Element
       (Error_Type_Library_Node : Node_Id;
        List_First_Node         : in out Node_Id;
        List_Last_Node          : in out Node_Id;
        Record_Identifier       : Boolean := True);

   procedure Put_In_A_List_Node (List_First_Node : in out Node_Id;
                                 List_Last_Node  : in out Node_Id;
                                 Pckg_Name : Node_Id);

   procedure Get_Error_Type_Id
        (AADL_Root         : Node_Id;
         Pckg_Spec         : Node_Id;
         Identifiers_First : in out Node_Id;
         Identifiers_Last  : in out Node_Id;
         Is_Set            : Boolean);

   function Find_Package_Annex_Library
     (AADL_Root     : Node_Id;
      EMA_Root      : Node_Id)
    return Node_Id;

end Ocarina.Analyzer.AADL_EMA.Finder;
