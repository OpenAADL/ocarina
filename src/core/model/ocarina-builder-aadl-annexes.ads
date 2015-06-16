------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B U I L D E R . A A D L . A N N E X E S          --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides functions to build annex nodes into the AADL
--  tree.

with Ocarina.Types;
with Locations;

package Ocarina.Builder.AADL.Annexes is

   function Set_Annex_Content
     (Annex : Ocarina.Types.Node_Id;
      Text  : Ocarina.Types.Name_Id) return Boolean;
   --  Set the text of the annex. Annex is the Node_Id of the annex
   --  library or subclause, returned by Add_New_Annex_Subclause or
   --  Add_New_Annex_Library. Text is the Name_Id referencing the text
   --  of the annex. Return True is everything went right, else False.

   function Add_New_Annex_Subclause
     (Loc        : Locations.Location;
      Annex_Name : Ocarina.Types.Node_Id;
      Namespace  : Ocarina.Types.Node_Id;
      In_Modes   : Ocarina.Types.Node_Id) return Ocarina.Types.Node_Id;
   --  Create a new annex subclause. An annex subclause can be
   --  inserted into a component declaration (type or implementation)
   --  or a port group declaration. Loc is the location of the annex
   --  in the parsed text. Annex_Name is the name of the annex
   --  subclause. Namespace is the component or the port group where
   --  the annex must be inserted. This functions returns the Node_Id
   --  of the newly created annex subclause node, or No_Node if
   --  something went wrong.

   function Add_New_Annex_Library
     (Loc        : Locations.Location;
      Annex_Name : Ocarina.Types.Node_Id;
      Namespace  : Ocarina.Types.Node_Id;
      Is_Private : Boolean := False) return Ocarina.Types.Node_Id;
   --  Create a new annex library. An annex library can be inserted
   --  into a package or the top level AADL specification (i.e. the
   --  unnamed namespace). Loc is the location of the annex in the
   --  parsed text. Annex_Name is the name of the annex
   --  library. Namespace is the package specification or the top
   --  level AADL specification where the annex must be inserted. This
   --  functions returns the Node_Id of the newly created annex
   --  library node, or No_Node if something went wrong.

   function Add_New_Annex_Path
     (Loc              : Locations.Location;
      Container        : Ocarina.Types.Node_Id;
      Annex_Identifier : Ocarina.Types.Node_Id;
      List_Identifiers : Ocarina.Types.List_Id) return Ocarina.Types.Node_Id;
   --  Create a new annex path node. Loc is the location of the annex
   --  path in the parsed text. Container is the namespace which contain
   --  the annex path declaration. Annex_Identifier is the identifier of
   --  the annex path, it maybe No_Node. List_Identifiers is the list of
   --  identifiers declared. This function returns the Node_Id of the
   --  newly created annex path node, or No_Node if something went wrong.

end Ocarina.Builder.AADL.Annexes;
