------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  O C A R I N A . A N N O T A T I O N S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2007-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

--  This package implements an annotation mechanism. An annotation is
--  an information attached to an object. Each object has a list of
--  annotations (initially empty). The user can attach one or several
--  annotations and retrieve them by providing some criteria.

--  In this package, the object is an AST node. The objective here is
--  to provide a annotation mechanisme without interfering with the
--  AST definition.

with Ocarina.Types; use Ocarina.Types;

package Ocarina.Annotations is

   type Annotation_Id is new Natural;
   No_Annotation : constant Annotation_Id := 0;

   procedure Annotate (N : Node_Id; A : Node_Id; I : Node_Id := No_Node);
   procedure Annotate (N : Node_Id; A : Name_Id; I : Node_Id := No_Node);
   --  If there is already an annotation about A in N, update its
   --  information with I. Otherwise, create a new annotation.

   function Annotation_Index (N : Node_Id; A : Node_Id) return Annotation_Id;
   function Annotation_Index (N : Node_Id; A : Name_Id) return Annotation_Id;
   --  Return the annotation index if annotation about A exists for
   --  N. Otherwise, return No_Annotation.

   function Annotation_Node (I : Annotation_Id) return Node_Id;
   function Annotation_Name (I : Annotation_Id) return Name_Id;

   function Annotation_Info (N : Node_Id; A : Node_Id) return Node_Id;
   function Annotation_Info (N : Node_Id; A : Name_Id) return Node_Id;

   function First_Annotation (N : Node_Id) return Annotation_Id;
   function Next_Annotation (I : Annotation_Id) return Annotation_Id;
   --  Iteration functions in order to list all the annotations
   --  available for node N.

end Ocarina.Annotations;
