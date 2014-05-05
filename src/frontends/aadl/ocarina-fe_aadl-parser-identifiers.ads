------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . F E _ A A D L . P A R S E R . I D E N T I F I E R S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
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

package Ocarina.FE_AADL.Parser.Identifiers is

   type Refinement_Type is (RT_Refinement, RT_Refinable, RT_Not_Refinable);
   --  This type is used for parsing functions parameter
   --     RT_Refinement    : parse only Refinement
   --     RT_Refinable     : parse Refinement or Non_Refinement
   --     RT_Not_Refinable : Refinement is not allowed

   Refinable_To_RT : constant array (Boolean) of Refinement_Type :=
     (True => RT_Refinable, False => RT_Not_Refinable);

   function Make_Current_Identifier (Entity  : Node_Id) return Node_Id;
   --  Make an identifier from current token

   function P_Expected_Identifier (Expected_Id : Node_Id) return Boolean;
   --  Parse Identifier
   --  Return TRUE if parsed identifier name equals to Expected_Id name

   function P_Expected_Identifiers
     (Identifiers : List_Id;
      Delimiter   : Ocarina.ME_AADL.Tokens.Token_Type)
     return Boolean;
   --  Parse ( { Identifier Delimiter }* Identifier )
   --  These parsed identifiers must be the same as in list L
   --  These parsed identifiers will NOT be added in list L
   --  This function is useful for parsing 'end' clause for example
   --  Return TRUE if everything is OK

   function P_Identifier (Container : Ocarina.Types.Node_Id) return Node_Id;

   procedure P_Identifier_Refined_To
     (Option              : Refinement_Type;
      Optional_Identifier : Boolean;
      Code                : Parsing_Code;
      Refinement_Code     : Parsing_Code;
      Skip_Until_Token    : Ocarina.ME_AADL.Tokens.Token_Type;
      Identifier          : out Node_Id;
      Is_Refinement       : out Boolean;
      OK                  : out Boolean);
   --  Parse ( Identifier : [ refined to ] )
   --  Input parameters:
   --     Option              - Refinement is alowed, not allowed or imperative
   --     Optional_Identifier - If TRUE, ( Identifier : ) is optional
   --     Code                - Parsing code of current item
   --     Refinement_Code     - Parsing code of current item refinement
   --     Skip_Until_Token    - When an error occurs,
   --                           skip tokens until this token is reached
   --  Output parameters:
   --     Identifier          - defining identifier
   --     Is_Refinement       - 'refined to' is parsed ?
   --     OK                  - OK = TRUE if no error

   function P_Entity_Reference (Code : Parsing_Code) return Node_Id;
   --  [ package_name :: ] identifier [.identifier]
   --  Parse Unique_..._Identifier, etc.

   function P_Entity_Reference (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  Allow the possibility to parse entitiy reference in list of items

end Ocarina.FE_AADL.Parser.Identifiers;
