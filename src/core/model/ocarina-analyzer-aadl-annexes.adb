------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . A N A L Y Z E R . A A D L . A N N E X E S         --
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

with Ocarina.Analyzer;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_REAL.Tokens;
with Ocarina.ME_AADL_EMA.EMA_Tokens;
with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.Namet;
with Errors;
with Charset;
with Utils;

package body Ocarina.Analyzer.AADL.Annexes is

   use Ocarina.Analyzer.AADL.Finder;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.Namet;
   use Errors;

   package RT renames Ocarina.ME_REAL.Tokens;
   package EMAT renames Ocarina.ME_AADL_EMA.EMA_Tokens;
   package BAT renames Ocarina.ME_AADL_BA.Tokens;

   function Analyze (Language : Name_Id;
                     Language_Annex : String;
                     Root : Node_Id) return Boolean;
   --  Allows to analyze all the annexes in the aadl model

   procedure Execute_Analyze (Exist_Real : in out Boolean;
                              Exist_EMA  : in out Boolean;
                              Exist_BA   : in out Boolean;
                              Success    : in out Boolean;
                              Root       : Node_Id;
                              Ni         : Node_Id);

   -------------
   -- Analyze --
   -------------

   function Analyze (Language : Name_Id;
                     Language_Annex : String;
                     Root : Node_Id) return Boolean
   is
      Success : Boolean := False;

   begin

      if Language_Annex = RT.Language then
         --  For the REAL annex, analysis is done as part of the
         --  backend logic

         Success := True;
      else

         Success := Analyze (Language, Root);
      end if;

      Exit_On_Error (not Success, "Cannot analyze " &
      Charset.To_Upper (Language_Annex) & " specifications");

      return Success;
   end Analyze;

   --------------------------
   -- Find_Analyze_Annexes --
   --------------------------

   --  if we find a type of the annexes : real or ema
   --  then we call the function analyze once.
   --  The function analyze model will do the search of the
   --  rest of the annexes by her self

   function Find_Analyze_Annexes (Root : Node_Id) return Boolean
   is
      L1 : Node_List;
      L2 : Node_List;
      N1 : Node_Id;
      N2 : Node_Id;

      Success    : Boolean := True;
      Is_Library : Boolean := False;
      Exist_EMA  : Boolean := False;
      Exist_Real : Boolean := False;
      Exist_BA   : Boolean := False;
   begin
      L1 := Find_All_Declarations (Root,
                                   (K_Component_Type,
                                    K_Component_Implementation,
                                    K_Feature_Group_Type,
                                    K_Annex_Library));
      N1 := L1.First;
      while Present (N1) loop
         if Kind (N1) = K_Feature_Group_Type then
            L2 := Find_All_Subclauses
                  (N1, (1 => K_Annex_Subclause));
            --  FIXME : subclause annexes in Feature_Group_Type
            --  are not supported
         elsif Kind (N1) = K_Annex_Library then
            Is_Library := True;
         else
            L2 := Find_All_Subclauses (N1, (1 => K_Annex_Subclause));
         end if;

         if Is_Library then
            Execute_Analyze (Exist_Real, Exist_EMA, Exist_BA,
                             Success, Root, N1);
            --  FIXME : we can't have more than one annex library of the
            --  same type
         else
            N2 := L2.First;

            while Present (N2) loop
               Execute_Analyze (Exist_Real, Exist_EMA, Exist_BA,
                                Success, Root, N2);
               N2 := Next_Entity (N2);
            end loop;
         end if;

         N1 := Next_Entity (N1);
      end loop;

      return Success;
   end Find_Analyze_Annexes;

   ---------------------
   -- Execute_Analyze --
   ---------------------

   procedure Execute_Analyze (Exist_Real : in out Boolean;
                              Exist_EMA  : in out Boolean;
                              Exist_BA   : in out Boolean;
                              Success    : in out Boolean;
                              Root       : Node_Id;
                              Ni         : Node_Id)
   is
      Language : Name_Id;
   begin
      Language := Utils.To_Lower
            (Name (Identifier (Ni)));
      if Get_Name_String (Language) = RT.Language
         and then Present (Corresponding_Annex (Ni))
      then
         if not Exist_Real then
            Success := Success and then Analyze
            (Language, Get_Name_String (Language), Root);
         end if;
         Exist_Real := True;
      elsif Get_Name_String (Language) = EMAT.Language
         and then Present (Corresponding_Annex (Ni))
      then
         if not Exist_EMA then
            Success := Success and then Analyze
            (Language, Get_Name_String (Language), Root);
         end if;
         Exist_EMA := True;
      elsif Get_Name_String (Language) = BAT.Language
         and then Present (Corresponding_Annex (Ni))
      then
         if not Exist_BA then
            Success := Success and then Analyze
            (Language, Get_Name_String (Language), Root);
         end if;
         Exist_BA := True;
      end if;
   end Execute_Analyze;

end Ocarina.Analyzer.AADL.Annexes;
