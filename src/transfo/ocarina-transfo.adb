------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . T R A N S F O                       --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Debug;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Utils;
with Ocarina.Namet;

package body Ocarina.Transfo is
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Debug;
   use Utils;
   use Ocarina.Namet;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   -----------------------
   -- Build_Unique_Name --
   -----------------------

   function Build_Unique_Name
     (List   : List_Id;
      Prefix : Name_Id;
      Shift  : Natural := 0) return Name_Id
   is
      ----------------
      -- Is_Natural --
      ----------------

      function Is_Natural (Str : String) return Boolean is
      begin
         for I in 1 .. Str'Length loop
            if Str (I) not in '0' .. '9' then
               return False;
            end if;
         end loop;

         return True;
      end Is_Natural;

      Prefix_Str : constant String := Get_Name_String (Prefix);
      N          : Node_Id         := No_Node;
      T, Sup     : Name_Id;
      I          : Int;
      Max        : Int             := -1;
   begin
      if not Is_Empty (List) then
         N := First_Node (List);
      end if;

      while Present (N) loop
         if Kind (N) = K_Subcomponent
           or else Kind (N) = K_Component_Type
           or else Kind (N) = K_Component_Implementation
           or else Kind (N) = K_Port_Spec
           or else Kind (N) = K_Connection
         then

            --  We search for the greater value of <Id>
            --  in all <Prefix>_<id>

            if Present (Identifier (N)) then
               T := Name (Identifier (N));
               if T /= No_Name and then Is_Prefix (Prefix, T) then
                  Sup := Remove_Prefix_From_Name (Prefix_Str, T);

                  if Is_Natural (Get_Name_String (Sup)) then
                     I := Int'Value (Get_Name_String (Sup));
                     if I > Max then
                        Max := I;
                     end if;
                  end if;
               end if;
            end if;
         end if;

         N := Next_Node (N);
      end loop;

      --  Return <prefix>_<id+shift+1>

      return Get_String_Name (Prefix_Str & Image (Max + Int (Shift) + 1));

   end Build_Unique_Name;

   ---------------------------------
   -- Build_Unique_Component_Name --
   ---------------------------------

   function Build_Unique_Component_Name
     (Pkg    : Node_Id;
      Prefix : Name_Id) return Name_Id
   is
      pragma Assert (Kind (Pkg) = K_Package_Specification);
   begin
      return Build_Unique_Name (Declarations (Pkg), Prefix);
   end Build_Unique_Component_Name;

   ------------------------------------
   -- Build_Unique_Subcomponent_Name --
   ------------------------------------

   function Build_Unique_Subcomponent_Name
     (Container : Node_Id;
      Prefix    : Name_Id) return Name_Id
   is
      pragma Assert (Kind (Container) = K_Component_Implementation);
   begin
      return Build_Unique_Name (Subcomponents (Container), Prefix);
   end Build_Unique_Subcomponent_Name;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Thread_Prefix     := Get_String_Name ("thr_");
      Process_Prefix    := Get_String_Name ("proc_");
      Subprogram_Prefix := Get_String_Name ("func_");
      Data_Prefix       := Get_String_Name ("dat_");
      Feature_Prefix    := Get_String_Name ("msg_");
      Wrapper_Prefix    := Get_String_Name ("wrapper_");
      Connection_Prefix := Get_String_Name ("cnx_");
   end Init;

   ---------------------------
   -- Search_Thread_By_Name --
   ---------------------------

   function Search_Thread_By_Name
     (Process     : Node_Id;
      Thread_Name : String) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Instances.Entities;

      T : Node_Id;
   begin
      T := AIN.First_Node (AIN.Subcomponents (Process));
      while Present (T) loop
         if Get_Category_Of_Component (T) = CC_Thread then
            declare
               Raw_Name : constant String :=
                 Get_Name_String
                   (Name (Identifier (AIN.Corresponding_Declaration (T))));
            begin
               if Raw_Name = Thread_Name then
                  return T;
               end if;
            end;
         end if;
         T := AIN.Next_Node (T);
      end loop;

      return No_Node;
   end Search_Thread_By_Name;

   ----------------------------
   -- Search_Process_By_Name --
   ----------------------------

   function Search_Process_By_Name (Process_Name : String) return Node_Id is
      use Ocarina.ME_AADL.AADL_Instances.Entities;
      use Ocarina.ME_AADL.AADL_Instances.Nodes;

      Node : Node_Id := No_Node;
   begin

      --  We search for all threads, build the related process_name,
      --  and then compare it to the parameter

      for N in 1 .. AIN.Entries.Last loop
         if AIN.Kind (N) = AIN.K_Component_Instance then
            if Get_Category_Of_Component (N) = CC_Process then
               if Get_Name_String
                   (ATN.Name
                      (ATN.Identifier
                         (AIN.Corresponding_Declaration
                            (AIN.Parent_Subcomponent (N))))) =
                 Process_Name
               then
                  Node := N;
               end if;
            end if;
         end if;
      end loop;

      return Node;
   end Search_Process_By_Name;

   --------------------------
   -- Build_Name_From_Path --
   --------------------------

   function Build_Name_From_Path (Path : List_Id) return Name_Id is
      N     : Node_Id := First_Node (Path);
      First : Boolean := True;
   begin
      Set_Str_To_Name_Buffer ("");
      while Present (N) loop
         if First then
            First := False;
         else
            Add_Str_To_Name_Buffer (".");
         end if;
         Get_Name_String_And_Append (Name (Item (N)));

         N := Next_Node (N);
      end loop;

      return Name_Find;
   end Build_Name_From_Path;

   ------------------
   -- Concat_Names --
   ------------------

   function Concat_Names (N1, N2 : Node_Id) return Name_Id is
      pragma Assert
        (Kind (N1) = K_Component_Type
         or else Kind (N1) = K_Component_Implementation);
      pragma Assert
        (Kind (N2) = K_Component_Type
         or else Kind (N2) = K_Component_Implementation);
      NM1 : constant Name_Id := Name (Identifier (N1));
      NM2 : constant Name_Id := Name (Identifier (N2));
      Str : constant String  :=
        Get_Name_String (NM1) & "_" & Get_Name_String (NM2);
   begin
      return Get_String_Name (Str);
   end Concat_Names;

end Ocarina.Transfo;
