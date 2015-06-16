------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . P N . F O R M A T . C A M I       --
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

with Ocarina.Output;
with Ocarina.Backends.PN.Debug;

with Ocarina.Backends.PN.Nodes;
with Ocarina.Backends.PN.Nutils;
with Ocarina.Namet;
with Ocarina.AADL_Values;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.PN.Format.Cami is

   package OPN renames Ocarina.Backends.PN.Nodes;
   package OPU renames Ocarina.Backends.PN.Nutils;
   package OAV renames Ocarina.AADL_Values;

   -------------------
   --  Print_Place  --
   -------------------

   procedure Print_Place (Pn_Generated : Node_Id; Pn_P : Node_Id) is
      use Ocarina.Output;
      use Ocarina.Backends.PN.Debug;
      use OPN;
      use Ocarina.Namet;
      use OAV;

   begin

      if Pn_Generated /= No_Node and then Pn_P /= No_Node then
         --  print node
         Write_Line ("CN(5:place," & Image (Pn_P) & ")");
         --  print name
         declare
            S : constant String := Get_Name_String (Name (Identifier (Pn_P)));
         begin
            Write_Line
              ("CT(4:name," &
               Image (Pn_P) &
               "," &
               OAV.Image (New_Integer_Value (S'Length), False) &
               ":" &
               S &
               ")");
         end;
         --  print domain
         if Domain (Pn_P) /= No_Node then
            --  colored
            declare
               S : constant String :=
                 Get_Name_String (Name (Identifier (Domain (Pn_P))));
            begin
               Write_Line
                 ("CT(6:domain," &
                  Image (Pn_P) &
                  "," &
                  OAV.Image (New_Integer_Value (S'Length), False) &
                  ":" &
                  S &
                  ")");
            end;
         end if;
         --  print marking
         if Domain (Pn_P) /= No_Node then
            --  build complete string in order to compute length
            --  potentially colored place
            declare
               Node_Iter : Node_Id;
            begin
               Node_Iter := OPN.First_Node (Tokens (Marking (Pn_P)));
               if Node_Iter /= No_Node then
                  Write_Str ("CT(7:marking," & Image (Pn_P) & ",");
                  Set_Str_To_Name_Buffer ("<");
                  while Present (Node_Iter) loop
                     Add_Str_To_Name_Buffer
                       (Get_Name_String (Name (Identifier (Node_Iter))));
                     if OPN.Next_Node (Node_Iter) = No_Node then
                        Add_Str_To_Name_Buffer (">");
                     else
                        Add_Str_To_Name_Buffer (",");
                     end if;
                     --  next
                     Node_Iter := OPN.Next_Node (Node_Iter);
                  end loop;

                  --  here, marking is accessible through name_find
                  declare
                     S : constant String := Get_Name_String (Name_Find);
                  begin
                     Write_Str
                       (OAV.Image (New_Integer_Value (S'Length), False) &
                        ":" &
                        S);
                  end;

                  Write_Line (")");
               end if;
            end;

         else
            --  uncolored
            declare
               Tokens_Count : constant Value_Type :=
                 Get_Value_Type (Nb_T (Pn_P));
            begin
               if Tokens_Count.IVal /= 0 then
                  --  uncolored place
                  Write_Str ("CT(7:marking," & Image (Pn_P) & ",");
                  Set_Str_To_Name_Buffer (OAV.Image (Tokens_Count));
                  declare
                     S : constant String := Get_Name_String (Name_Find);
                  begin
                     Write_Str
                       (OAV.Image (New_Integer_Value (S'Length), False) &
                        ":" &
                        S);
                  end;

                  Write_Line (")");

               end if;
            end;
         end if;                        --  color on not

      end if;
   end Print_Place;

   -------------------
   --  Print_Trans  --
   -------------------

   procedure Print_Trans (Pn_Generated : Node_Id; Pn_T : Node_Id) is
      use Ocarina.Output;
      use Ocarina.Backends.PN.Debug;
      use OPN;
      use Ocarina.Namet;
      use OAV;
      use OPU;
      use Ocarina.Backends.Utils;

   begin
      if Pn_Generated /= No_Node
        and then Pn_T /= No_Node
        and then Get_Handling (Pn_T, By_Node, H_PN_To_Delete) = No_Node
      then
         --  print node
         Write_Line ("CN(10:transition," & Image (Pn_T) & ")");

         --  print name
         declare
            S : constant String := Get_Name_String (Name (Identifier (Pn_T)));
         begin
            Write_Line
              ("CT(4:name," &
               Image (Pn_T) &
               "," &
               OAV.Image (New_Integer_Value (S'Length), False) &
               ":" &
               S &
               ")");
         end;

         --  print guard
         --  XXX

         --  print arcs

         declare
            Iter : Node_Id;
         begin
            --  in
            if not Is_Empty (Pn_Arcs_In (Pn_T)) then
               Iter := OPN.First_Node (Pn_Arcs_In (Pn_T));
               while Present (Iter) loop
                  Write_Line
                    ("CA(3:arc," &
                     Image (Iter) &
                     "," &
                     Image (Pn_From (Iter)) &
                     "," &
                     Image (Pn_To (Iter)) &
                     ")");
                  --  valuation
                  if not Is_Empty (Valuations (Iter)) then
                     Write_Str ("CT(9:valuation," & Image (Iter) & ",");
                     --  build string to compute length
                     declare
                        Val_Iter : Node_Id :=
                          OPN.First_Node (Valuations (Iter));
                     begin
                        if Is_Colored (Val_Iter) then
                           Set_Str_To_Name_Buffer ("<");
                           while Present (Val_Iter) loop
                              Add_Str_To_Name_Buffer
                                (Get_Name_String
                                   (Name (Identifier (Val_Iter))));
                              if OPN.Next_Node (Val_Iter) /= No_Node then
                                 Add_Str_To_Name_Buffer (",");
                              end if;
                              --  next
                              Val_Iter := OPN.Next_Node (Val_Iter);
                           end loop;
                           Add_Str_To_Name_Buffer (">");
                        else
                           Set_Str_To_Name_Buffer
                             (Get_Name_String (Name (Identifier (Val_Iter))));
                        end if;
                     end;
                     --  here, marking is accessible through name_find
                     declare
                        S : constant String := Get_Name_String (Name_Find);
                     begin
                        Write_Str
                          (OAV.Image (New_Integer_Value (S'Length), False) &
                           ":" &
                           S);
                     end;

                     Write_Line (")");

                  end if;
                  --  next
                  Iter := OPN.Next_Node (Iter);
               end loop;
            end if;
            --  out
            if not Is_Empty (Pn_Arcs_Out (Pn_T)) then
               Iter := OPN.First_Node (Pn_Arcs_Out (Pn_T));
               while Present (Iter) loop
                  Write_Line
                    ("CA(3:arc," &
                     Image (Iter) &
                     "," &
                     Image (Pn_From (Iter)) &
                     "," &
                     Image (Pn_To (Iter)) &
                     ")");
                  --  valuation
                  if not Is_Empty (Valuations (Iter)) then
                     Write_Str ("CT(9:valuation," & Image (Iter) & ",");
                     --  build string to compute length
                     declare
                        Val_Iter : Node_Id :=
                          OPN.First_Node (Valuations (Iter));
                     begin
                        if Is_Colored (Val_Iter) then

                           Set_Str_To_Name_Buffer ("<");
                           while Present (Val_Iter) loop
                              Add_Str_To_Name_Buffer
                                (Get_Name_String
                                   (Name (Identifier (Val_Iter))));
                              if OPN.Next_Node (Val_Iter) /= No_Node then
                                 Add_Str_To_Name_Buffer (",");
                              end if;
                              --  next
                              Val_Iter := OPN.Next_Node (Val_Iter);
                           end loop;
                           Add_Str_To_Name_Buffer (">");
                        else
                           Set_Str_To_Name_Buffer
                             (Get_Name_String (Name (Identifier (Val_Iter))));
                        end if;
                     end;
                     --  here, marking is accessible through name_find
                     declare
                        S : constant String := Get_Name_String (Name_Find);
                     begin
                        Write_Str
                          (OAV.Image (New_Integer_Value (S'Length), False) &
                           ":" &
                           S);
                     end;

                     Write_Line (")");

                  end if;
                  --  next
                  Iter := OPN.Next_Node (Iter);
               end loop;
            end if;
         end;
      end if;

      --  arcs in

      --  arcs out

      --    print arcs valuations

   end Print_Trans;

   -----------------------------------
   --  Print_Formalism_Information  --
   -----------------------------------

   procedure Print_Formalism_Information (Pn_Generated : Node_Id) is
      use Ocarina.Output;
      use OPN;
      use Ocarina.Namet;
      use OAV;
      use OPU;

      Line_Count : Unsigned_Long_Long := 1;
   begin
      if Pn_Generated /= No_Node then
         --  print net node
         Write_Line ("CN(3:net,1)");

         --      print classes
         if Classes (Pn_Formalism_Specific_Informations (Pn_Generated)) /=
           No_Node
         then
            Write_Line
              ("CM(11:declaration,1," &
               OAV.Image (New_Integer_Value (Line_Count), False) &
               ",1,5:Class)");
            Line_Count := Line_Count + 1;

            --  first is th_ids
            declare
               --  hack since OAV.Image blast name_buffer....
               Begin_Range : constant String :=
                 Get_Name_String
                   (Name
                      (Identifier
                         (OPN.First_Node
                            (Class_List
                               (Classes
                                  (Pn_Formalism_Specific_Informations
                                     (Pn_Generated)))))));
               Max_Range : constant String :=
                 OAV.Image
                   (Get_Value_Type
                      (Threads_Count
                         (Pn_Formalism_Specific_Informations (Pn_Generated))));
            begin
               Set_Str_To_Name_Buffer
                 (Begin_Range & " is 0.." & Max_Range & ";");
               declare
                  S : constant String := Get_Name_String (Name_Find);
               begin

                  Write_Line
                    ("CM(11:declaration,1," &
                     OAV.Image (New_Integer_Value (Line_Count), False) &
                     ",1," &
                     OAV.Image (New_Integer_Value (S'Length), False) &
                     ":" &
                     S &
                     ")");
                  Line_Count := Line_Count + 1;
               end;
            end;

            --  print others according to IDL
            declare
               Class_Iter : Node_Id :=
                 OPN.Next_Node
                   (OPN.First_Node
                      (Class_List
                         (Classes
                            (Pn_Formalism_Specific_Informations
                               (Pn_Generated)))));
            begin
               while Present (Class_Iter) loop
                  case OPN.Kind (Class_Iter) is
                     when K_CPN_Formalism_Class_Item_Enum =>
                        if not Is_Empty (Enum (Class_Iter)) then
                           Write_Str
                             ("CM(11:declaration,1," &
                              OAV.Image
                                (New_Integer_Value (Line_Count),
                                 False) &
                              ",1,");
                           Set_Str_To_Name_Buffer
                             (Get_Name_String
                                (Name (Identifier (Class_Iter))) &
                              " is [");
                           declare
                              Enum_Iter : Node_Id :=
                                OPN.First_Node (Enum (Class_Iter));
                           begin
                              while Present (Enum_Iter) loop
                                 Add_Str_To_Name_Buffer
                                   (Get_Name_String
                                      (Name (Identifier (Enum_Iter))));
                                 if OPN.Next_Node (Enum_Iter) /= No_Node then
                                    Add_Str_To_Name_Buffer ("|");
                                 end if;
                                 --  next
                                 Enum_Iter := OPN.Next_Node (Enum_Iter);
                              end loop;
                              Add_Str_To_Name_Buffer ("];");
                              declare
                                 S : constant String :=
                                   Get_Name_String (Name_Find);
                              begin

                                 Write_Line
                                   (OAV.Image
                                      (New_Integer_Value (S'Length),
                                       False) &
                                    ":" &
                                    S &
                                    ")");
                                 Line_Count := Line_Count + 1;
                              end;
                           end;
                        end if;
                     when K_CPN_Formalism_Class_Item_Range =>
                        declare
                           --  hack since OAV.Image blast name_buffer....
                           Begin_Range : constant String :=
                             Get_Name_String (Name (Identifier (Class_Iter)));
                           Max_Range : constant String :=
                             OAV.Image (Get_Value_Type (High (Class_Iter)));
                           Min_Range : constant String :=
                             OAV.Image (Get_Value_Type (Low (Class_Iter)));
                        begin
                           Set_Str_To_Name_Buffer
                             (Begin_Range &
                              " is " &
                              Min_Range &
                              ".." &
                              Max_Range &
                              ";");
                           declare
                              S : constant String :=
                                Get_Name_String (Name_Find);
                           begin

                              Write_Line
                                ("CM(11:declaration, 1," &
                                 OAV.Image
                                   (New_Integer_Value (Line_Count),
                                    False) &
                                 ",1," &
                                 OAV.Image
                                   (New_Integer_Value (S'Length),
                                    False) &
                                 ":" &
                                 S &
                                 ")");
                              Line_Count := Line_Count + 1;
                           end;
                        end;

                     when others =>
                        null;
                  end case;
                  --  next
                  Class_Iter := OPN.Next_Node (Class_Iter);
               end loop;
            end;
         end if;
         --      print domains
         if not Is_Empty
             (Domains (Pn_Formalism_Specific_Informations (Pn_Generated)))
         then
            Write_Line
              ("CM(11:declaration,1," &
               OAV.Image (New_Integer_Value (Line_Count), False) &
               ",1,6:Domain)");
            Line_Count := Line_Count + 1;

            declare
               Domain_Iter : Node_Id :=
                 OPN.First_Node
                   (Domains
                      (Pn_Formalism_Specific_Informations (Pn_Generated)));
            begin
               while Present (Domain_Iter) loop
                  Write_Str
                    ("CM(11:declaration,1," &
                     OAV.Image (New_Integer_Value (Line_Count), False) &
                     ",1,");

                  Set_Str_To_Name_Buffer
                    (Get_Name_String (Name (Identifier (Domain_Iter))) &
                     " is <");
                  if not Is_Empty (Domain_List (Domain_Iter)) then
                     declare
                        Domain_Item : Node_Id :=
                          OPN.First_Node (Domain_List (Domain_Iter));
                     begin
                        while Present (Domain_Item) loop
                           Add_Str_To_Name_Buffer
                             (Get_Name_String
                                (Name (Identifier (Domain_Item))));
                           if OPN.Next_Node (Domain_Item) /= No_Node then
                              Add_Str_To_Name_Buffer (",");
                           end if;
                           --  next
                           Domain_Item := OPN.Next_Node (Domain_Item);
                        end loop;
                     end;
                  end if;
                  Add_Str_To_Name_Buffer (">;");
                  declare
                     S : constant String := Get_Name_String (Name_Find);
                  begin

                     Write_Line
                       (OAV.Image (New_Integer_Value (S'Length), False) &
                        ":" &
                        S &
                        ")");
                     Line_Count := Line_Count + 1;
                  end;

                  --  next
                  Domain_Iter := OPN.Next_Node (Domain_Iter);
               end loop;
            end;
         end if;
         --      print variables
         if not Is_Empty
             (Variables (Pn_Formalism_Specific_Informations (Pn_Generated)))
         then
            Write_Line
              ("CM(11:declaration,1," &
               OAV.Image (New_Integer_Value (Line_Count), False) &
               ",1,3:Var)");
            Line_Count := Line_Count + 1;

            declare
               Var_Iter : Node_Id :=
                 OPN.First_Node
                   (Variables
                      (Pn_Formalism_Specific_Informations (Pn_Generated)));
            begin
               while Present (Var_Iter) loop
                  if not Is_Empty (Variable_List (Var_Iter)) then
                     Write_Str
                       ("CM(11:declaration,1," &
                        OAV.Image (New_Integer_Value (Line_Count), False) &
                        ",1,");
                     Set_Str_To_Name_Buffer (" "); --  to clear name_buffer
                     declare
                        Var_Item : Node_Id :=
                          OPN.First_Node (Variable_List (Var_Iter));
                     begin
                        while Present (Var_Item) loop
                           Add_Str_To_Name_Buffer
                             (Get_Name_String (Name (Identifier (Var_Item))));
                           if OPN.Next_Node (Var_Item) /= No_Node then
                              Add_Str_To_Name_Buffer (", ");
                           end if;
                           --  next
                           Var_Item := OPN.Next_Node (Var_Item);
                        end loop;
                        Add_Str_To_Name_Buffer
                          (" in " &
                           Get_Name_String
                             (Name (Identifier (Class_Type (Var_Iter)))) &
                           ";");
                        declare
                           S : constant String := Get_Name_String (Name_Find);
                        begin

                           Write_Line
                             (OAV.Image (New_Integer_Value (S'Length), False) &
                              ":" &
                              S &
                              ")");
                           Line_Count := Line_Count + 1;
                        end;
                     end;
                  end if;
                  --  next
                  Var_Iter := OPN.Next_Node (Var_Iter);
               end loop;
            end;
         end if;
      end if;
   end Print_Formalism_Information;

end Ocarina.Backends.PN.Format.Cami;
