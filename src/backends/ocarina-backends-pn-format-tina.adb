------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . P N . F O R M A T . T I N A       --
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

with Output;
with Ocarina.Backends.PN.Nodes;
with Ocarina.Backends.PN.Nutils;
with Namet;
with Ocarina.AADL_Values;

package body Ocarina.Backends.PN.Format.Tina is

   package OPN renames Ocarina.Backends.PN.Nodes;
   package OPU renames Ocarina.Backends.PN.Nutils;
   package OAV renames Ocarina.AADL_Values;

   -------------------
   --  Print_Place  --
   -------------------

   procedure Print_Place (Pn_Generated : Node_Id;
                          Pn_P : Node_Id)
   is
      use Output;
      use OPN;
      use Namet;
      use OAV;

   begin

      if Pn_Generated /= No_Node then
         Write_Line ("pl "
                     & Get_Name_String (Name (Identifier (Pn_P)))
                     & " ("
                     & Image (Tokens_Number (Pn_P))
                     &")");
      end if;
   end Print_Place;

   -------------------
   --  Print_Trans  --
   -------------------

   procedure Print_Trans (Pn_Generated : Node_Id;
                          Pn_T : Node_Id)
   is
      use Output;
      use OPN;
      use Namet;
      use OAV;
      use OPU;

   begin
      if Pn_Generated /= No_Node then
         Write_Str ("tr "
                    & Get_Name_String (Name (Identifier (Pn_T))));
         declare
            BM : constant Value_Type := Get_Value_Type
              (OPN.Braces_Mode (OPN.Guard (Pn_T)));
            Braces : String := "[,]";
            Max_Guard : constant Value_Type := Get_Value_Type
              (Higher_Value (Guard (Pn_T)));
         begin
            case BM.Ival is
               --   0..3: [,] ; ],[ ; [,[ ; ],]
               when 0 =>
                  Braces := "[,]";
               when 1 =>
                  Braces := "],[";
               when 2 =>
                  Braces := "[,[";
               when 3 =>
                  Braces := "],]";
               when others =>
                  null;
            end case;
            if Max_Guard.Ival = -1 then
               --  infinity
               Braces := "[,[";
               Write_Str (" "
                          & Braces (1 .. 1)
                          & Image (Lower_Value (Guard (Pn_T)))
                          & Braces (2 .. 2)
                          & "w"
                          & Braces (3 .. 3)
                          & " ");
            else
               Write_Str (" "
                          & Braces (1 .. 1)
                          & Image (Lower_Value (Guard (Pn_T)))
                          & Braces (2 .. 2)
                          & Image (Higher_Value (Guard (Pn_T)))
                          & Braces (3 .. 3)
                          & " ");
            end if;
         end;

         --  print arcs
         declare
            Iter : Node_Id;
         begin
            --  in
            if not Is_Empty (Pn_Arcs_In (Pn_T)) then
               Iter := OPN.First_Node (Pn_Arcs_In (Pn_T));
               while Present (Iter) loop
                  Write_Str (Get_Name_String
                             (Name (Identifier (Pn_From (Iter))))
                             & " ");
                  Iter := OPN.Next_Node (Iter);
               end loop;
            end if;
            --  out
            if not Is_Empty (Pn_Arcs_Out (Pn_T)) then
               Write_Str ("-> ");
               Iter := OPN.First_Node (Pn_Arcs_Out (Pn_T));
               while Present (Iter) loop
                  Write_Str (Get_Name_String (Name (Identifier (Pn_To (Iter))))
                             & " ");
                  Iter := OPN.Next_Node (Iter);
               end loop;
            end if;
         end;
      end if;
      Write_Line ("");
   end Print_Trans;

   -----------------------------------
   --  Print_Formalism_Information  --
   -----------------------------------

   procedure Print_Formalism_Information
     (Pn_Generated : Node_Id)
   is
      use Output;
      use OPN;
      use Namet;
      use OAV;
      use OPU;

   begin
      if Pn_Generated /= No_Node then
         Write_Line ("net AADL_TO_TPN_GENERATED");

         --  print priorities
         declare
            Node_Iter : Node_Id;
         begin
            if not Is_Empty (Priorities
              (Pn_Formalism_Specific_Informations
               (Pn_Generated))) then
               Node_Iter := OPN.First_Node
                 (Priorities
                  (Pn_Formalism_Specific_Informations
                   (Pn_Generated)));
               while Present (Node_Iter) loop
                  declare
                     Prio_Iter : Node_Id;
                  begin
                     Prio_Iter := OPN.First_Node (Bounded_Trans (Node_Iter));
                     while Present (Prio_Iter) loop
                        declare
                           Iter2 : Node_Id;
                           Current_Prio : Value_Type;
                           Iter_Prio : Value_Type;
                        begin
                           Current_Prio := Get_Value_Type (Priority
                                                           (Prio_Iter));
                           Iter2 := OPN.Next_Node (Prio_Iter);
                           while Present (Iter2) loop
                              Iter_Prio := Get_Value_Type (Priority (Iter2));
                              if Current_Prio.Ival > Iter_Prio.Ival then
                                 Write_Line ("pr "
                                             & Get_Name_String
                                             (Name (Identifier (Prio_Iter)))
                                             & " > "
                                             & Get_Name_String
                                             (Name (Identifier (Iter2))));
                              end if;
                              --  next
                              Iter2 := OPN.Next_Node (Iter2);
                           end loop;
                        end;
                        --  next
                        Prio_Iter := OPN.Next_Node (Prio_Iter);
                     end loop;
                  end;
                  --  next
                  Node_Iter := OPN.Next_Node (Node_Iter);
               end loop;
            end if;
         end;
      end if;
   end Print_Formalism_Information;

end Ocarina.Backends.PN.Format.Tina;
