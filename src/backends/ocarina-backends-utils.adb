------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . U T I L S                --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.OS_Lib;
with Ada.Directories;
with GNAT.Table;

with Ocarina.Namet;
with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.Utils is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;
   package ADU renames Ocarina.Backends.Ada_Tree.Nutils;
   package ADV renames Ocarina.Backends.Ada_Values;

   use GNAT.OS_Lib;
   use Ada.Directories;

   use Ocarina.Namet;
   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Nutils;

   --  The entered directories stack

   package Directories_Stack is new GNAT.Table (Name_Id, Int, 1, 5, 10);

   function Get_Handling_Internal_Name
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind) return Name_Id;
   --  Code factorisation between Set_Handling and Get_Handling. This
   --  function computes an internal name used to store the handling
   --  information.

   function Map_Ada_Subprogram_Status_Name (S : Node_Id) return Name_Id;
   --  Maps an name for the record type corresponding to a hybrid
   --  subprogram.

   function Map_Ada_Call_Seq_Access_Name (S : Node_Id) return Name_Id;
   --  Maps an name for the subprogram access type corresponding to a
   --  hybrid subprogram.

   function Map_Ada_Call_Seq_Subprogram_Name
     (Spg : Node_Id;
      Seq : Node_Id) return Name_Id;
   --  Maps an name for the subprogram corresponding to a hybrid
   --  subprogram call sequence.

   type Repository_Entry is record
      E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind;
      A          : Node_Id;
   end record;
   --  One entry of the internal handling repository

   Recording_Requested : Boolean := False;

   package Handling_Repository is new GNAT.Table
     (Repository_Entry,
      Int,
      1,
      5,
      10);
   --  The internal handling repository

   procedure May_Be_Append_Handling_Entry
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind;
      A          : Node_Id);
   --  Add a new entry corresponding to the given parameters to the
   --  internal handling repository. The addition is only done in case
   --  the user requested explicitely the recording of handling

   function Bind_Transport_API_Internal_Name (P : Node_Id) return Name_Id;
   --  For code factorization purpose

   ----------------------
   -- Create_Directory --
   ----------------------

   procedure Create_Directory (Dir_Full_Name : Name_Id) is
      Dir_Full_String : constant String := Get_Name_String (Dir_Full_Name);
   begin
      if Is_Regular_File (Dir_Full_String)
        or else Is_Symbolic_Link (Dir_Full_String)
      then
         Display_Error
           ("Cannot create " &
            Dir_Full_String &
            " because there is a file with the same name",
            Fatal => True);
         return;
      end if;

      if Is_Directory (Dir_Full_String) then
         if Dir_Full_String /= "." then
            Display_Error
              (Dir_Full_String & " already exists",
               Fatal   => False,
               Warning => True);
         end if;
         return;
      end if;

      --  The directory name does not clash with anything, create it

      Create_Directory (Dir_Full_String);
   end Create_Directory;

   ---------------------
   -- Enter_Directory --
   ---------------------

   procedure Enter_Directory (Dirname : Name_Id) is
      use Directories_Stack;

      Current_Dir : constant Name_Id := Get_String_Name (Current_Directory);

   begin
      Increment_Last;
      Table (Last) := Current_Dir;
      Display_Debug_Message ("Left    : " & Get_Name_String (Current_Dir));
      Set_Directory (Get_Name_String (Dirname));
      Display_Debug_Message ("Entered : " & Get_Name_String (Dirname));
   end Enter_Directory;

   ---------------------
   -- Leave_Directory --
   ---------------------

   procedure Leave_Directory is
      use Directories_Stack;

      Last_Directory : constant Name_Id := Table (Last);

   begin
      Decrement_Last;
      Display_Debug_Message ("Left    : " & Current_Directory);
      Set_Directory (Get_Name_String (Last_Directory));
      Display_Debug_Message ("Entered : " & Get_Name_String (Last_Directory));
   end Leave_Directory;

   -----------------------------
   -- Add_Directory_Separator --
   -----------------------------

   function Add_Directory_Separator (Path : Name_Id) return Name_Id is
   begin
      Get_Name_String (Path);
      if Name_Buffer (Name_Len) /= Directory_Separator then
         Add_Char_To_Name_Buffer (Directory_Separator);
      end if;
      return Name_Find;
   end Add_Directory_Separator;

   --------------------------------
   -- Remove_Directory_Separator --
   --------------------------------

   function Remove_Directory_Separator (Path : Name_Id) return Name_Id is
   begin
      Get_Name_String (Path);

      if Name_Buffer (Name_Len) = Directory_Separator then
         Name_Len := Name_Len - 1;
      end if;
      return Name_Find;
   end Remove_Directory_Separator;

   ----------------------------------
   -- May_Be_Append_Handling_Entry --
   ----------------------------------

   procedure May_Be_Append_Handling_Entry
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind;
      A          : Node_Id)
   is
      package HR renames Handling_Repository;
      The_Entry : constant Repository_Entry :=
        Repository_Entry'
          (E => E, Comparison => Comparison, Handling => Handling, A => A);
   begin
      if Recording_Requested then
         HR.Increment_Last;
         HR.Table (HR.Last) := The_Entry;
      end if;
   end May_Be_Append_Handling_Entry;

   -------------------------------
   -- Start_Recording_Handlings --
   -------------------------------

   procedure Start_Recording_Handlings is
   begin
      if Recording_Requested then
         raise Program_Error
           with "Consecutive calls to Start_Recording_Handlings are forbidden";
      else
         Recording_Requested := True;
      end if;
   end Start_Recording_Handlings;

   ------------------------------
   -- Stop_Recording_Handlings --
   ------------------------------

   procedure Stop_Recording_Handlings is
   begin
      Recording_Requested := False;
   end Stop_Recording_Handlings;

   ---------------------
   -- Reset_Handlings --
   ---------------------

   procedure Reset_Handlings is
      package HR renames Handling_Repository;

      Index     : Int := HR.First;
      The_Entry : Repository_Entry;
   begin
      --  Disable the user handling request. It is important to do
      --  this at the beginning to avoid adding new entries when
      --  resetting.

      Recording_Requested := False;

      while Index <= HR.Last loop
         The_Entry := HR.Table (Index);

         --  Reset the handling information

         Set_Handling
           (The_Entry.E,
            The_Entry.Comparison,
            The_Entry.Handling,
            No_Node);

         Index := Index + 1;
      end loop;

      --  Deallocate and reinitialize the repository

      HR.Free;
      HR.Init;
   end Reset_Handlings;

   --------------------
   -- Normalize_Name --
   --------------------

   function Normalize_Name
     (Name      : Name_Id;
      Ada_Style : Boolean := False) return Name_Id
   is
      Normalized_Name : Name_Id;
   begin
      --  FIXME: The algorithm does not ensure a bijection between
      --  the input and the output. It should be improved.

      if Name = No_Name then
         Normalized_Name := Name;
      else
         declare
            Initial_Name : constant String := Get_Name_String (Name);
         begin
            Name_Len := 0;

            for Index in Initial_Name'First .. Initial_Name'Last loop
               if Initial_Name (Index) = '.' then
                  Add_Char_To_Name_Buffer ('_');
                  if Ada_Style then
                     Add_Char_To_Name_Buffer ('_');
                  end if;
               elsif Initial_Name (Index) = '-' then
                  Add_Char_To_Name_Buffer ('_');
                  if Ada_Style then
                     Add_Char_To_Name_Buffer ('_');
                  end if;
               elsif Initial_Name (Index) = ':' then
                  Add_Char_To_Name_Buffer ('_');
                  if Ada_Style then
                     Add_Char_To_Name_Buffer ('_');
                  end if;

               else
                  Add_Char_To_Name_Buffer (Initial_Name (Index));
               end if;
            end loop;

            Normalized_Name := Name_Find;
         end;
      end if;

      return Normalized_Name;
   end Normalize_Name;

   -----------------------------------
   -- Fully_Qualified_Instance_Name --
   -----------------------------------

   function Fully_Qualified_Instance_Name (E : Node_Id) return Name_Id is
      Current_Node : Node_Id := Parent_Subcomponent (E);
      Current_Name : Name_Id;

   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Normalize_Name (Name (Identifier (Current_Node))));
      Current_Name := Name_Find;
      Current_Node := Parent_Component (Current_Node);

      while Present (Current_Node) loop
         exit when No (Parent_Subcomponent (Current_Node));

         Get_Name_String
           (Normalize_Name
              (Name (Identifier (Parent_Subcomponent (Current_Node)))));
         Set_Str_To_Name_Buffer
           (Get_Name_String (Name_Find) &
            "_" &
            Get_Name_String (Current_Name));
         Current_Name := Name_Find;

         Current_Node := Parent_Component (Parent_Subcomponent (Current_Node));
      end loop;

      return Current_Name;
   end Fully_Qualified_Instance_Name;

   ------------------
   -- Is_Namespace --
   ------------------

   function Is_Namespace (N : Node_Id) return Boolean is
   begin
      return Kind (N) = K_Namespace_Instance;
   end Is_Namespace;

   ----------------
   -- Is_Delayed --
   ----------------

   function Is_Delayed (E : Node_Id) return Boolean is
      C : Node_Id;
      S : Node_Id;
   begin
      pragma Assert
        (Kind (E) = K_Port_Spec_Instance and then not Is_Event (E));

      if not AAU.Is_Empty (Sources (E)) then
         C := Extra_Item (First_Node (Sources (E)));

         case AADL_Version is
            when AADL_V1 =>
               if ATN.Category (Corresponding_Declaration (C)) =
                 Connection_Type'Pos (CT_Data_Delayed)
               then
                  return True;
               else
                  --  Recurse through the connection path

                  S := Item (First_Node (Sources (E)));

                  return S /= E
                    and then Kind (S) = K_Port_Spec_Instance
                    and then Is_Delayed (S);
               end if;

            when AADL_V2 =>
               if Get_Port_Timing (E) = Port_Timing_Delayed then
                  return True;
               else
                  --  Recurse through the connection path

                  S := Item (First_Node (Sources (E)));

                  return S /= E
                    and then Kind (S) = K_Port_Spec_Instance
                    and then Is_Delayed (S);
               end if;
         end case;
      end if;

      return False;
   end Is_Delayed;

   -----------------------
   -- Has_In_Parameters --
   -----------------------

   function Has_In_Parameters (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Parameter_Instance and then Is_In (F) then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_In_Parameters;

   ------------------------
   -- Has_Out_Parameters --
   ------------------------

   function Has_Out_Parameters (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Parameter_Instance and then Is_Out (F) then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_Out_Parameters;

   ------------------
   -- Has_In_Ports --
   ------------------

   function Has_In_Ports (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_In_Ports;

   ------------------------
   -- Has_In_Event_Ports --
   ------------------------

   function Has_In_Event_Ports (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance
              and then Is_In (F)
              and then Is_Event (F)
            then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_In_Event_Ports;

   -------------------
   -- Has_Out_Ports --
   -------------------

   function Has_Out_Ports (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_Out_Ports;

   -------------------------
   -- Has_Out_Event_Ports --
   -------------------------

   function Has_Out_Event_Ports (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance
              and then Is_Out (F)
              and then Is_Event (F)
            then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_Out_Event_Ports;

   ---------------
   -- Has_Ports --
   ---------------

   function Has_Ports (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_Ports;

   ----------------------
   -- Has_Output_Ports --
   ----------------------

   function Has_Output_Ports (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_Output_Ports;

   ---------------------
   -- Has_Input_Ports --
   ---------------------

   function Has_Input_Ports (E : Node_Id) return Boolean is
      F : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
               return True;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Has_Input_Ports;

   ---------------
   -- Has_Modes --
   ---------------

   function Has_Modes (E : Node_Id) return Boolean is
   begin
      pragma Assert (Kind (E) = K_Component_Instance);

      return not AAU.Is_Empty (Modes (E));
   end Has_Modes;

   ----------------------
   -- Get_Source_Ports --
   ----------------------

   function Get_Source_Ports (P : Node_Id) return List_Id is
      function Rec_Get_Source_Ports
        (P : Node_Id;
         B : Node_Id := No_Node) return List_Id;
      --  Recursive internal routine

      --------------------------
      -- Rec_Get_Source_Ports --
      --------------------------

      function Rec_Get_Source_Ports
        (P : Node_Id;
         B : Node_Id := No_Node) return List_Id
      is
         Result : constant List_Id := New_List (K_List_Id, No_Location);
         C      : Node_Id;
         S      : Node_Id;
         Bus    : Node_Id;
      begin
         if AAU.Is_Empty (Sources (P)) then
            AAU.Append_Node_To_List (Make_Node_Container (P, B), Result);
         end if;

         S := First_Node (Sources (P));

         while Present (S) loop
            if Kind (Item (S)) = K_Port_Spec_Instance
              and then Parent_Component (Item (S)) /= No_Node
              and then Is_Thread (Parent_Component (Item (S)))
            then
               --  We reached our end point, append it to the result list

               AAU.Append_Node_To_List
                 (Make_Node_Container (Item (S), B),
                  Result);
            elsif Kind (Item (S)) = K_Port_Spec_Instance
              and then Parent_Component (Item (S)) /= No_Node
              and then (Is_Process_Or_Device (Parent_Component (Item (S))))
            then

               if Is_In (Item (S)) then
                  --  See whether the connection to the process is
                  --  bound to a bus.

                  C := Extra_Item (S);

                  if No (C) then
                     --  There has been definitly a bug while
                     --  expanding connections.

                     raise Program_Error with "Wrong expansion of connections";
                  end if;

                  --  Get the bus of the connection

                  Bus := Get_Bound_Bus (C, False);
               else
                  Bus := No_Node;
               end if;

               if Present (B) and then Present (Bus) and then B /= Bus then
                  Display_Located_Error
                    (Loc (C),
                     "This connection is involved in a data flow" &
                     " mapped to several different buses",
                     Fatal => True);
               end if;

               --  Fetch recursively all the sources of S

               AAU.Append_Node_To_List
                 (First_Node (Rec_Get_Source_Ports (Item (S), Bus)),
                  Result);
            else
               Display_Located_Error
                 (Loc (P),
                  "This port has a source of a non supported kind",
                  Fatal => True);
            end if;

            S := Next_Node (S);
         end loop;

         return Result;
      end Rec_Get_Source_Ports;

   begin
      if AAU.Is_Empty (Sources (P)) then
         return No_List;
      else
         return Rec_Get_Source_Ports (P, No_Node);
      end if;
   end Get_Source_Ports;

   ---------------------------
   -- Get_Destination_Ports --
   ---------------------------

   function Get_Destination_Ports
     (P             : Node_Id;
      Custom_Parent : Node_Id := No_Node) return List_Id
   is

      function Rec_Get_Destination_Ports
        (P             : Node_Id;
         B             : Node_Id := No_Node;
         Custom_Parent : Node_Id := No_Node) return List_Id;
      --  Recursive internal routine

      -------------------------------
      -- Rec_Get_Destination_Ports --
      -------------------------------

      function Rec_Get_Destination_Ports
        (P             : Node_Id;
         B             : Node_Id := No_Node;
         Custom_Parent : Node_Id := No_Node) return List_Id
      is
         Result : constant List_Id := New_List (K_List_Id, No_Location);
         C      : Node_Id;
         D      : Node_Id;
         Bus    : Node_Id;
      begin
         D := First_Node (Destinations (P));

         while Present (D) loop
            if Kind (Item (D)) = K_Port_Spec_Instance
              and then Parent_Component (Item (D)) /= No_Node
              and then Is_Thread (Parent_Component (Item (D)))
            then
               --  We reached our end point, append it to the result list

               AAU.Append_Node_To_List
                 (Make_Node_Container (Item (D), B),
                  Result);

            elsif Kind (Item (D)) = K_Port_Spec_Instance
              and then Parent_Component (Item (D)) /= No_Node
              and then Is_Process (Parent_Component (Item (D)))
            then
               if Is_In (Item (D)) then
                  --  See whether the connection to the process is
                  --  bound to a bus.

                  C := Extra_Item (D);

                  if No (C) then
                     --  There has been definitly a bug while
                     --  expanding connections.

                     raise Program_Error with "Wrong expansion of connections";
                  end if;

                  --  Get the bus of the connection

                  Bus := Get_Bound_Bus (C, False);
               else
                  Bus := No_Node;
               end if;

               if Present (B) and then Present (Bus) and then B /= Bus then
                  Display_Located_Error
                    (Loc (C),
                     "This connection is involved in a data flow" &
                     " mapped to several different buses",
                     Fatal => True);
               end if;

               --  Fetch recursively all the destinations of D

               AAU.Append_Node_To_List
                 (First_Node (Rec_Get_Destination_Ports (Item (D), Bus)),
                  Result);

            elsif Kind (Item (D)) = K_Port_Spec_Instance
              and then Parent_Component (Item (D)) /= No_Node
              and then Is_Device (Parent_Component (Item (D)))
            then
               --  We reached our end point, append it to the result list

               AAU.Append_Node_To_List
                 (Make_Node_Container (Item (D), B),
                  Result);
            elsif Custom_Parent /= No_Node
              and then Is_Device (Custom_Parent)
              and then Get_Port_By_Name (P, Custom_Parent) /= No_Node
            then

               AAU.Append_Node_To_List
                 (First_Node
                    (Rec_Get_Destination_Ports
                       (Get_Port_By_Name (P, Custom_Parent),
                        B,
                        No_Node)),
                  Result);
            else
               Display_Located_Error
                 (Loc (P),
                  "This port has a destination of a non supported kind",
                  Fatal => True);
            end if;

            D := Next_Node (D);
         end loop;

         return Result;
      end Rec_Get_Destination_Ports;
   begin
      return Rec_Get_Destination_Ports (P, No_Node, Custom_Parent);
   end Get_Destination_Ports;

   ----------------------
   -- Get_Actual_Owner --
   ----------------------

   function Get_Actual_Owner (Spg_Call : Node_Id) return Node_Id is
      Spg            : constant Node_Id := Corresponding_Instance (Spg_Call);
      Data_Component : Node_Id;
      F              : Node_Id;
   begin
      --  If the subprogram call is not a method return No_Node

      if AAU.Is_Empty (Path (Spg_Call)) then
         return No_Node;
      end if;

      Data_Component := Item (First_Node (Path (Spg_Call)));

      --  Traverse all the required access of the subprogram instance
      --  and find the one corresponding to the owner data component.

      if not AAU.Is_Empty (Features (Spg)) then
         F := First_Node (Features (Spg));

         while Present (F) loop
            if Kind (F) = K_Subcomponent_Access_Instance then
               --  FIXME: We stop at the first met feature that
               --  corresponds to our criteria.

               --  The corresponding declaration of Data_Component is
               --  always a component type and not a component
               --  implementation. However the type of the feature F
               --  may be a component type as well as a component
               --  implementation. We test both cases.

               declare
                  Dcl_Data_Component : constant Node_Id :=
                    Corresponding_Declaration (Data_Component);
                  Dcl_F : constant Node_Id :=
                    Corresponding_Declaration (Corresponding_Instance (F));

                  use Ocarina.ME_AADL.AADL_Tree.Nodes;
               begin
                  exit when
                    (ATN.Kind (Dcl_F) = K_Component_Type
                     and then Dcl_F = Dcl_Data_Component)
                    or else
                    (ATN.Kind (Dcl_F) = K_Component_Implementation
                     and then
                       ATN.Corresponding_Entity
                         (ATN.Component_Type_Identifier (Dcl_F)) =
                       Dcl_Data_Component);
               end;
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      --  If no feature matched, raise an error

      if AAU.Is_Empty (Features (Spg)) or else No (F) then
         Display_Located_Error
           (Loc (Spg),
            "Feature subprogram has not access to its owner component",
            Fatal => True);
      end if;

      return Get_Subcomponent_Access_Source (F);
   end Get_Actual_Owner;

   ---------------------------
   -- Get_Container_Process --
   ---------------------------

   function Get_Container_Process (E : Node_Id) return Node_Id is
      pragma Assert (Present (E));
   begin
      case Kind (E) is
         when K_Call_Instance =>
            return Get_Container_Process (Parent_Sequence (E));

         when K_Call_Sequence_Instance | K_Subcomponent_Instance =>
            return Get_Container_Process (Parent_Component (E));

         when others =>
            if Is_Thread (E)
              or else Is_Subprogram (E)
              or else AAU.Is_Data (E)
            then
               return Get_Container_Process (Parent_Subcomponent (E));

            elsif Is_Process (E) or else Is_Device (E) then
               return Parent_Subcomponent (E);

            elsif Is_Abstract (E) then
               --  It is allowed for a thread to be part of an
               --  abstract component (e.g. a device driver). In this
               --  case, we cannot retrieve the corresponding process
               --  instance.

               return No_Node;

            else
               raise Program_Error
                 with "Wrong node kind in " &
                 "Get_Container_Process: " &
                 Kind (E)'Img &
                 " " &
                 Get_Category_Of_Component (E)'Img;

            end if;
      end case;
   end Get_Container_Process;

   --------------------------
   -- Get_Container_Thread --
   --------------------------

   function Get_Container_Thread (E : Node_Id) return Node_Id is
   begin
      case Kind (E) is
         when K_Call_Instance =>
            return Get_Container_Thread (Parent_Sequence (E));

         when K_Call_Sequence_Instance =>
            return Parent_Component (E);

         when others =>
            if Is_Subprogram (E) then
               return Get_Container_Thread (Parent_Subcomponent (E));
            else
               raise Program_Error
                 with "Wrong node kind in " &
                 "Get_Container_Thread: " &
                 Kind (E)'Img;
            end if;
      end case;
   end Get_Container_Thread;

   --------------------------------
   -- Get_Handling_Internal_Name --
   --------------------------------

   function Get_Handling_Internal_Name
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind) return Name_Id
   is
   begin
      case Comparison is
         when By_Name =>
            Get_Name_String (Map_Ada_Defining_Identifier (E));
         --  Get_Name_String (Compute_Full_Name_Of_Instance (E));

         when By_Node =>
            Set_Nat_To_Name_Buffer (Nat (E));
      end case;

      Add_Str_To_Name_Buffer ("%Handling%" & Handling'Img);

      return Name_Find;
   end Get_Handling_Internal_Name;

   ------------------
   -- Set_Handling --
   ------------------

   procedure Set_Handling
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind;
      A          : Node_Id)
   is
      Internal_Name : constant Name_Id :=
        Get_Handling_Internal_Name (E, Comparison, Handling);

   begin
      Set_Name_Table_Info (Internal_Name, Nat (A));
      May_Be_Append_Handling_Entry (E, Comparison, Handling, A);
   end Set_Handling;

   ------------------
   -- Get_Handling --
   ------------------

   function Get_Handling
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind) return Node_Id
   is
      Internal_Name : constant Name_Id :=
        Get_Handling_Internal_Name (E, Comparison, Handling);
   begin
      return Node_Id (Get_Name_Table_Info (Internal_Name));
   end Get_Handling;

   --------------------
   -- Bind_Two_Nodes --
   --------------------

   function Bind_Two_Nodes (N_1 : Node_Id; N_2 : Node_Id) return Node_Id is
      function Get_Binding_Internal_Name
        (N_1 : Node_Id;
         N_2 : Node_Id) return Name_Id;
      --  Return an internal name id useful for the binding

      -------------------------------
      -- Get_Binding_Internal_Name --
      -------------------------------

      function Get_Binding_Internal_Name
        (N_1 : Node_Id;
         N_2 : Node_Id) return Name_Id
      is
      begin
         Set_Nat_To_Name_Buffer (Nat (N_1));
         Add_Str_To_Name_Buffer ("%Binding%");
         Add_Nat_To_Name_Buffer (Nat (N_2));
         return Name_Find;
      end Get_Binding_Internal_Name;

      I_Name : constant Name_Id := Get_Binding_Internal_Name (N_1, N_2);
      N      : Node_Id;
   begin
      --  If the Bind_Two_Nodes has already been called on N_1 and
      --  N_1, return the result of the first call.

      if Get_Name_Table_Info (I_Name) /= 0 then
         return Node_Id (Get_Name_Table_Info (I_Name));
      end if;

      --  Otherwise, create a new binding node

      N := Make_Identifier (No_Location, No_Name, No_Name, No_Node);
      Set_Name_Table_Info (I_Name, Int (N));

      return N;
   end Bind_Two_Nodes;

   --------------------------------------
   -- Bind_Transport_API_Internal_Name --
   --------------------------------------

   function Bind_Transport_API_Internal_Name (P : Node_Id) return Name_Id is
   begin
      pragma Assert (Is_Process (P));

      Set_Nat_To_Name_Buffer (Nat (P));
      Add_Str_To_Name_Buffer ("%transport%layer%binding%");
      return Name_Find;
   end Bind_Transport_API_Internal_Name;

   ------------------------
   -- Bind_Transport_API --
   ------------------------

   procedure Bind_Transport_API (P : Node_Id; T : Supported_Transport_APIs) is
      I_Name : constant Name_Id := Bind_Transport_API_Internal_Name (P);
   begin
      Set_Name_Table_Byte (I_Name, Supported_Transport_APIs'Pos (T));
   end Bind_Transport_API;

   -------------------------
   -- Fetch_Transport_API --
   -------------------------

   function Fetch_Transport_API
     (P : Node_Id) return Supported_Transport_APIs
   is
      I_Name : constant Name_Id := Bind_Transport_API_Internal_Name (P);
   begin
      return Supported_Transport_APIs'Val (Get_Name_Table_Byte (I_Name));
   end Fetch_Transport_API;

   -------------------------------
   -- Map_Ada_Full_Feature_Name --
   -------------------------------

   function Map_Ada_Full_Feature_Name
     (E      : Node_Id;
      Suffix : Character := ASCII.NUL) return Name_Id
   is
   begin
      Get_Name_String
        (Compute_Full_Name_Of_Instance
           (Instance         => E,
            Display_Name     => True,
            Keep_Root_System => False));
      Get_Name_String (ADU.To_Ada_Name (Name_Find));

      if Suffix /= ASCII.NUL then
         Add_Str_To_Name_Buffer ('_' & Suffix);
      end if;

      return Name_Find;
   end Map_Ada_Full_Feature_Name;

   ----------------------------------
   -- Map_Ada_Data_Type_Designator --
   ----------------------------------

   function Map_Ada_Data_Type_Designator (E : Node_Id) return Node_Id is
      pragma Assert (AAU.Is_Data (E));

   begin
      return ADU.Extract_Designator
          (ADN.Type_Definition_Node (Backend_Node (Identifier (E))));
   end Map_Ada_Data_Type_Designator;

   ---------------------------------
   -- Map_Ada_Full_Parameter_Name --
   ---------------------------------

   function Map_Ada_Full_Parameter_Name
     (Spg    : Node_Id;
      P      : Node_Id;
      Suffix : Character := ASCII.NUL) return Name_Id
   is
   begin
      pragma Assert (Kind (P) = K_Parameter_Instance);

      if Kind (Spg) = K_Component_Instance and then Is_Subprogram (Spg) then
         Get_Name_String (Compute_Full_Name_Of_Instance (Spg, True));
      elsif Kind (Spg) = K_Call_Instance then
         Get_Name_String (Display_Name (Identifier (Spg)));
      else
         raise Program_Error with "Wrong subprogram kind";
      end if;

      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Display_Name (Identifier (P)));

      --  Convert the name to a valid Ada identifier name

      Get_Name_String (ADU.To_Ada_Name (Name_Find));

      if Suffix /= ASCII.NUL then
         Add_Str_To_Name_Buffer ('_' & Suffix);
      end if;

      return Name_Find;
   end Map_Ada_Full_Parameter_Name;

   -----------------------------
   -- Map_Ada_Enumerator_Name --
   -----------------------------

   function Map_Ada_Enumerator_Name
     (E      : Node_Id;
      Server : Boolean := False) return Name_Id
   is
      Ada_Name_1 : Name_Id;
      Ada_Name_2 : Name_Id;
   begin
      pragma Assert
        (Is_Subprogram (E) or else Kind (E) = K_Subcomponent_Instance);

      if Is_Subprogram (E)
        or else Is_Process (Corresponding_Instance (E))
        or else Is_Device (Corresponding_Instance (E))
      then
         --  For subprograms and processes, the enumerator name is
         --  mapped from the entity name.

         Get_Name_String (ADU.To_Ada_Name (Display_Name (Identifier (E))));
         Add_Str_To_Name_Buffer ("_K");

      elsif Is_Thread (Corresponding_Instance (E)) then
         --  For threads, the enumerator name is mapped from the
         --  containing process or abstract component name and the
         --  thread subcomponent name.

         --  Verifiy that the thread is a subcomponent of a process,
         --  or an abstract component (in the case of threads that
         --  belong to a device driver).

         pragma Assert
           (Is_Process (Parent_Component (E))
            or else Is_Abstract (Parent_Component (E)));

         if Is_Process (Parent_Component (E)) then
            Ada_Name_1 :=
              ADU.To_Ada_Name
                (Display_Name
                   (Identifier (Parent_Subcomponent (Parent_Component (E)))));

         elsif Is_Abstract (Parent_Component (E)) then
            Ada_Name_1 :=
              ADU.To_Ada_Name
                (Display_Name (Identifier (Parent_Component (E))));

         end if;

         Ada_Name_2 := ADU.To_Ada_Name (Display_Name (Identifier (E)));

         Get_Name_String (Ada_Name_1);
         Add_Char_To_Name_Buffer ('_');
         Get_Name_String_And_Append (Ada_Name_2);
         Add_Str_To_Name_Buffer ("_K");
      else
         raise Program_Error
           with "Wrong node kind for Map_Ada_Enumerator_Name " & Kind (E)'Img;
      end if;

      if Server then
         Add_Str_To_Name_Buffer ("_Server");
      end if;

      return Name_Find;
   end Map_Ada_Enumerator_Name;

   ---------------------------------
   -- Map_Ada_Defining_Identifier --
   ---------------------------------

   function Map_Ada_Defining_Identifier
     (A      : Node_Id;
      Suffix : String := "") return Name_Id
   is
      I         : Node_Id := A;
      N         : Node_Id := No_Node;
      J         : Node_Id;
      Name_List : List_Id;
   begin
      if Kind (A) /= K_Identifier then
         I := Identifier (A);
      end if;

      if Kind (A) = K_Component_Instance then
         N := Namespace (A);

      elsif Kind (A) = K_Subcomponent_Instance then
         if Present (Parent_Component (A)) then
            N := Namespace (Parent_Component (A));
         end if;
      end if;

      if N /= No_Node
        and then Display_Name (Identifier (N)) /= No_Name
        and then Get_Category_Of_Component (A) /= CC_Data
      then
         --  Use both namespace and identifier to build the Ada
         --  defining identifier, to avoid collisions in the Ada
         --  namespace.

         --  XXX Note: we do not handle data component types for now,
         --  as their mapping is unclear for now, see Code generation
         --  annex for more details.

         Name_List := AAU.Split_Name (N);

         J := First_Node (Name_List);

         if Present (J) then
            Get_Name_String (To_Ada_Name (Display_Name (J)));
            J := Next_Node (J);

            while Present (J) loop
               Add_Str_To_Name_Buffer
                 ("_" & Get_Name_String (Display_Name (J)));
               J := Next_Node (J);
            end loop;
         end if;
         Add_Str_To_Name_Buffer ("_" & Get_Name_String (Display_Name (I)));

      else
         Get_Name_String (To_Ada_Name (Display_Name (I)));
      end if;

      if Suffix /= "" then
         Add_Str_To_Name_Buffer ("_" & Suffix);
      end if;

      return Name_Find;
   end Map_Ada_Defining_Identifier;

   function Map_Ada_Defining_Identifier
     (A      : Node_Id;
      Suffix : String := "") return Node_Id
   is
   begin
      return Make_Defining_Identifier
          (Map_Ada_Defining_Identifier (A, Suffix));
   end Map_Ada_Defining_Identifier;

   ----------------------------
   -- Map_Ada_Component_Name --
   ----------------------------

   function Map_Ada_Component_Name (F : Node_Id) return Name_Id is
   begin
      Get_Name_String (To_Ada_Name (Display_Name (Identifier (F))));
      Add_Str_To_Name_Buffer ("_DATA");
      return Name_Find;
   end Map_Ada_Component_Name;

   --------------------------------------------
   -- Map_Ada_Protected_Aggregate_Identifier --
   --------------------------------------------

   function Map_Ada_Protected_Aggregate_Identifier
     (S : Node_Id;
      A : Node_Id) return Node_Id
   is
      S_Name : Name_Id;
      A_Name : Name_Id;
   begin
      pragma Assert
        (Kind (S) = K_Subcomponent_Access_Instance
         and then Kind (A) = K_Subcomponent_Instance);

      S_Name := To_Ada_Name (Display_Name (Identifier (S)));
      A_Name := To_Ada_Name (Display_Name (Identifier (A)));

      Get_Name_String (S_Name);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (A_Name);

      return Make_Defining_Identifier (Name_Find);
   end Map_Ada_Protected_Aggregate_Identifier;

   --------------------------------------
   -- Map_Ada_Default_Value_Identifier --
   --------------------------------------

   function Map_Ada_Default_Value_Identifier (D : Node_Id) return Node_Id is
      I : Node_Id;
   begin
      if Kind (D) /= K_Identifier then
         I := Identifier (D);
      end if;

      Get_Name_String (To_Ada_Name (Display_Name (I)));
      Add_Str_To_Name_Buffer ("_Default_Value");
      return Make_Defining_Identifier (Name_Find);
   end Map_Ada_Default_Value_Identifier;

   --------------------------------
   -- Map_Ada_Package_Identifier --
   --------------------------------

   function Map_Ada_Package_Identifier (E : Node_Id) return Node_Id is
      Port_Name   : Name_Id;
      Thread_Name : Name_Id;
   begin
      pragma Assert (AAU.Is_Data (E) or else Kind (E) = K_Port_Spec_Instance);

      if AAU.Is_Data (E) then
         Get_Name_String (To_Ada_Name (Display_Name (Identifier (E))));
      else
         Port_Name   := To_Ada_Name (Display_Name (Identifier (E)));
         Thread_Name :=
           To_Ada_Name
             (Display_Name
                (Identifier (Parent_Subcomponent (Parent_Component (E)))));
         Get_Name_String (Thread_Name);
         Add_Char_To_Name_Buffer ('_');
         Get_Name_String_And_Append (Port_Name);
      end if;

      Add_Str_To_Name_Buffer ("_Pkg");

      return Make_Defining_Identifier (Name_Find);
   end Map_Ada_Package_Identifier;

   -----------------------------------
   -- Map_Ada_Subprogram_Identifier --
   -----------------------------------

   function Map_Ada_Subprogram_Identifier (E : Node_Id) return Node_Id is
      pragma Assert
        (Is_Thread (E)
         or else Is_Subprogram (E)
         or else Kind (E) = K_Port_Spec_Instance);

      Spg_Name : Name_Id;

   begin
      if Is_Subprogram (E)
        and then Get_Source_Language (E) /= Language_Ada_95
      then
         Display_Located_Error
           (Loc (E),
            "This is not an Ada subprogram",
            Fatal => True);
      end if;

      --  Get the subprogram name

      if Is_Subprogram (E) then
         Spg_Name := Get_Source_Name (E);

      elsif Is_Thread (E) then
         Spg_Name := Get_Thread_Compute_Entrypoint (E);

      else
         Spg_Name := Get_Port_Compute_Entrypoint (E);
      end if;

      return Map_Ada_Subprogram_Identifier (Spg_Name);
   end Map_Ada_Subprogram_Identifier;

   -----------------------------------
   -- Map_Ada_Subprogram_Identifier --
   -----------------------------------

   function Map_Ada_Subprogram_Identifier (N : Name_Id) return Node_Id is
      P_Name : Name_Id;
      Result : Node_Id;
      D      : Node_Id;
   begin
      --  Get the package implementation and add the 'with' clause

      P_Name := Unit_Name (N);

      if P_Name = No_Name then
         Display_Error
           ("You must give the subprogram implementation name",
            Fatal => True);
      end if;

      D := Make_Designator (P_Name);
      ADN.Set_Corresponding_Node
        (ADN.Defining_Identifier (D),
         New_Node (ADN.K_Package_Specification));
      Add_With_Package (D);

      --  Get the full implementation name

      Get_Name_String (Local_Name (N));
      Result := Make_Defining_Identifier (Name_Find);
      Set_Homogeneous_Parent_Unit_Name (Result, D);
      return Result;
   end Map_Ada_Subprogram_Identifier;

   -----------------------------
   -- Map_Ada_Subprogram_Spec --
   -----------------------------

   function Map_Ada_Subprogram_Spec (S : Node_Id) return Node_Id is
      Profile : constant List_Id := ADU.New_List (ADN.K_Parameter_Profile);
      Param   : Node_Id;
      Mode    : Mode_Id;
      F       : Node_Id;
      N       : Node_Id;
      D       : Node_Id;
      Field   : Node_Id;
   begin
      pragma Assert (Is_Subprogram (S));

      --  We build the parameter profile of the subprogram instance by
      --  adding:

      --  First, the parameter features mapping

      if not AAU.Is_Empty (Features (S)) then
         F := First_Node (Features (S));

         while Present (F) loop
            if Kind (F) = K_Parameter_Instance then
               if Is_In (F) and then Is_Out (F) then
                  Mode := Mode_Inout;
               elsif Is_Out (F) then
                  Mode := Mode_Out;
               elsif Is_In (F) then
                  Mode := Mode_In;
               else
                  Display_Located_Error
                    (Loc (F),
                     "Unspecified parameter mode",
                     Fatal => True);
               end if;

               D := Corresponding_Instance (F);

               Param :=
                 ADU.Make_Parameter_Specification
                   (Map_Ada_Defining_Identifier (F),
                    Map_Ada_Data_Type_Designator (D),
                    Mode);

               ADU.Append_Node_To_List (Param, Profile);
            end if;

            F := Next_Node (F);
         end loop;
      end if;

      --  Second, the data access mapping. The data accesses are not
      --  mapped in the case of pure call sequence subprogram because
      --  they are used only to close the access chain.

      if Get_Subprogram_Kind (S) /= Subprogram_Pure_Call_Sequence then
         if not AAU.Is_Empty (Features (S)) then
            F := First_Node (Features (S));

            while Present (F) loop
               if Kind (F) = K_Subcomponent_Access_Instance then
                  case Get_Required_Data_Access (Corresponding_Instance (F)) is
                     when Access_Read_Only =>
                        Mode := Mode_In;
                     when Access_Write_Only =>
                        Mode := Mode_Out;
                     when Access_Read_Write =>
                        Mode := Mode_Inout;
                     when Access_None =>
                        --  By default, we allow read/write access

                        Mode := Mode_Inout;
                     when others =>
                        Display_Located_Error
                          (Loc (F),
                           "Unsupported required access",
                           Fatal => True);
                  end case;

                  D := Corresponding_Instance (F);

                  case Get_Data_Representation (D) is
                     when Data_Integer     |
                       Data_Boolean        |
                       Data_Float          |
                       Data_Fixed          |
                       Data_String         |
                       Data_Wide_String    |
                       Data_Character      |
                       Data_Wide_Character |
                       Data_Array          =>
                        --  If the data component is a simple data
                        --  component (not a structure), we simply add a
                        --  parameter with the computed mode and with a
                        --  type mapped from the data component.

                        Param :=
                          ADU.Make_Parameter_Specification
                            (Map_Ada_Defining_Identifier (F),
                             Map_Ada_Data_Type_Designator (D),
                             Mode);
                        ADU.Append_Node_To_List (Param, Profile);

                     when Data_Struct | Data_With_Accessors =>
                        --  If the data component is a complex data
                        --  component (which has subcomponents), we add a
                        --  parameter with the computed mode and with a
                        --  type mapped from each subcomponent type.

                        Field := First_Node (Subcomponents (D));

                        while Present (Field) loop
                           --  The parameter name is mapped from the
                           --  container data component and the data
                           --  subcomponent.

                           if AAU.Is_Data (Corresponding_Instance (Field)) then
                              Param :=
                                ADU.Make_Parameter_Specification
                                  (Map_Ada_Protected_Aggregate_Identifier
                                     (F,
                                      Field),
                                   Map_Ada_Data_Type_Designator
                                     (Corresponding_Instance (Field)),
                                   Mode);
                              ADU.Append_Node_To_List (Param, Profile);
                           end if;

                           Field := Next_Node (Field);
                        end loop;

                     when others =>
                        Display_Located_Error
                          (Loc (F),
                           "Unsupported data type",
                           Fatal => True);
                  end case;
               end if;

               F := Next_Node (F);
            end loop;
         end if;
      end if;

      --  Last, if the subprogram has OUT ports, we add an additional
      --  Status parameter.

      if Has_Out_Ports (S) then
         Param :=
           ADU.Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Status)),
              Extract_Designator
                (ADN.Type_Definition_Node (Backend_Node (Identifier (S)))),
              Mode_Inout);
         ADU.Append_Node_To_List (Param, Profile);
      end if;

      N :=
        ADU.Make_Subprogram_Specification
          (Map_Ada_Defining_Identifier (S),
           Profile,
           No_Node);

      --  If the program is an Opaque_C, we add the pragma Import
      --  instruction in the private part of the current package

      if Get_Subprogram_Kind (S) = Subprogram_Opaque_C then
         declare
            use ADN;

            P : constant Node_Id :=
              Make_Pragma_Statement
                (Pragma_Import,
                 Make_List_Id
                   (Make_Defining_Identifier (PN (P_C)),
                    Map_Ada_Defining_Identifier (S),
                    Make_Literal
                      (ADV.New_String_Value (Get_Source_Name (S)))));
         begin
            --  We must ensure that we are inside the scope of a
            --  package spec before inserting the pragma. In fact,
            --  Map_Ada_Subprogram_Spec is called also when we build
            --  the body of the subprogram, and we do not want to
            --  insert the pragma when building the body.

            if ADN.Kind (Current_Package) = K_Package_Specification then
               ADU.Append_Node_To_List (P, Private_Part (Current_Package));
            end if;
         end;
      end if;
      return N;
   end Map_Ada_Subprogram_Spec;

   -----------------------------
   -- Map_Ada_Subprogram_Body --
   -----------------------------

   function Map_Ada_Subprogram_Body (S : Node_Id) return Node_Id is
      Spec         : constant Node_Id := Map_Ada_Subprogram_Spec (S);
      Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
      Statements   : constant List_Id := New_List (ADN.K_Statement_List);

      Profile  : List_Id;
      N        : Node_Id;
      F        : Node_Id;
      Call_Seq : Node_Id;
   begin
      case Get_Subprogram_Kind (S) is
         when Subprogram_Empty =>
            --  An empty AADL subprogram is mapped into an Ada
            --  subprogram that raises an exception to warn the user.

            N :=
              Make_Exception_Declaration
                (Make_Defining_Identifier (EN (E_NYI)));
            ADU.Append_Node_To_List (N, Declarations);

            N := Make_Raise_Statement (Make_Defining_Identifier (EN (E_NYI)));
            ADU.Append_Node_To_List (N, Statements);

            return Make_Subprogram_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Opaque_C =>
            --  An opaque C AADL subprogram is a subprogram which is
            --  implemented by a C subprogram. We perform the mapping
            --  between the two subprograms using the Ada `Import'
            --  pragma in the specification. Therefore, we have
            --  nothing to do in the body.

            return No_Node;

         when Subprogram_Opaque_Ada_95 | Subprogram_Default =>
            --  An opaque Ada AADL subprogram is a subprogram which is
            --  implemented by an Ada subprogram. We perform the
            --  mapping between the two subprograms using the Ada
            --  renaming facility.

            --  Add the proper `with' clause

            N := Make_Designator (Unit_Name (Get_Source_Name (S)));
            Add_With_Package (N);

            --  Perform the renaming

            N :=
              Make_Designator
                (Local_Name (Get_Source_Name (S)),
                 Unit_Name (Get_Source_Name (S)));
            ADN.Set_Renamed_Entity (Spec, N);
            return Spec;

         when Subprogram_Opaque_Ada_95_Transfo =>
            --  Same as above, but does not with the package, because
            --  it is actually an instanciated generic package

            --  Perform the renaming

            N :=
              Make_Designator
                (Local_Name (Get_Transfo_Source_Name (S)),
                 Unit_Name (Get_Transfo_Source_Name (S)));
            ADN.Set_Renamed_Entity (Spec, N);
            return Spec;

         when Subprogram_Pure_Call_Sequence =>
            --  A pure call sequence subprogram is a subprogram that
            --  has exactly one call sequence. The behaviour of this
            --  subprogram is simply the call to the subprograms
            --  present in its call list.

            Handle_Call_Sequence
              (S,
               Make_Defining_Identifier (PN (P_Status)),
               First_Node (Calls (S)),
               Declarations,
               Statements);
            return ADU.Make_Subprogram_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Hybrid_Ada_95 =>
            --  Hybrid subprograms are subprograms that contain more
            --  that one call sequence.

            --  Declare the Status local variable

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (PN (P_Status)),
                 Object_Definition =>
                   Make_Defining_Identifier
                     (Map_Ada_Subprogram_Status_Name (S)));
            ADU.Append_Node_To_List (N, Declarations);

            --  Initialise the record fields that correspond to IN
            --  parameters.

            if not AAU.Is_Empty (Features (S)) then
               F := First_Node (Features (S));

               while Present (F) loop
                  if Kind (F) = K_Parameter_Instance and then Is_In (F) then
                     N :=
                       Make_Assignment_Statement
                         (Make_Designator
                            (To_Ada_Name (Display_Name (Identifier (F))),
                             PN (P_Status)),
                          Make_Designator
                            (To_Ada_Name (Display_Name (Identifier (F)))));
                     ADU.Append_Node_To_List (N, Statements);
                  end if;

                  F := Next_Node (F);
               end loop;
            end if;

            Profile := New_List (ADN.K_Parameter_Profile);

            --  Append the 'Status' variable to the call profile

            N := Make_Defining_Identifier (PN (P_Status));
            ADU.Append_Node_To_List (N, Profile);

            --  For each call sequence, we add the subprogram that
            --  handles it.

            Call_Seq := First_Node (Calls (S));

            while Present (Call_Seq) loop
               N :=
                 Make_Attribute_Designator
                   (Make_Defining_Identifier
                      (Map_Ada_Call_Seq_Subprogram_Name (S, Call_Seq)),
                    A_Access);
               ADU.Append_Node_To_List (N, Profile);

               Call_Seq := Next_Node (Call_Seq);
            end loop;

            --  Call the implementation subprogram

            --  Add the proper `with' clause

            N := Make_Designator (Unit_Name (Get_Source_Name (S)));
            Add_With_Package (N);

            N :=
              Make_Designator
                (Local_Name (Get_Source_Name (S)),
                 Unit_Name (Get_Source_Name (S)));

            N := Make_Subprogram_Call (ADN.Defining_Identifier (N), Profile);
            ADU.Append_Node_To_List (N, Statements);

            --  Update the OUT parameters from the corresponding
            --  record fields.

            if not AAU.Is_Empty (Features (S)) then
               F := First_Node (Features (S));

               while Present (F) loop
                  if Kind (F) = K_Parameter_Instance and then Is_Out (F) then
                     N :=
                       Make_Assignment_Statement
                         (Make_Designator
                            (To_Ada_Name (Display_Name (Identifier (F)))),
                          Make_Designator
                            (To_Ada_Name (Display_Name (Identifier (F))),
                             PN (P_Status)));
                     ADU.Append_Node_To_List (N, Statements);
                  end if;

                  F := Next_Node (F);
               end loop;
            end if;

            return Make_Subprogram_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Lustre =>
            --  In PolyORB-HI-Ada, a Lustre subprogram is mapped onto an Ada
            --  subprogram that raises an exception to warn the user.

            N :=
              Make_Exception_Declaration
                (Make_Defining_Identifier (EN (E_NYI)));
            ADU.Append_Node_To_List (N, Declarations);

            N := Make_Raise_Statement (Make_Defining_Identifier (EN (E_NYI)));
            ADU.Append_Node_To_List (N, Statements);

            return Make_Subprogram_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when others =>
            Display_Located_Error
              (Loc (S),
               "This kind of subprogram is not supported: " &
               Get_Subprogram_Kind (S)'Img,
               Fatal => True);
            return No_Node;
      end case;
   end Map_Ada_Subprogram_Body;

   --------------------------------------
   -- Map_Ada_Call_Seq_Subprogram_Spec --
   --------------------------------------

   function Map_Ada_Call_Seq_Subprogram_Spec
     (Spg : Node_Id;
      Seq : Node_Id) return Node_Id
   is
      Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
      N       : Node_Id;
   begin
      N :=
        Make_Parameter_Specification
          (Make_Defining_Identifier (PN (P_Status)),
           Make_Defining_Identifier (Map_Ada_Subprogram_Status_Name (Spg)),
           Mode_Inout);
      ADU.Append_Node_To_List (N, Profile);

      N :=
        Make_Subprogram_Specification
          (Make_Defining_Identifier
             (Map_Ada_Call_Seq_Subprogram_Name (Spg, Seq)),
           Profile);
      return N;
   end Map_Ada_Call_Seq_Subprogram_Spec;

   --------------------------------------
   -- Map_Ada_Call_Seq_Subprogram_Body --
   --------------------------------------

   function Map_Ada_Call_Seq_Subprogram_Body
     (Spg : Node_Id;
      Seq : Node_Id) return Node_Id
   is
      Spec : constant Node_Id := Map_Ada_Call_Seq_Subprogram_Spec (Spg, Seq);
      Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
      Statements   : constant List_Id := New_List (ADN.K_Statement_List);
   begin
      Handle_Call_Sequence
        (Spg,
         Make_Defining_Identifier (PN (P_Status)),
         Seq,
         Declarations,
         Statements);

      return Make_Subprogram_Implementation (Spec, Declarations, Statements);
   end Map_Ada_Call_Seq_Subprogram_Body;

   ------------------------------------
   -- Map_Ada_Subprogram_Status_Name --
   ------------------------------------

   function Map_Ada_Subprogram_Status_Name (S : Node_Id) return Name_Id is
   begin
      pragma Assert (Is_Subprogram (S) or else Kind (S) = K_Call_Instance);

      Get_Name_String (ADU.To_Ada_Name (Display_Name (Identifier (S))));
      Add_Str_To_Name_Buffer ("_Status");
      return Name_Find;
   end Map_Ada_Subprogram_Status_Name;

   --------------------------------------
   -- Map_Ada_Call_Seq_Subprogram_Name --
   --------------------------------------

   function Map_Ada_Call_Seq_Subprogram_Name
     (Spg : Node_Id;
      Seq : Node_Id) return Name_Id
   is
      Spg_Name : Name_Id;
      Seg_Name : Name_Id;
   begin
      pragma Assert
        (Is_Subprogram (Spg) and then Kind (Seq) = K_Call_Sequence_Instance);

      Spg_Name := ADU.To_Ada_Name (Display_Name (Identifier (Spg)));
      Seg_Name := ADU.To_Ada_Name (Display_Name (Identifier (Seq)));

      Get_Name_String (Spg_Name);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Seg_Name);
      return Name_Find;
   end Map_Ada_Call_Seq_Subprogram_Name;

   ----------------------------------
   -- Map_Ada_Call_Seq_Access_Name --
   ----------------------------------

   function Map_Ada_Call_Seq_Access_Name (S : Node_Id) return Name_Id is
      Spg_Name : Name_Id;
   begin
      pragma Assert (Is_Subprogram (S));

      Spg_Name := ADU.To_Ada_Name (Display_Name (Identifier (S)));

      Get_Name_String (Spg_Name);
      Add_Str_To_Name_Buffer ("_Sequence_Access");
      return Name_Find;
   end Map_Ada_Call_Seq_Access_Name;

   -----------------------------
   -- Map_Ada_Call_Seq_Access --
   -----------------------------

   function Map_Ada_Call_Seq_Access (S : Node_Id) return Node_Id is
      Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
      N       : Node_Id;
   begin
      N :=
        Make_Parameter_Specification
          (Make_Defining_Identifier (PN (P_Status)),
           Make_Defining_Identifier (Map_Ada_Subprogram_Status_Name (S)),
           Mode_Inout);
      ADU.Append_Node_To_List (N, Profile);

      N := Make_Subprogram_Specification (No_Node, Profile);

      N :=
        Make_Full_Type_Declaration
          (Make_Defining_Identifier (Map_Ada_Call_Seq_Access_Name (S)),
           Make_Access_Type_Definition (N));
      return N;
   end Map_Ada_Call_Seq_Access;

   -------------------------------
   -- Map_Ada_Subprogram_Status --
   -------------------------------

   function Map_Ada_Subprogram_Status (S : Node_Id) return Node_Id is
      Fields : constant List_Id := New_List (ADN.K_Component_List);
      F      : Node_Id;
      N      : Node_Id;
   begin
      pragma Assert (Is_Subprogram (S));

      if not AAU.Is_Empty (Features (S)) then
         F := First_Node (Features (S));

         while Present (F) loop
            N :=
              Make_Component_Declaration
                (Map_Ada_Defining_Identifier (F),
                 Map_Ada_Data_Type_Designator (Corresponding_Instance (F)));
            ADU.Append_Node_To_List (N, Fields);

            F := Next_Node (F);
         end loop;
      else
         Display_Located_Error
           (Loc (S),
            "This hybrid subprogram has no parameters",
            Fatal => True);
      end if;

      N :=
        Make_Full_Type_Declaration
          (Make_Defining_Identifier (Map_Ada_Subprogram_Status_Name (S)),
           Make_Record_Definition (Fields));
      return N;
   end Map_Ada_Subprogram_Status;

   --------------------------
   -- Handle_Call_Sequence --
   --------------------------

   procedure Handle_Call_Sequence
     (Caller       : Node_Id;
      Caller_State : Node_Id;
      Call_Seq     : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      Spg_Call      : Node_Id;
      Spg           : Node_Id;
      Destination_F : Node_Id;
      Source_F      : Node_Id;
      Source_Parent : Node_Id;
      Call_Profile  : List_Id;
      Param_Value   : Node_Id;
      Owner_Object  : Node_Id;
      N             : Node_Id;
      M             : Node_Id;
      F             : Node_Id;
      Parent        : Node_Id;
      Hybrid        : constant Boolean :=
        Is_Subprogram (Caller)
        and then Get_Subprogram_Kind (Caller) = Subprogram_Hybrid_Ada_95;
   begin
      --  The lists have to be created

      if Declarations = No_List or else Statements = No_List then
         raise Program_Error
           with "Lists have to be created before any call " &
           "to Handle_Call_Sequence";
      end if;

      --  The call sequence must contain at least one call to a
      --  subprogram.

      if AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
         Display_Located_Error
           (Loc (Call_Seq),
            "Empty call sequence",
            Fatal   => False,
            Warning => True);
         return;
      end if;

      Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

      while Present (Spg_Call) loop
         Spg := Corresponding_Instance (Spg_Call);

         Call_Profile := New_List (ADN.K_List_Id);

         if not AAU.Is_Empty (Features (Spg)) then
            F := First_Node (Features (Spg));

            while Present (F) loop
               if Kind (F) = K_Parameter_Instance and then Is_Out (F) then
                  --  Raise an error if the parameter is not connected
                  --  to any source.

                  if AAU.Length (Destinations (F)) = 0 then
                     Display_Located_Error
                       (Loc (F),
                        "This OUT parameter is not connected to" &
                        " any destination",
                        Fatal => True);
                  elsif AAU.Length (Destinations (F)) > 1 then
                     Display_Located_Error
                       (Loc (F),
                        "This OUT parameter has too many destinations",
                        Fatal => True);
                  end if;

                  --  At this point, we have a subprogram call
                  --  parameter that has exactly one destination.

                  Destination_F := Item (First_Node (Destinations (F)));

                  --  For each OUT parameter, we declare a local
                  --  variable if the OUT parameter is connected to
                  --  another subprogram call or if the caller is a
                  --  thread. Otherwise, we use the corresponding
                  --  caller subprogram parameter.

                  --  The parameter association value takes 4 possible
                  --  values (see the (1), (2), (3) and (4) comments
                  --  below.

                  if Is_Thread (Caller) then
                     --  Here we declare a variable based on the
                     --  thread feature name.

                     N :=
                       Make_Object_Declaration
                         (Defining_Identifier =>
                            Map_Ada_Defining_Identifier (Destination_F, "V"),
                          Object_Definition =>
                            Map_Ada_Data_Type_Designator
                              (Corresponding_Instance (Destination_F)));
                     ADU.Append_Node_To_List (N, Declarations);

                     --  (1) If we declared a local variable, we use it
                     --      as parameter value.

                     Param_Value :=
                       Map_Ada_Defining_Identifier (Destination_F, "V");

                  elsif Parent_Component (Destination_F) /= Caller then
                     --  Here, we map the variable name from the
                     --  subprogram *call* name and the feature
                     --  name. This avoids name clashing when a
                     --  subprogram calls twice the same subprogram.

                     N :=
                       Make_Object_Declaration
                         (Defining_Identifier =>
                            Make_Defining_Identifier
                              (Map_Ada_Full_Parameter_Name (Spg_Call, F)),
                          Object_Definition =>
                            Map_Ada_Data_Type_Designator
                              (Corresponding_Instance (F)));
                     ADU.Append_Node_To_List (N, Declarations);

                     --  (2) If we declared a local variable, we use it
                     --      as parameter value.

                     Param_Value :=
                       Make_Designator
                         (Map_Ada_Full_Parameter_Name (Spg_Call, F));

                  elsif Hybrid then
                     --  (3) If the calleD parameter is connected to
                     --      the calleR parameter and then the calleR
                     --      IS hybrid, then we use the 'Status'
                     --      record field corresponding to the calleR
                     --      parameter.

                     Param_Value :=
                       Make_Designator
                         (To_Ada_Name (Display_Name (Identifier (F))),
                          PN (P_Status));
                  else
                     --  (4) If the calleD parameter is connected to
                     --      the calleR parameter and then then calleR
                     --      is NOT hybrid, then we use simply the
                     --      corresponding parameter of the calleR.

                     Param_Value :=
                       Map_Ada_Defining_Identifier (Destination_F);
                  end if;

                  --  For each OUT parameter we build a parameter
                  --  association of the actual profile of the
                  --  implementation subprogram call <Param> =>
                  --  <Param_Value>.

                  N :=
                    Make_Parameter_Association
                      (Selector_Name    => Map_Ada_Defining_Identifier (F),
                       Actual_Parameter => Param_Value);
                  ADU.Append_Node_To_List (N, Call_Profile);

               elsif Kind (F) = K_Parameter_Instance and then Is_In (F) then
                  --  Raise an error if the parameter is not connected
                  --  to any source.

                  if AAU.Length (Sources (F)) = 0 then
                     Display_Located_Error
                       (Loc (F),
                        "This IN parameter is not connected to" &
                        " any source" &
                        Image (Loc (Caller)),
                        Fatal => True);
                  elsif AAU.Length (Sources (F)) > 1 then
                     Display_Located_Error
                       (Loc (F),
                        "This IN parameter has too many sources",
                        Fatal => True);
                  end if;

                  --  Here we have an IN parameter with exactly one
                  --  source.

                  Source_F := Item (First_Node (Sources (F)));

                  --  Get the source feature parent

                  Source_Parent := Parent_Component (Source_F);

                  --  The parameter value of the built parameter
                  --  association can take 4 different values (see
                  --  comments (1), (2), (3) and (4) below).

                  if Is_Thread (Source_Parent) then
                     --  (1) If the Parent of 'Source_F' is a thread,
                     --  then we use the local variable corresponding
                     --  to the IN port.

                     Param_Value :=
                       Map_Ada_Defining_Identifier (Source_F, "V");
                  elsif Source_Parent /= Caller then
                     --  (2) If the the source call is different from
                     --      the englobing subprogram, we use the
                     --      formerly declared variable.

                     Param_Value :=
                       Make_Designator
                         (Map_Ada_Full_Parameter_Name
                            (Parent_Subcomponent (Source_Parent),
                             Source_F));

                  elsif Hybrid then
                     --  (3) If the calleD parameter is connected to
                     --      the calleR parameter and then then calleR
                     --      IS hybrid, the we use the 'Status' record
                     --      field corresponding to the calleR
                     --      parameter.

                     Param_Value :=
                       Make_Selected_Component
                         (Make_Defining_Identifier (PN (P_Status)),
                          Map_Ada_Defining_Identifier (Source_F));
                  else
                     --  (4) If the calleD parameter is connected to
                     --      the calleR parameter and then then calleR
                     --      is NOT hybrid, then we use simply the
                     --      corresponding parameter of the calleR.

                     Param_Value := Map_Ada_Defining_Identifier (Source_F);
                  end if;

                  --  For each IN parameter we build a parameter
                  --  association association of the actual profile of
                  --  the implementaion subprogram call <Param> =>
                  --  <Param_Value>.

                  N :=
                    Make_Parameter_Association
                      (Selector_Name    => Map_Ada_Defining_Identifier (F),
                       Actual_Parameter => Param_Value);
                  ADU.Append_Node_To_List (N, Call_Profile);
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         if not AAU.Is_Empty (Path (Spg_Call)) then
            --  FIXME: Feature subprograms that have OUT ports are not
            --  supported yet.

            if Has_Out_Ports (Spg) then
               Display_Located_Error
                 (Loc (Spg),
                  "Feature subprograms that have OUT ports are not" &
                  " supported yet",
                  Fatal => True);
            end if;

            --  If this is a feature subprogram call, generate a call
            --  to the corresponding method.

            N := Message_Comment ("Invoking method");
            ADU.Append_Node_To_List (N, Statements);

            N :=
              Map_Ada_Defining_Identifier (Item (Last_Node (Path (Spg_Call))));

            --  Get the actual owner object

            --  FIXME: THIS WORKS ONLY FOR A LOCAL OBJECT

            Owner_Object := Get_Actual_Owner (Spg_Call);

            Set_Homogeneous_Parent_Unit_Name
              (N,
               Extract_Designator
                 (ADN.Object_Node (Backend_Node (Identifier (Owner_Object)))));

            N := Make_Subprogram_Call (N, Call_Profile);
            ADU.Append_Node_To_List (N, Statements);
         else
            --  If this is a classic subprogram, and if it has OUT
            --  ports, we declare an additional status variable and
            --  pass it to the implementation as the last INOUT
            --  parameter.

            if Has_Out_Ports (Spg) then
               N :=
                 Make_Object_Declaration
                   (Defining_Identifier =>
                      Make_Defining_Identifier
                        (Map_Ada_Subprogram_Status_Name (Spg_Call)),
                    Object_Definition =>
                      Extract_Designator
                        (ADN.Type_Definition_Node
                           (Backend_Node (Identifier (Spg)))));
               ADU.Append_Node_To_List (N, Declarations);

               N :=
                 Make_Parameter_Association
                   (Make_Defining_Identifier (PN (P_Status)),
                    Make_Defining_Identifier
                      (Map_Ada_Subprogram_Status_Name (Spg_Call)));
               ADU.Append_Node_To_List (N, Call_Profile);
            end if;

            --  Call the implementation.

            N := Message_Comment ("Call implementation");
            ADU.Append_Node_To_List (N, Statements);

            N :=
              Make_Subprogram_Call
                (Extract_Designator
                   (ADN.Subprogram_Node (Backend_Node (Identifier (Spg)))),
                 Call_Profile);
            ADU.Append_Node_To_List (N, Statements);

            --  After the implementation is called and if the called
            --  subprogram has OUT port, we trigger the destination of
            --  these ports, which are out ports of the containing
            --  thread or subprogram.

            if Has_Out_Ports (Spg) then
               F := First_Node (Features (Spg));

               while Present (F) loop
                  if Kind (F) = K_Port_Spec_Instance then
                     --  Verify whether the port has been triggered
                     --  then send the value to all its destinations.

                     declare
                        D       : Node_Id;
                        Profile : List_Id;
                        Aggr    : List_Id;
                        St      : constant List_Id :=
                          ADU.New_List (ADN.K_Statement_List);
                     begin
                        D := First_Node (Destinations (F));

                        while Present (D) loop
                           --  D is necessarily a feature of Caller,
                           --  otherwise we have a serious problem.

                           pragma Assert
                             (Parent_Component (Item (D)) = Caller);

                           Profile := ADU.New_List (ADN.K_List_Id);
                           ADU.Append_Node_To_List
                             (ADU.Copy_Node (Caller_State),
                              Profile);

                           Aggr := ADU.New_List (ADN.K_List_Id);

                           N :=
                             Make_Component_Association
                               (Make_Defining_Identifier (CN (C_Port)),
                                Map_Ada_Defining_Identifier (Item (D)));
                           ADU.Append_Node_To_List (N, Aggr);

                           if Ocarina.ME_AADL.AADL_Instances.Nodes.Is_Data
                               (Item (D))
                           then

                              N := Map_Ada_Defining_Identifier (F);

                              --  We do not put use clause to avoid
                              --  name clashing, so enumerators have
                              --  to be qualified.

                              M :=
                                Extract_Designator
                                  (ADN.Port_Enumeration_Node
                                     (Backend_Node (Identifier (Spg))));
                              Parent := ADN.Parent_Unit_Name (M);
                              N      := Make_Selected_Component (Parent, N);
                              N      :=
                                Make_Qualified_Expression
                                  (M,
                                   Make_Record_Aggregate (Make_List_Id (N)));

                              N :=
                                Make_Subprogram_Call
                                  (Extract_Designator
                                     (ADN.Get_Value_Node
                                        (Backend_Node (Identifier (Spg)))),
                                   Make_List_Id
                                     (Make_Defining_Identifier
                                        (Map_Ada_Subprogram_Status_Name
                                           (Spg_Call)),
                                      N));

                              N :=
                                Make_Component_Association
                                  (Make_Defining_Identifier
                                     (Map_Ada_Component_Name (Item (D))),
                                   Make_Selected_Component
                                     (N,
                                      Make_Defining_Identifier
                                        (Map_Ada_Component_Name (F))));
                              ADU.Append_Node_To_List (N, Aggr);
                           end if;

                           N :=
                             Make_Qualified_Expression
                               (Extract_Designator
                                  (ADN.Port_Interface_Node
                                     (Backend_Node (Identifier (Caller)))),
                                Make_Record_Aggregate (Aggr));
                           ADU.Append_Node_To_List (N, Profile);

                           --  Call, the Put_Value routine
                           --  corresponding to the destination.

                           N :=
                             Make_Subprogram_Call
                               (Extract_Designator
                                  (ADN.Put_Value_Node
                                     (Backend_Node (Identifier (Caller)))),
                                Profile);

                           ADU.Append_Node_To_List (N, St);

                           D := Next_Node (D);
                        end loop;

                        --  Make the if statement

                        Profile := ADU.New_List (ADN.K_List_Id);

                        N :=
                          Make_Defining_Identifier
                            (Map_Ada_Subprogram_Status_Name (Spg_Call));
                        ADU.Append_Node_To_List (N, Profile);

                        N := Map_Ada_Defining_Identifier (F);

                        --  We do not put use clause to avoid name
                        --  clashing, so enumerators have to be fully
                        --  qualified.

                        M :=
                          Extract_Designator
                            (ADN.Port_Enumeration_Node
                               (Backend_Node (Identifier (Spg))));
                        Parent := ADN.Parent_Unit_Name (M);

                        N := Make_Selected_Component (Parent, N);
                        N :=
                          Make_Qualified_Expression
                            (M,
                             Make_Record_Aggregate (Make_List_Id (N)));
                        ADU.Append_Node_To_List (N, Profile);

                        N :=
                          Make_Subprogram_Call
                            (Extract_Designator
                               (ADN.Get_Count_Node
                                  (Backend_Node (Identifier (Spg)))),
                             Profile);
                        N :=
                          Make_Expression
                            (N,
                             Op_Greater_Equal,
                             Make_Literal (ADV.New_Integer_Value (1, 1, 10)));
                        N :=
                          Make_If_Statement
                            (Condition       => N,
                             Then_Statements => St);
                        ADU.Append_Node_To_List (N, Statements);
                     end;
                  end if;

                  F := Next_Node (F);
               end loop;
            end if;
         end if;

         Spg_Call := Next_Node (Spg_Call);
      end loop;
   end Handle_Call_Sequence;

   ---------------------------
   -- Get_Ada_Default_Value --
   ---------------------------

   function Get_Ada_Default_Value (D : Node_Id) return Node_Id is
      Data_Representation : Supported_Data_Representation;
      Result              : Node_Id;
   begin
      pragma Assert (AAU.Is_Data (D));

      Data_Representation := Get_Data_Representation (D);

      case Data_Representation is
         when Data_Integer =>
            --  For integers, default value is 0

            Result := ADU.Make_Literal (ADV.New_Integer_Value (0, 1, 10));

         when Data_Float | Data_Fixed =>
            --  For reals, the default value is 0.0

            Result := ADU.Make_Literal (ADV.New_Floating_Point_Value (0.0));

         when Data_Boolean =>
            --  For booleans, the default value is FALSE

            Result := ADU.Make_Literal (ADV.New_Boolean_Value (False));

         when Data_Character =>
            --  For characters, the default value is the space ' '

            Result :=
              ADU.Make_Literal (ADV.New_Character_Value (Character'Pos (' ')));

         when Data_Wide_Character =>
            --  For wide characters, the default value is the wide
            --  space ' '.

            Result :=
              ADU.Make_Literal
                (ADV.New_Character_Value (Wide_Character'Pos (' '), True));

         when Data_String =>
            --  For strings, the default value is the null bounded string

            Result :=
              Make_Selected_Component
                (Map_Ada_Package_Identifier (D),
                 Make_Defining_Identifier (PN (P_Null_Bounded_String)));

         when Data_Wide_String =>
            --  For wide strings, the default value is the null
            --  bounded wide string.

            Result :=
              Make_Selected_Component
                (Map_Ada_Package_Identifier (D),
                 Make_Defining_Identifier (PN (P_Null_Bounded_Wide_String)));

         when Data_Array =>
            --  The default value for an array type is an array
            --  aggregate of the default value of the array element
            --  type.

            --  We use "<T>'Range =>" instead of using "others =>" to
            --  avoid implicit loops.

            Result :=
              Make_Record_Aggregate
                (Make_List_Id
                   (Make_Element_Association
                      (Make_Attribute_Designator
                         (Map_Ada_Defining_Identifier (D),
                          A_Range),
                       Get_Ada_Default_Value
                         (ATN.Entity (ATN.First_Node (Get_Base_Type (D)))))));

         when Data_Struct =>
            --  For data record, the default value is an aggregate
            --  list of default values of all the record aggregates.

            declare
               Aggregates : constant List_Id :=
                 ADU.New_List (ADN.K_Component_List);
               S : Node_Id;
               C : Node_Id;
            begin
               if not AAU.Is_Empty (Subcomponents (D)) then
                  S := First_Node (Subcomponents (D));
                  while Present (S) loop
                     C :=
                       ADU.Make_Component_Association
                         (Map_Ada_Defining_Identifier (S),
                          Get_Ada_Default_Value (Corresponding_Instance (S)));
                     ADU.Append_Node_To_List (C, Aggregates);

                     S := Next_Node (S);
                  end loop;

                  Result := ADU.Make_Record_Aggregate (Aggregates);
               else
                  Display_Located_Error
                    (Loc (D),
                     "Record types must not be empty!",
                     Fatal => True);
               end if;
            end;

         when Data_With_Accessors =>
            --  This is definitely a code generation error

            raise Program_Error
              with "Data types with accessors should" &
              " not have default values";

         when others =>
            Display_Located_Error
              (Loc (D),
               "Cannot generate default value for type",
               Fatal   => False,
               Warning => True);
            Result := No_Node;
      end case;

      return Result;
   end Get_Ada_Default_Value;

   -------------------------------------------
   -- Map_Ada_Namespace_Defining_Identifier --
   -------------------------------------------

   function Map_Ada_Namespace_Defining_Identifier
     (N      : Node_Id;
      Prefix : String := "") return Node_Id
   is
      Name_List : List_Id;
      I         : Node_Id;
      Id        : Node_Id;
      Parent_Id : Node_Id := No_Node;
   begin

      if Name (Identifier (N)) = No_Name then
         --  This is the unnamed namespace

         if Prefix = "" then
            --  Display an error if the user did not give a prefix

            raise Program_Error
              with "You must provide a prefix to map the" &
              " unnamed namespace";
         end if;

         return ADU.Make_Defining_Identifier (Get_String_Name (Prefix));
      else
         --  This is a "classical" namespace obtained from the
         --  instanciation of an AADL package.

         Name_List := Split_Name (N);

         if Prefix /= "" then
            Parent_Id :=
              ADU.Make_Defining_Identifier (Get_String_Name (Prefix));
         end if;

         I := First_Node (Name_List);

         while Present (I) loop
            Id := ADU.Make_Defining_Identifier (Display_Name (I));
            ADN.Set_Parent_Unit_Name (Id, Parent_Id);
            Parent_Id := Id;

            I := Next_Node (I);
         end loop;

         return Id;
      end if;
   end Map_Ada_Namespace_Defining_Identifier;

   -------------
   -- To_Bits --
   -------------

   How_Many_Bits : constant array (Size_Units) of Unsigned_Long_Long :=
     (Bit             => 1,
      Properties.Byte => 8,
      Kilo_Byte       => 8 * 1_000,
      Mega_Byte       => 8 * 1_000_000,
      Giga_Byte       => 8 * 1_000_000_000,
      Tera_Byte       => 8 * 1_000_000_000_000);
   --  To easily convert sizes into bits

   function To_Bits (S : Size_Type) return Unsigned_Long_Long is
   begin
      return S.S * How_Many_Bits (S.U);
   end To_Bits;

   ----------------
   -- To_Seconds --
   ----------------

   function To_Seconds (S : Time_Type) return Long_Double is
      Value : constant Long_Double := Long_Double (S.T);
   begin
      case S.U is
         when Picosecond =>
            return Value / 1_000_000_000_000.0;

         when Nanosecond =>
            return Value / 1_000_000_000.0;

         when Microsecond =>
            return Value / 1_000_000.0;

         when Millisecond =>
            return Value / 1_000.0;

         when Second =>
            return Value;

         when Minute =>
            return Value * 60.0;

         when Hour =>
            return Value * 3600.0;

      end case;
   end To_Seconds;

   ---------------------
   -- To_Milliseconds --
   ---------------------

   function To_Milliseconds (S : Time_Type) return Unsigned_Long_Long is
   begin
      return Unsigned_Long_Long (To_Seconds (S) * 1_000.0);
   end To_Milliseconds;

   ---------------------
   -- To_Nanoseconds --
   ---------------------

   function To_Nanoseconds (S : Time_Type) return Unsigned_Long_Long is
   begin
      return Unsigned_Long_Long (To_Seconds (S) * 1_000_000_000.0);
   end To_Nanoseconds;

   --------------
   -- To_Bytes --
   --------------

   How_Many_Bytes : constant array (Size_Units) of Unsigned_Long_Long :=
     (Bit             => 0,
      Properties.Byte => 1,
      Kilo_Byte       => 1_000,
      Mega_Byte       => 1_000_000,
      Giga_Byte       => 1_000_000_000,
      Tera_Byte       => 1_000_000_000_000);
   --  To easily convert sizes into bytes

   function To_Bytes (S : Size_Type) return Unsigned_Long_Long is
   begin
      case S.U is
         when Bit =>
            --  If the size can be converted into byte, we are OK,
            --  else, this is an error.

            if S.S mod 8 = 0 then
               return S.S / 8;
            else
               return 0;
            end if;

         when others =>
            return S.S * How_Many_Bytes (S.U);
      end case;
   end To_Bytes;

   ----------------------------------
   -- Check_Connection_Consistency --
   ----------------------------------

   procedure Check_Connection_Consistency (C : Node_Id) is
      B     : Node_Id;
      C_Src : Node_Id;
      C_Dst : Node_Id;
      P_Src : Node_Id;
      P_Dst : Node_Id;

      procedure Check_Port_Consistency (P : Node_Id);
      --  Check that a port belongs to a process and complains with an
      --  error otherwise.

      procedure Check_Processes_Bus_Access (P : Node_Id; Bus : Node_Id);
      --  Check that the process P have access to the bus 'Bus'
      --  through its bound processor.

      ----------------------------
      -- Check_Port_Consistency --
      ----------------------------

      procedure Check_Port_Consistency (P : Node_Id) is
      begin
         if not Is_Process (Parent_Component (P)) then
            Display_Located_Error
              (Loc (P),
               "The parent of this port is not a process and it" &
               " is involved in a system-level connection in " &
               Image (Loc (C)),
               Fatal => True);
         end if;
      end Check_Port_Consistency;

      --------------------------------
      -- Check_Processes_Bus_Access --
      --------------------------------

      procedure Check_Processes_Bus_Access (P : Node_Id; Bus : Node_Id) is
         CPU : Node_Id;
         F   : Node_Id := No_Node;
         S   : Node_Id;
      begin
         --  Get the processor to which P is bound

         CPU := Get_Bound_Processor (P);

         --  Loop on the features of CPU to find the required access
         --  to the Bus.

         if not AAU.Is_Empty (Features (CPU)) then
            F := First_Node (Features (CPU));
            Outer_Loop :
            while Present (F) loop
               if Kind (F) = K_Subcomponent_Access_Instance then
                  --  Verify that the required access is indeed connected to
                  --  the bus subcomponent correspondiong to Bus.

                  if not AAU.Is_Empty (Sources (F)) then
                     S := First_Node (Sources (F));

                     while Present (S) loop
                        exit Outer_Loop when Item (S) =
                          Parent_Subcomponent (B);

                        S := Next_Node (S);
                     end loop;
                  end if;
               end if;

               F := Next_Node (F);
            end loop Outer_Loop;
         end if;

         if No (F) then
            --  This means we went through all the previous loop
            --  without finding any matching bus access or that we did
            --  never enter the loop.

            Display_Located_Error
              (Loc (Parent_Subcomponent (CPU)),
               "This process has no access to the bus declared at " &
               Image (Loc (Parent_Subcomponent (Bus))),
               Fatal => True);
         end if;

      end Check_Processes_Bus_Access;

   begin
      pragma Assert (Kind (C) = K_Connection_Instance);

      --  We only check connection at system level

      if not Is_System (Parent_Component (C)) then
         return;
      end if;

      --  We only check port connections

      if not
        (Get_Category_Of_Connection (C) in Port_Connection_Type'Range)
      then
         return;
      end if;

      --  Get the connecion bus

      B := Get_Bound_Bus (C);

      --  Get the connection extremities

      C_Src := Get_Referenced_Entity (Source (C));
      C_Dst := Get_Referenced_Entity (Destination (C));

      --  Check that the connection connects two ports

      if Kind (C_Src) /= K_Port_Spec_Instance
        or else Kind (C_Src) /= K_Port_Spec_Instance
      then
         --  FIXME: May be refined in the future when distributed
         --  shared variable will be supported.

         Display_Located_Error
           (Loc (C),
            "One of the extremities of this connection is not a port",
            Fatal => True);
      end if;

      --  Check that the connected ports belongs to processes

      Check_Port_Consistency (C_Src);
      Check_Port_Consistency (C_Dst);

      --  Get the processes

      P_Src := Parent_Component (C_Src);
      P_Dst := Parent_Component (C_Dst);

      --  Check that the two processes have an access to the Bus to
      --  which the connection is bound through their respective bound
      --  processors.

      Check_Processes_Bus_Access (P_Src, B);
      Check_Processes_Bus_Access (P_Dst, B);

      --  Everything is OK
   end Check_Connection_Consistency;

   ------------------------------
   -- Check_Thread_Consistency --
   ------------------------------

   procedure Check_Thread_Consistency (T : Node_Id) is
   begin
      pragma Assert (Is_Thread (T));

      --  Check implementation kind

      if Get_Thread_Implementation_Kind (T) = Thread_Unknown then
         Display_Located_Error
           (Loc (T),
            "Unknown thread implementation kind",
            Fatal => True);
      end if;
   end Check_Thread_Consistency;

   ------------------------------------
   -- Get_Subcomponent_Access_Source --
   ------------------------------------

   function Get_Subcomponent_Access_Source (S : Node_Id) return Node_Id is
      Src : Node_Id;
   begin
      pragma Assert (Kind (S) = K_Subcomponent_Access_Instance);

      --  Raise an error if the provided access is not connected

      if AAU.Is_Empty (Sources (S)) then
         Display_Located_Error
           (Loc (S),
            "Required access not connected to anything",
            Fatal => True);
      end if;

      --  Loop on the sources of the access until finding a
      --  subcomponent.

      Src := First_Node (Sources (S));

      while Present (Src) loop

         exit when Kind (Item (Src)) = K_Subcomponent_Instance;

         --  Raise an error if the provided access is not connected

         if AAU.Is_Empty (Sources (Item (Src))) then
            Display_Located_Error
              (Loc (Item (Src)),
               "Required access not connected to anything",
               Fatal => True);
         end if;

         Src := First_Node (Sources (Item (Src)));
      end loop;

      --  If Src is No_Node, this means that the required access chain
      --  does not end with a subcomponent as stated by the AADL
      --  standard.

      if No (Src) then
         Display_Located_Error
           (Loc (S),
            "Required access chain does not end with a subcomponent",
            Fatal => True);
      end if;

      return Item (Src);
   end Get_Subcomponent_Access_Source;

   -------------------------
   -- Get_First_Processor --
   -------------------------

   function Get_First_Processor (P : Node_Id) return Node_Id is
      C : Node_Id;
   begin
      pragma Assert (Is_System (P));

      C := First_Node (Subcomponents (P));
      while Present (C) loop
         if Is_Processor (Corresponding_Instance (C)) then
            return C;
         end if;

         C := Next_Node (C);
      end loop;

      return No_Node;
   end Get_First_Processor;

   ----------------------------
   -- Get_Connection_Pattern --
   ----------------------------

   function Get_Connection_Pattern
     (E : Node_Id) return Connection_Pattern_Kind
   is
      L               : List_Id;
      N               : Node_Id;
      Current_Process : Node_Id;
      Remote_Process  : Node_Id;
      Return_Value    : Connection_Pattern_Kind;
   begin
      if Is_Process (Parent_Component (E)) then
         Current_Process := Parent_Component (E);
      elsif Is_Device (Parent_Component (E)) then
         Current_Process := Parent_Component (E);
      elsif Is_Thread (Parent_Component (E)) then
         Current_Process :=
           Get_Container_Process (Parent_Subcomponent (Parent_Component (E)));
      end if;

      Return_Value := Intra_Process;

      if Is_In (E) then
         L := Get_Source_Ports (E);
      else
         L := Get_Destination_Ports (E);
      end if;

      if L = No_List or else AAU.Is_Empty (L) then
         return Inter_Process;
      end if;

      N := First_Node (L);

      while Present (N) loop
         Remote_Process := Get_Container_Process (Parent_Component (Item (N)));

         if Remote_Process /= Current_Process then
            Return_Value := Inter_Process;
         end if;

         N := Next_Node (N);

      end loop;

      return Return_Value;
   end Get_Connection_Pattern;

   ------------------
   -- Compare_Time --
   ------------------

   function "<=" (T1 : Time_Type; T2 : Time_Type) return Boolean is
      T1_Value : Unsigned_Long_Long;
      T2_Value : Unsigned_Long_Long;
   begin
      case T1.U is
         when Picosecond | Nanosecond | Microsecond =>
            return True;
         --  At this time, we assume than our runtimes
         --  cannot handle a granularity less than the
         --  millisecond. So, we consider nanosecond,
         --  picosecond and microsecond equals.
         --  FIXME: Must be fixed later

         when Millisecond =>
            T1_Value := T1.T;

         when Second =>
            T1_Value := T1.T * 1000;

         when Minute =>
            T1_Value := T1.T * 60 * 1000;

         when Hour =>
            T1_Value := T1.T * 3600 * 1000;
      end case;

      case T2.U is
         when Picosecond | Nanosecond | Microsecond =>
            return True;
         --  At this time, we assume than our runtimes
         --  cannot handle a granularity less than the
         --  millisecond. So, we consider nanosecond,
         --  picosecond and microsecond equals.
         --  FIXME: Must be fixed later

         when Millisecond =>
            T2_Value := T2.T;

         when Second =>
            T2_Value := T2.T * 1000;

         when Minute =>
            T2_Value := T2.T * 60 * 1000;

         when Hour =>
            T2_Value := T2.T * 3600 * 1000;
      end case;

      return (T1_Value <= T2_Value);
   end "<=";

   -----------------------
   -- Get_Accessed_Data --
   -----------------------

   function Get_Accessed_Data (Data_Access : Node_Id) return Node_Id is
      Accessed_Component : Node_Id;
   begin
      if not AAU.Is_Empty (Sources (Data_Access)) then
         Accessed_Component := Item (First_Node (Sources (Data_Access)));
         if Kind (Accessed_Component) = K_Subcomponent_Access_Instance then
            return Get_Accessed_Data (Accessed_Component);
         else
            return Accessed_Component;
         end if;
      else
         return No_Node;
      end if;
   end Get_Accessed_Data;

   ---------------------------
   -- Get_Device_of_Process --
   ---------------------------

   function Get_Device_Of_Process
     (Bus     : Node_Id;
      Process : Node_Id) return Node_Id
   is
      The_Process : Node_Id := Process;

      The_System : Node_Id;
      S          : Node_Id;
      Device     : Node_Id;
   begin
      if not Is_Process (The_Process) then
         The_Process :=
           Corresponding_Instance
             (Get_Container_Process (Parent_Subcomponent (Process)));
      end if;

      The_System :=
        Parent_Component
          (Parent_Subcomponent
             (Corresponding_Instance (Parent_Subcomponent (The_Process))));

      if Present (Bus)
        and then not AAU.Is_Empty (Connections (The_System))
      then
         S := First_Node (Connections (The_System));

         --  Check whether a device is attached to this bus

         while Present (S) loop
            if Kind (S) = K_Connection_Instance
              and then Get_Category_Of_Connection (S) = CT_Access_Bus
            then
               Device := Item (First_Node (Path (Destination (S))));

               if True
                  --  We actually found a device

                 and then Present (Device)
                 and then AAU.Is_Device (Corresponding_Instance (Device))

                  --  Process and device are on the same processor

                 and then
                   Get_Bound_Processor (Corresponding_Instance (Device)) =
                   Get_Bound_Processor (The_Process)

                  --  This device is connected to the bus

                 and then
                   Parent_Subcomponent (Bus) =
                   Item (First_Node (Path (Source (S))))
               then
                  --  Note, for now, we assume there is only one
                  --  device at each end of the bus.

                  return Device;
               end if;
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      return No_Node;
   end Get_Device_Of_Process;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected (Bus : Node_Id; Device : Node_Id) return Boolean is
      The_System : Node_Id;
      S          : Node_Id;
   begin
      The_System :=
        Parent_Component
          (Parent_Subcomponent
             (Corresponding_Instance (Parent_Subcomponent (Bus))));

      pragma Assert (Is_System (The_System));

      if Present (Bus)
        and then not AAU.Is_Empty (Connections (The_System))
      then
         S := First_Node (Connections (The_System));

         --  Check whether Device is attached to this bus

         while Present (S) loop
            if Kind (S) = K_Connection_Instance
              and then Get_Category_Of_Connection (S) = CT_Access_Bus
            then
               if True
                  --  This device is connected to the bus

                 and then
                   Parent_Subcomponent (Bus) =
                   Item (First_Node (Path (Source (S))))

                 and then Device = Item (First_Node (Path (Destination (S))))
               then
                  --  Note, for now, we assume there is only one
                  --  device at each end of the bus.

                  return True;
               end if;
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      return False;
   end Is_Connected;

   -----------------------
   -- Is_Protected_Data --
   -----------------------

   function Is_Protected_Data (Data_Component : Node_Id) return Boolean is
      Concurrency_Protocol : constant Supported_Concurrency_Control_Protocol
        := Get_Concurrency_Protocol (Data_Component);
   begin
      return Concurrency_Protocol = Priority_Ceiling;
   end Is_Protected_Data;

   ----------------------
   -- Get_Port_By_Name --
   ----------------------

   function Get_Port_By_Name
     (Port      : Node_Id;
      Component : Node_Id) return Node_Id
   is
      F : Node_Id;
   begin
      if Component = No_Node then
         return No_Node;
      end if;

      F := First_Node (Features (Component));

      while Present (F) loop
         if Name (Identifier (F)) = Name (Identifier (Port)) then
            return F;
         end if;
         F := Next_Node (F);
      end loop;

      return No_Node;
   end Get_Port_By_Name;

   ----------------
   -- Is_Virtual --
   ----------------

   function Is_Virtual (Port : Node_Id) return Boolean is
      Remote_Port      : Node_Id;
      Remote_Processor : Node_Id;
      Local_Processor  : Node_Id;
   begin
      if Port = No_Node then
         return False;
      end if;

      if Is_In (Port) and then not Is_Out (Port) then
         if AAU.Is_Empty (Sources (Port)) then
            Display_Error ("IN ports must be connected !", Fatal => True);
         end if;
         Remote_Port := Item (First_Node (Sources (Port)));
      elsif Is_Out (Port) and then not Is_In (Port) then
         if AAU.Is_Empty (Destinations (Port)) then
            Display_Error ("IN ports must be connected !", Fatal => True);
         end if;
         Remote_Port := Item (First_Node (Destinations (Port)));
      else
         Display_Error
           ("Virtual port cannot be IN and OUT at the same time",
            Fatal => True);
      end if;

      Local_Processor :=
        Parent_Component
          (Parent_Subcomponent
             (Get_Bound_Processor (Parent_Component (Port))));
      Remote_Processor :=
        Parent_Component
          (Parent_Subcomponent
             (Get_Bound_Processor (Parent_Component (Remote_Port))));

      return Is_Device (Parent_Component (Port))
        and then (Local_Processor /= Remote_Processor);
   end Is_Virtual;

   -----------------------------------------------------
   --  Get_Instance_Type_Associated_With_Virtual_Bus  --
   -----------------------------------------------------

   function Get_Instance_Type_Associated_With_Virtual_Bus
     (Port : Node_Id) return Node_Id
   is
      Virtual_Buses  : List_Id;
      Virtual_Bus    : Node_Id;
      Implementation : Node_Id;
      Property_Node  : Node_Id;
      Tmp_Node       : Node_Id;
      VB_Type        : Node_Id;
   begin
      Virtual_Buses := Get_Associated_Virtual_Buses (Port);

      if Virtual_Buses = No_List then
         return No_Node;
      end if;

      Tmp_Node := ATN.First_Node (Virtual_Buses);

      while Present (Tmp_Node) loop
         Virtual_Bus   := ATN.Entity (Tmp_Node);
         Property_Node :=
           Look_For_Property_In_Declarative (Virtual_Bus, "implemented_as");

         --  Now, we are trying to catch the value of the "Implemented_As"
         --  property of the virtual bus. The virtual bus should have
         --  a property Implemented_As with an abstract component that
         --  describe how the virtual bus is implemented (subprograms, data,
         --  ...). Then, we will browse this abstract component to call
         --  the right functions.

         if Property_Node = No_Node then
            exit;
         end if;

         Implementation := ATN.Entity (ATN.Single_Value (Property_Node));

         --  Here, get the type we use to marshall data

         VB_Type :=
           Look_For_Subcomponent_In_Declarative
             (Implementation,
              "marshalling_type");

         if VB_Type /= No_Node
           and then ATN.Default_Instance (VB_Type) /= No_Node
         then
            return ATN.Default_Instance (VB_Type);
         end if;

         Tmp_Node := ATN.Next_Node (Tmp_Node);
      end loop;

      return No_Node;
   end Get_Instance_Type_Associated_With_Virtual_Bus;

   ----------------------------------
   -- Get_Associated_Virtual_Buses --
   ----------------------------------

   function Get_Associated_Virtual_Buses (Port : Node_Id) return List_Id is
      Port_Spec     : Node_Id;
      Property_Node : Node_Id;
   begin
      Port_Spec     := Corresponding_Declaration (Port);
      Property_Node :=
        Look_For_Property_In_Declarative
          (Port_Spec,
           "allowed_connection_binding_class");

      --  We are looking for the Allowed_Connection_Binding_Class
      --  on the port. A list of virtual bus should be associated
      --  to this port. These virtual buses potentially describe
      --  the protocols we use to pack/unpack data before/after
      --  network send.

      if Property_Node /= No_Node
        and then ATN.Expanded_Multi_Value (Property_Node) /= No_List
      then
         return ATN.Expanded_Multi_Value (Property_Node);
      else
         return No_List;
      end if;
   end Get_Associated_Virtual_Buses;

   ---------------------------------------
   -- Look_For_Property_In_Declarative  --
   ---------------------------------------

   function Look_For_Property_In_Declarative
     (Component     : Node_Id;
      Property_Name : String) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      Tmp                   : Node_Id;
      Property_In_Type      : Node_Id := No_Node;
      Property_In_Inherited : Node_Id := No_Node;
   begin
      Tmp :=
        Find_Property_Association_From_Name
          (ATN.Properties (Component),
           Property_Name,
           No_Name);
      if Tmp /= No_Node then
         return ATN.Property_Association_Value (Tmp);
      else
         if ATN.Kind (Component) = ATN.K_Component_Implementation then
            Property_In_Type :=
              Look_For_Property_In_Declarative
                (ATN.Corresponding_Entity
                   (ATN.Component_Type_Identifier (Component)),
                 Property_Name);
         end if;

         if
           (ATN.Kind (Component) = ATN.K_Component_Type
            or else ATN.Kind (Component) = ATN.K_Component_Implementation)
           and then ATN.Parent (Component) /= No_Node
         then
            Property_In_Inherited :=
              Look_For_Property_In_Declarative
                (ATN.Entity (ATN.Parent (Component)),
                 Property_Name);
         end if;
      end if;

      if Property_In_Type /= No_Node then
         return Property_In_Type;
      end if;

      if Property_In_Inherited /= No_Node then
         return Property_In_Inherited;
      end if;
      return No_Node;
   end Look_For_Property_In_Declarative;

   -------------------------------------------
   -- Look_For_Subcomponent_In_Declarative  --
   -------------------------------------------

   function Look_For_Subcomponent_In_Declarative
     (Component         : Node_Id;
      Subcomponent_Name : String) return Node_Id
   is
      Tmp_Node       : Node_Id;
      Subcomponent   : Node_Id;
      Subcomponent_N : Name_Id;
   begin
      Subcomponent_N := Get_String_Name (Subcomponent_Name);

      if not ATU.Is_Empty (ATN.Subcomponents (Component)) then
         Tmp_Node := ATN.First_Node (ATN.Subcomponents (Component));
         while Present (Tmp_Node) loop
            Subcomponent := ATN.Entity (ATN.Entity_Ref (Tmp_Node));

            if ATN.Display_Name (ATN.Identifier (Tmp_Node)) =
              Subcomponent_N
            then
               return Subcomponent;
            end if;
            Tmp_Node := ATN.Next_Node (Tmp_Node);
         end loop;
      end if;

      return No_Node;
   end Look_For_Subcomponent_In_Declarative;

   --------------------------
   -- Is_Pure_Device_Port  --
   --------------------------

   function Is_Pure_Device_Port (Port : Node_Id) return Boolean is
      Other_Ports : List_Id;
      Tmp         : Node_Id;
   begin
      if Port = No_Node then
         return False;
      end if;

      if not Is_Device (Parent_Component (Port)) then
         return False;
      end if;

      if Is_In (Port) and then Is_Out (Port) then
         Display_Error
           ("Is_Communicating_With_Device does not work on in out port",
            Fatal => True);
      end if;

      if Is_In (Port) then
         Other_Ports := Sources (Port);
      else
         Other_Ports := Destinations (Port);
      end if;

      Tmp := First_Node (Other_Ports);

      while Present (Tmp) loop
         if Is_Device (Parent_Component (Item (Tmp))) then
            return True;
         end if;
         Tmp := Next_Node (Tmp);
      end loop;
      return False;
   end Is_Pure_Device_Port;

   ----------------------------
   --  Is_Using_Virtual_Bus  --
   ----------------------------

   function Is_Using_Virtual_Bus (Port : Node_Id) return Boolean is

      type Browsing_Kind is (By_Source, By_Destination);

      function Is_Using_Virtual_Bus_Rec
        (Port   : Node_Id;
         Method : Browsing_Kind) return Boolean
      is
         Source_Port      : Node_Id;
         Destination_Port : Node_Id;
         Tmp              : Node_Id;
      begin

         if Get_Associated_Virtual_Buses (Port) /= No_List then
            return True;
         end if;

         if AAU.Is_Thread (Parent_Component (Port)) then
            return False;
         end if;
         --  We are now in the applicative domain, no need
         --  to browse further the component hierarchy.

         if Method = By_Source
           and then not AAU.Is_Empty (AIN.Sources (Port))
         then
            Tmp := AIN.First_Node (AIN.Sources (Port));

            while Present (Tmp) loop
               Source_Port := AIN.Item (Tmp);

               if Is_Using_Virtual_Bus_Rec (Source_Port, Method) then
                  return True;
               end if;

               Tmp := AIN.Next_Node (Tmp);

            end loop;
         end if;

         if Method = By_Destination
           and then not AAU.Is_Empty (AIN.Destinations (Port))
         then
            Tmp := AIN.First_Node (AIN.Destinations (Port));

            while Present (Tmp) loop
               Destination_Port := AIN.Item (Tmp);

               if Is_Using_Virtual_Bus_Rec (Destination_Port, Method) then
                  return True;
               end if;

               Tmp := AIN.Next_Node (Tmp);

            end loop;
         end if;

         return False;

      end Is_Using_Virtual_Bus_Rec;
   begin
      if not AIN.Is_Data (Port) then
         raise Program_Error
           with "Call to Is_Using_Virtual_Bus with non DATA port";
      end if;

      if AIN.Is_In (Port) then
         return Is_Using_Virtual_Bus_Rec (Port, By_Source);
      else
         return Is_Using_Virtual_Bus_Rec (Port, By_Destination);
      end if;

   end Is_Using_Virtual_Bus;

   -------------------------------------------
   --  Get_Corresponding_Port_In_Component  --
   -------------------------------------------

   function Get_Corresponding_Port_In_Component
     (Port : Node_Id) return Node_Id
   is
      function Get_Corresponding_Port_In_Component_Rec
        (Port            : Node_Id;
         Parent_Searched : Node_Id;
         Method          : Browsing_Kind) return Node_Id
      is
         L : List_Id;
         T : Node_Id;
         R : Node_Id;
      begin
         if Parent_Component (Port) = Parent_Searched then
            return Port;
         end if;

         if Method = By_Destination then
            L := Destinations (Port);
         else
            L := Sources (Port);
         end if;

         if AAU.Is_Empty (L) then
            return No_Node;
         end if;

         T := First_Node (L);

         while Present (T) loop
            R :=
              Get_Corresponding_Port_In_Component_Rec
                (Item (T),
                 Parent_Searched,
                 Method);

            if R /= No_Node then
               return R;
            end if;

            T := Next_Node (T);
         end loop;

         return No_Node;
      end Get_Corresponding_Port_In_Component_Rec;
      Parent    : Node_Id;
      Tmp       : Node_Id;
      R         : Node_Id;
      L         : List_Id;
      My_Method : Browsing_Kind;
   begin
      Parent := Parent_Component (Port);

      if Is_In (Port) then
         L         := Destinations (Port);
         My_Method := By_Destination;
      else
         L         := Sources (Port);
         My_Method := By_Source;
      end if;

      if AAU.Is_Empty (L) then
         return No_Node;
      end if;

      Tmp := First_Node (L);

      while Present (Tmp) loop
         R :=
           Get_Corresponding_Port_In_Component_Rec
             (Item (Tmp),
              Parent,
              My_Method);
         if R /= No_Node then
            return R;
         end if;
         Tmp := Next_Node (Tmp);
      end loop;

      return No_Node;
   end Get_Corresponding_Port_In_Component;

   -----------------------------------
   --  Process_Use_Default_Sockets  --
   -----------------------------------

   function Process_Use_Defaults_Sockets
     (The_Process : Node_Id) return Boolean
   is
      C        : Node_Id;
      F        : Node_Id;
      B        : Node_Id;
      C_End    : Node_Id;
      End_List : List_Id;
   begin
      if not AAU.Is_Empty (Features (The_Process)) then
         F := First_Node (Features (The_Process));

         while Present (F) loop

            --  We make two iteration to traverse (1) the sources
            --  of F then (2) the destinations of F.

            End_List := Sources (F);

            for I in Boolean'Range loop
               if not AAU.Is_Empty (End_List) then
                  C_End := First_Node (End_List);

                  while Present (C_End) loop

                     --  Get the connection involving C_End

                     C := Extra_Item (C_End);

                     --  Get the bus of the connection

                     B := Get_Bound_Bus (C);

                     if Get_Transport_API (B, The_Process) =
                       Transport_BSD_Sockets
                     then
                        return True;
                     end if;

                     C_End := Next_Node (C_End);
                  end loop;
               end if;

               End_List := Destinations (F);
            end loop;

            F := Next_Node (F);
         end loop;
      end if;

      return False;
   end Process_Use_Defaults_Sockets;

   --------------------------
   --  Get_Associated_Bus  --
   --------------------------

   function Get_Associated_Bus (Port : Node_Id) return Node_Id is
      C               : Node_Id;
      F               : Node_Id;
      B               : Node_Id;
      C_End           : Node_Id;
      End_List        : List_Id;
      Ports_To_Browse : List_Id;
   begin
      if Is_In (Port) then
         Ports_To_Browse := Sources (Port);
      else
         Ports_To_Browse := Destinations (Port);
      end if;

      if not AAU.Is_Empty (Ports_To_Browse) then
         F := First_Node (Ports_To_Browse);

         while Present (F) loop

            --  We make two iteration to traverse (1) the sources
            --  of F then (2) the destinations of F.

            End_List := Sources (Item (F));

            for I in Boolean'Range loop
               if not AAU.Is_Empty (End_List) then
                  C_End := First_Node (End_List);

                  while Present (C_End) loop

                     --  Get the connection involving C_End

                     C := Extra_Item (C_End);

                     --  Get the bus of the connection

                     B := Get_Bound_Bus (C);

                     if B /= No_Node then
                        return B;
                     end if;

                     C_End := Next_Node (C_End);
                  end loop;
               end if;

               End_List := Destinations (Item (F));
            end loop;

            F := Next_Node (F);
         end loop;
      end if;

      return No_Node;
   end Get_Associated_Bus;

   ------------------------
   -- Get_Root_Component --
   ------------------------

   function Get_Root_Component (C : Node_Id)
                               return Node_Id is
   begin
      if (Parent_Subcomponent (C) = No_Node) then
         return C;
      end if;

      return Get_Root_Component
         (Parent_Component (Parent_Subcomponent (C)));
   end Get_Root_Component;

   -----------------------------
   -- Find_Associated_Process --
   -----------------------------

   function Find_Associated_Process (Runtime    : Node_Id;
                                     Root_Node  : Node_Id := No_Node)
                                     return Node_Id is
      T : Node_Id;
      S : Node_Id;
      Current_Node : Node_Id;
   begin
      if Root_Node = No_Node then
         Current_Node := Get_Root_Component (Runtime);
      else
         Current_Node := Root_Node;
      end if;

      if Get_Category_Of_Component (Current_Node) = CC_Process and then
         Get_Bound_Processor (Current_Node) = Runtime
      then
         return Current_Node;
      end if;

      if not AAU.Is_Empty (Subcomponents (Current_Node)) then
         S := First_Node (Subcomponents (Current_Node));
         while Present (S) loop
            T := Find_Associated_Process
               (Runtime, Corresponding_Instance (S));

            if T /= No_Node then
               return T;
            end if;

            S := Next_Node (S);
         end loop;
      end if;

      return No_Node;
   end Find_Associated_Process;

   ---------------------------
   -- Get_Partition_Runtime --
   ---------------------------

   function Get_Partition_Runtime (Process    : Node_Id;
                                   Root_Node  : Node_Id := No_Node)
                                     return Node_Id is
      T : Node_Id;
      S : Node_Id;
      Current_Node : Node_Id;
   begin
      if Root_Node = No_Node then
         Current_Node := Get_Root_Component (Process);
      else
         Current_Node := Root_Node;
      end if;

      if Get_Category_Of_Component (Current_Node) = CC_Virtual_Processor
         and then Get_Bound_Processor (Process) = Current_Node
      then
         return Current_Node;
      end if;

      if not AAU.Is_Empty (Subcomponents (Current_Node)) then
         S := First_Node (Subcomponents (Current_Node));
         while Present (S) loop
            T := Get_Partition_Runtime
               (Process, Corresponding_Instance (S));

            if T /= No_Node then
               return T;
            end if;

            S := Next_Node (S);
         end loop;
      end if;

      return No_Node;
   end Get_Partition_Runtime;

end Ocarina.Backends.Utils;
