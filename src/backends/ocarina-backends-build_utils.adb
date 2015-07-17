------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . B U I L D _ U T I L S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ada.Unchecked_Deallocation;

with GNAT.Table;

with GNAT.OS_Lib;
with GNAT.Directory_Operations;

with Ocarina.Namet;
with Ocarina.Output;
with Utils; use Utils;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Options;  use Ocarina.Options;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends; use Ocarina.Backends;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;

package body Ocarina.Backends.Build_Utils is

   use GNAT.OS_Lib;
   use GNAT.Directory_Operations;
   use Ocarina.Namet;
   use Ocarina.Output;

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADU renames Ocarina.Backends.Ada_Tree.Nutils;

   procedure Split_Path
     (Filename  :     Name_Id;
      Directory :     Name_Id;
      Basename  : out Name_Id;
      Dirname   : out Name_Id);
   --  Dirname and Basename corresponds to Dir_Name and Base_Name of
   --  an intermediate filename. If Filename is an absolute path, this
   --  intermediate filename is Filename. If Filename is a relative
   --  path, the intermediate filename is either relative to
   --  Directory, if Directory is non-null, or to the current working
   --  directory if Directory is null. Directory may be relative to
   --  the current directory in which case it is also normalized.

   function Resolve_Language (E : Node_Id) return Supported_Source_Language;
   --  Fetches the Source_Language property of E. If the property is
   --  not set, try to deduce the language from the current generator.

   generic
      --  This generic package is a generic list to store the "build
      --  utils" (makefiles, project files...). It provides accessor
      --  routines to allow a process node to find its corresponding
      --  "build util".

      type Build_Util is private;
      --  The type of "build util"

      Id : String;
      --  The Id of the generic table. It MUST be a unique string

      with procedure Free (T : in out Build_Util);
      --  For deallocation purpose

   package Generic_List is
      --  This package is a generic list to store the "build utils"
      --  (makefiles, project files...). It provides accessor routines
      --  to allow a process node to find its corresponding "build util".

      procedure Set (P : Node_Id; U : Build_Util);
      function Get (P : Node_Id) return Build_Util;

      procedure Free;
      --  Deallocates the table

      procedure Init;
      --  A call to this procedure is NECESSARY after any call to
      --  Free. It is not necessary before the first use of the table.

   end Generic_List;

   ------------
   -- Length --
   ------------

   function Length (T : Name_Tables.Instance) return Int is
   begin
      return Name_Tables.Last (T) - Name_Tables.First + 1;
   end Length;

   ----------------------
   -- Get_Runtime_Path --
   ----------------------

   function Get_Runtime_Path (Runtime_Name : String) return String is
   begin
      Get_Name_String (Installation_Directory);
      Add_Str_To_Name_Buffer ("include" & Directory_Separator);
      Add_Str_To_Name_Buffer ("ocarina" & Directory_Separator);
      Add_Str_To_Name_Buffer ("runtime" & Directory_Separator);
      Add_Str_To_Name_Buffer (Runtime_Name);

      declare
         Path : constant String := Get_Name_String (Name_Find);
      begin
         if not Is_Directory (Path) then
            Display_Error (Path & " is not a valid runtime directory", True);
         end if;
         return Path;
      end;
   end Get_Runtime_Path;

   ----------------
   -- Split_Path --
   ----------------

   procedure Split_Path
     (Filename  :     Name_Id;
      Directory :     Name_Id;
      Basename  : out Name_Id;
      Dirname   : out Name_Id)
   is
   begin
      if Directory = No_Name then
         Set_Str_To_Name_Buffer (".");
      else
         Get_Name_String (Directory);
      end if;
      declare
         Normalized_Dir : constant String :=
           Normalize_Pathname (Name_Buffer (1 .. Name_Len));
         Resolved_Filename : constant String :=
           Normalize_Pathname (Get_Name_String (Filename), Normalized_Dir);
      begin
         Dirname  := Get_String_Name (Dir_Name (Resolved_Filename));
         Basename := Get_String_Name (Base_Name (Resolved_Filename));
      end;
   end Split_Path;

   ------------------
   -- Generic_List --
   ------------------

   package body Generic_List is

      package Internal_Table is new GNAT.Table (Build_Util, Nat, 1, 10, 10);
      --  The internal table

      function Get_Internal_Name (P : Node_Id) return Name_Id;
      --  For code factorization purpose

      -----------------------
      -- Get_Internal_Name --
      -----------------------

      function Get_Internal_Name (P : Node_Id) return Name_Id is
         pragma Assert (AAU.Is_Process (P));
      begin

         Set_Nat_To_Name_Buffer (Nat (P));
         Add_Str_To_Name_Buffer ('%' & Id & '%');
         return Name_Find;
      end Get_Internal_Name;

      ---------
      -- Set --
      ---------

      procedure Set (P : Node_Id; U : Build_Util) is
         I_Name : constant Name_Id := Get_Internal_Name (P);
      begin
         Internal_Table.Append (U);
         Set_Name_Table_Info (I_Name, Internal_Table.Last);
      end Set;

      ---------
      -- Get --
      ---------

      function Get (P : Node_Id) return Build_Util is
         I_Name : constant Name_Id := Get_Internal_Name (P);
         Index  : constant Nat     := Get_Name_Table_Info (I_Name);
      begin
         if Index = 0 then
            raise Program_Error
              with "Try to get a build utils which has" & " not been set";
         end if;

         return Internal_Table.Table (Index);
      end Get;

      ----------
      -- Init --
      ----------

      procedure Init is
      begin
         Internal_Table.Init;
      end Init;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         for J in Internal_Table.First .. Internal_Table.Last loop
            Free (Internal_Table.Table (J));
         end loop;

         Internal_Table.Free;
         Internal_Table.Init;
      end Free;
   end Generic_List;

   ----------------------
   -- Resolve_Language --
   ----------------------

   function Resolve_Language (E : Node_Id) return Supported_Source_Language is
      Language : Supported_Source_Language := Get_Source_Language (E);
   begin
      --  If the user did not specify a language for E, we assume that
      --  the language is the current generator one.

      if Language = Language_None then
         case Get_Current_Backend_Kind is
            when PolyORB_HI_Ada =>
               Language := Language_Ada_95;

            when PolyORB_HI_C =>
               Language := Language_C;

            when PolyORB_HI_RTSJ =>
               Language := Language_RTSJ;

            when others =>
               raise Program_Error;
         end case;
      end if;

      return Language;
   end Resolve_Language;

   ---------------
   -- Makefiles --
   ---------------

   package body Makefiles is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance
        (E            : Node_Id;
         Force_Parent : Node_Id := No_Node);
      procedure Visit_Port_Instance (E : Node_Id);
      procedure Visit_Bus_Instance (E : Node_Id);
      procedure Visit_Virtual_Bus_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Abstract_Instance (E : Node_Id);

      procedure Build_Architecture_Instance (E : Node_Id);
      procedure Build_Component_Instance (E : Node_Id);
      procedure Build_System_Instance (E : Node_Id);
      procedure Build_Process_Instance (E : Node_Id);

      procedure Clean_Architecture_Instance (E : Node_Id);
      procedure Clean_Component_Instance (E : Node_Id);
      procedure Clean_System_Instance (E : Node_Id);

      Current_Process : Node_Id := No_Node;

      type Makefile_Rec is record
         Appli_Name : Name_Id;
         --  The distributed application name

         Node_Name : Name_Id;
         --  The node name (in lower case)

         Execution_Platform : Supported_Execution_Platform := Platform_None;
         Execution_Platform_Name : Name_Id                      := No_Name;
         --  The execution platform of the processor the current node
         --  is bound to.

         Transport_API : Supported_Transport_APIs;
         --  The transport API used by the current node to
         --  communicate with other nodes.

         C_Objs : Name_Tables.Instance;

         Ada_Sources : Name_Tables.Instance;

         Asn_Sources : Name_Tables.Instance;

         C_Sources : Name_Tables.Instance;
         --  The C source files that may implement some subprograms of
         --  the current node (absolute or relative path).

         CPP_Sources : Name_Tables.Instance;
         --  The C source files that may implement some subprograms of
         --  the current node (absolute or relative path).

         C_Libraries : Name_Tables.Instance;
         --  The C libraries that may contain the binary code of some
         --  subprograms of the current node (absolute or relative
         --  path).

         User_Source_Dirs : Name_Tables.Instance;
         --  Directories of the source files provided by the user

         Use_Transport : Boolean;
         --  Use_Transport is used to know if the node has in or out
         --  port If it uses transport, the C Makefiles will contain
         --  something like NEED_TRANSPORT = [yes|no]. It is used to
         --  know if the files that handle transport in PolyORB-HI-C
         --  should be compiled or not.

         Simulink_Directory : Name_Id;
         --  The Simulink_Directory corresponds to the directory
         --  that contains the simulink application code.

         Simulink_Node : Name_Id;
         --  The Simulink_Node is the name of the node we try to
         --  integrate in our AADL model.

         Use_Simulink : Boolean;
         --  Use_Simulink states if we integrate simulink application
         --  code or not.

         Scade_Directory : Name_Id;
         --  The Scade_Directory is the name of the directory that contains
         --  Scade source code.

         Use_Scade : Boolean;
         --  The Use_Scade variable tells the build-system if we try
         --  to integrate SCADE application code in our generated
         --  system.

      end record;
      --  This structure gathers all the information needed to
      --  generate a makefile for a given node of the distributed
      --  application.

      type Makefile_Type is access all Makefile_Rec;

      procedure Free (M : in out Makefile_Type);
      --  Deallocates the internals of T

      procedure Ada_C_Command_Line_Flags
        (Ada_Sources : Name_Tables.Instance;
         C_Sources   : Name_Tables.Instance;
         CPP_Sources : Name_Tables.Instance;
         C_Libraries : Name_Tables.Instance);

      procedure Compile_Ada_Files (Ada_Sources : Name_Tables.Instance);
      procedure Compile_C_Files (C_Sources : Name_Tables.Instance);
      procedure Compile_CPP_Files (CPP_Sources : Name_Tables.Instance);
      --  Generate a makefile target to compile C_Sources C files

      procedure Handle_C_Source
        (E                 : Node_Id;
         Implem_Name       : Name_Id;
         Source_Files      : Name_Array;
         M                 : Makefile_Type;
         Custom_Source_Dir : Name_Id := No_Name);
      --  Update the makefile structure by adding necessary paths to
      --  sources or libraries provided by the 'Source_Files' array. E
      --  is the node for which the source files are given, it is used
      --  to resolve relative paths through its absolute location.

      procedure Handle_CPP_Source
        (E                 : Node_Id;
         Implem_Name       : Name_Id;
         Source_Files      : Name_Array;
         M                 : Makefile_Type;
         Custom_Source_Dir : Name_Id := No_Name);
      --  Update the makefile structure by adding necessary paths to
      --  sources or libraries provided by the 'Source_Files' array. E
      --  is the node for which the source files are given, it is used
      --  to resolve relative paths through its absolute location.

      procedure Handle_Ada_Source
        (E            : Node_Id;
         Implem_Name  : Name_Id;
         Source_Files : Name_Array;
         M            : Makefile_Type);
      --  Update the makefile structure by adding necessary paths to
      --  sources or libraries provided by the 'Source_Files' array. E
      --  is the node for which the source files are given, it is used
      --  to resolve relative paths through its absolute location.

      ----------
      -- Free --
      ----------

      procedure Free (M : in out Makefile_Type) is
         procedure Deallocate is new Ada.Unchecked_Deallocation
           (Makefile_Rec,
            Makefile_Type);
      begin
         Name_Tables.Free (M.all.Ada_Sources);
         Name_Tables.Free (M.all.Asn_Sources);
         Name_Tables.Free (M.all.C_Objs);
         Name_Tables.Free (M.all.C_Sources);
         Name_Tables.Free (M.all.C_Libraries);
         Name_Tables.Free (M.all.User_Source_Dirs);

         Deallocate (M);
      end Free;

      package Makefiles is new Generic_List
        (Makefile_Type,
         "Makefile_List",
         Free);
      --  The list of all the makefile structures

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Makefiles.Free;
      end Reset;

      -----------------------
      -- Handle_Ada_Source --
      -----------------------

      procedure Handle_Ada_Source
        (E            : Node_Id;
         Implem_Name  : Name_Id;
         Source_Files : Name_Array;
         M            : Makefile_Type)
      is
         Source_Basename : Name_Id;
         Source_Dirname  : Name_Id;
         S_Name          : Name_Id;

      begin
         --  Ensure the user gives at most one source file (.adb)

         if Source_Files'Length > 2 then
            Display_Located_Error
              (Loc (E),
               "cannot have more than two source files for an Ada subprogram",
               Fatal => True);

         elsif Source_Files'Length /= 0 and then Implem_Name /= No_Name then
            for J in Source_Files'Range loop
               --  Ensure the source is added only once per node

               Get_Name_String (Source_Files (J));
               Get_Name_String_And_Append (M.Node_Name);
               Add_Str_To_Name_Buffer ("%source_text%");
               S_Name := Name_Find;

               if Get_Name_Table_Info (S_Name) = 0 then
                  Set_Name_Table_Info (S_Name, 1);

                  Get_Name_String (Source_Files (J));

                  Split_Path
                    (Source_Files (J),
                     Loc (E).Dir_Name,
                     Source_Basename,
                     Source_Dirname);

                  Get_Name_String (Source_Basename);

                  if Name_Buffer (Name_Len - 3 .. Name_Len) = ".adb" then
                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (Source_Basename);

                     Name_Tables.Append (M.Ada_Sources, Name_Find);
                  end if;
               end if;
            end loop;
         end if;
      end Handle_Ada_Source;

      ---------------------
      -- Handle_C_Source --
      ---------------------

      procedure Handle_C_Source
        (E                 : Node_Id;
         Implem_Name       : Name_Id;
         Source_Files      : Name_Array;
         M                 : Makefile_Type;
         Custom_Source_Dir : Name_Id := No_Name)
      is
         Source_Basename : Name_Id;
         Source_Dirname  : Name_Id;
         S_Name          : Name_Id;
         Binding_Key     : constant String := "%user_src_dir%";
      begin
         --  Ensure the user gives at most one source file (.c)

         if Source_Files'Length > 1
           and then Get_Current_Backend_Kind = PolyORB_HI_Ada
         then
            Display_Located_Error
              (Loc (E),
               "more than one source files for a C subprogram",
               Fatal => True);
         end if;

         if Source_Files'Length = 0 and then Implem_Name /= No_Name then
            --  This means that the user did not provide source file
            --  names for the C implementation but did provide the
            --  implementation name. Therefore, the corresponding
            --  source files have conventional names and are located
            --  at the same directory as the AADL file.

            Split_Path
              (Implem_Name,
               Loc (E).Dir_Name,
               Source_Basename,
               Source_Dirname);

            if Custom_Source_Dir /= No_Name then
               Source_Dirname := Custom_Source_Dir;
            end if;

            Set_Str_To_Name_Buffer (Binding_Key);
            Get_Name_String_And_Append (Source_Dirname);
            Get_Name_String_And_Append (M.Node_Name);

            if Get_Name_Table_Byte (Name_Find) = 0 then
               Name_Tables.Append (M.User_Source_Dirs, Source_Dirname);
               Set_Name_Table_Byte (Name_Find, 1);
            end if;

         elsif Source_Files'Length /= 0 then
            for J in Source_Files'Range loop
               --  Ensure the source is added only once per node

               Get_Name_String (Source_Files (J));
               Get_Name_String_And_Append (M.Node_Name);
               Add_Str_To_Name_Buffer ("%source_text%");
               S_Name := Name_Find;

               if Get_Name_Table_Info (S_Name) = 0 then
                  Set_Name_Table_Info (S_Name, 1);

                  Get_Name_String (Source_Files (J));

                  Split_Path
                    (Source_Files (J),
                     Loc (E).Dir_Name,
                     Source_Basename,
                     Source_Dirname);

                  if Custom_Source_Dir /= No_Name then
                     Source_Dirname := Custom_Source_Dir;
                  end if;

                  Get_Name_String (Source_Basename);

                  if Name_Buffer (Name_Len - 1 .. Name_Len) = ".o"
                    or else Name_Buffer (Name_Len - 1 .. Name_Len) = ".a"
                  then
                     --  Library names MUST begin with "lib"

                     if Name_Buffer (Name_Len - 1 .. Name_Len) = ".a"
                       and then
                       (Name_Len <= 5 or else Name_Buffer (1 .. 3) /= "lib")
                     then
                        Display_Error
                          ("Invalid library name" &
                           Name_Buffer (1 .. Name_Len),
                           Fatal => True);
                     end if;

                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (Source_Basename);

                     Name_Tables.Append (M.C_Libraries, Name_Find);

                  elsif Name_Buffer (Name_Len - 1 .. Name_Len) = ".c" then
                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (Source_Basename);

                     Name_Tables.Append (M.C_Sources, Name_Find);

                     Set_Str_To_Name_Buffer (Binding_Key);
                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (M.Node_Name);

                     if Get_Name_Table_Byte (Name_Find) = 0 then
                        Name_Tables.Append
                          (M.User_Source_Dirs,
                           Source_Dirname);
                        Set_Name_Table_Byte (Name_Find, 1);
                     end if;

                  else
                     Set_Str_To_Name_Buffer (Binding_Key);
                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (M.Node_Name);

                     if Get_Name_Table_Byte (Name_Find) = 0 then
                        Name_Tables.Append
                          (M.User_Source_Dirs,
                           Source_Dirname);
                        Set_Name_Table_Byte (Name_Find, 1);
                     end if;
                  end if;
               end if;
            end loop;
         end if;
      end Handle_C_Source;

      -----------------------
      -- Handle_CPP_Source --
      -----------------------

      procedure Handle_CPP_Source
        (E                 : Node_Id;
         Implem_Name       : Name_Id;
         Source_Files      : Name_Array;
         M                 : Makefile_Type;
         Custom_Source_Dir : Name_Id := No_Name)
      is
         Source_Basename : Name_Id;
         Source_Dirname  : Name_Id;
         S_Name          : Name_Id;
         Binding_Key     : constant String := "%user_src_dir%";
      begin
         --  Ensure the user gives at most one source file (.cc or .cpp)

         if Source_Files'Length > 1
           and then Get_Current_Backend_Kind = PolyORB_HI_Ada
         then
            Display_Located_Error
              (Loc (E),
               "more than one source files for a C++ subprogram",
               Fatal => True);
         end if;

         if Source_Files'Length = 0 and then Implem_Name /= No_Name then
            --  This means that the user did not provide source file
            --  names for the C implementation but did provide the
            --  implementation name. Therefore, the corresponding
            --  source files have conventional names and are located
            --  at the same directory as the AADL file.

            Split_Path
              (Implem_Name,
               Loc (E).Dir_Name,
               Source_Basename,
               Source_Dirname);

            if Custom_Source_Dir /= No_Name then
               Source_Dirname := Custom_Source_Dir;
            end if;

            Set_Str_To_Name_Buffer (Binding_Key);
            Get_Name_String_And_Append (Source_Dirname);
            Get_Name_String_And_Append (M.Node_Name);

            if Get_Name_Table_Byte (Name_Find) = 0 then
               Name_Tables.Append (M.User_Source_Dirs, Source_Dirname);
               Set_Name_Table_Byte (Name_Find, 1);
            end if;

         elsif Source_Files'Length /= 0 then
            for J in Source_Files'Range loop
               --  Ensure the source is added only once per node

               Get_Name_String (Source_Files (J));
               Get_Name_String_And_Append (M.Node_Name);
               Add_Str_To_Name_Buffer ("%source_text%");
               S_Name := Name_Find;

               if Get_Name_Table_Info (S_Name) = 0 then
                  Set_Name_Table_Info (S_Name, 1);

                  Get_Name_String (Source_Files (J));

                  Split_Path
                    (Source_Files (J),
                     Loc (E).Dir_Name,
                     Source_Basename,
                     Source_Dirname);

                  if Custom_Source_Dir /= No_Name then
                     Source_Dirname := Custom_Source_Dir;
                  end if;

                  Get_Name_String (Source_Basename);

                  if Name_Buffer (Name_Len - 2 .. Name_Len) = ".cc"
                    or else Name_Buffer (Name_Len - 3 .. Name_Len) = ".cpp"
                  then
                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (Source_Basename);

                     Name_Tables.Append (M.CPP_Sources, Name_Find);

                     Set_Str_To_Name_Buffer (Binding_Key);
                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (M.Node_Name);

                     if Get_Name_Table_Byte (Name_Find) = 0 then
                        Name_Tables.Append
                          (M.User_Source_Dirs,
                           Source_Dirname);
                        Set_Name_Table_Byte (Name_Find, 1);
                     end if;

                  else
                     Set_Str_To_Name_Buffer (Binding_Key);
                     Get_Name_String (Source_Dirname);
                     Get_Name_String_And_Append (M.Node_Name);

                     if Get_Name_Table_Byte (Name_Find) = 0 then
                        Name_Tables.Append
                          (M.User_Source_Dirs,
                           Source_Dirname);
                        Set_Name_Table_Byte (Name_Find, 1);
                     end if;
                  end if;
               end if;
            end loop;
         end if;
      end Handle_CPP_Source;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when K_Component_Instance =>
               Visit_Component_Instance (E);

            when K_Port_Spec_Instance =>
               Visit_Port_Instance (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------

      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Bus =>
               Visit_Bus_Instance (E);

            when CC_Virtual_Bus =>
               Visit_Virtual_Bus_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when CC_Abstract =>
               Visit_Abstract_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ------------------------
      -- Visit_Bus_Instance --
      ------------------------

      procedure Visit_Bus_Instance (E : Node_Id) is
         SC : Node_Id;
      begin
         if not AAU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC

               Visit (Corresponding_Instance (SC));

               SC := Next_Node (SC);
            end loop;
         end if;
      end Visit_Bus_Instance;

      -----------------------------
      -- Visit_Abstract_Instance --
      -----------------------------

      procedure Visit_Abstract_Instance (E : Node_Id) is
         SC       : Node_Id;
         Instance : Node_Id;
      begin
         if not AAU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC
               Instance := Corresponding_Instance (SC);
               if (Get_Category_Of_Component (Instance) = CC_Subprogram) then
                  Visit_Subprogram_Instance (Instance, Current_Process);
               else
                  Visit (Instance);
               end if;

               SC := Next_Node (SC);
            end loop;
         end if;
      end Visit_Abstract_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         Source  : Name_Id;
         Sources : constant Name_Array    := Get_Source_Text (E);
         M       : constant Makefile_Type := Makefiles.Get (Current_Process);
      begin
         if Get_Source_Language (E) = Language_ASN1
           and then Sources'Length /= 0
         then
            Source := Sources (1);
            Name_Tables.Append (M.Asn_Sources, Source);
         end if;
      end Visit_Data_Instance;

      --------------------------------
      -- Visit_Virtual_Bus_Instance --
      --------------------------------

      procedure Visit_Virtual_Bus_Instance (E : Node_Id) is
         SC : Node_Id;
      begin
         if Get_Implementation (E) /= No_Node then
            Visit (Get_Implementation (E));
         end if;

         if not AAU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC

               Visit (Corresponding_Instance (SC));

               SC := Next_Node (SC);
            end loop;
         end if;
      end Visit_Virtual_Bus_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         C              : Node_Id;
         S              : constant Node_Id       := Parent_Subcomponent (E);
         A : constant Node_Id := Parent_Component (Parent_Subcomponent (E));
         M              : constant Makefile_Type := new Makefile_Rec;
         SC             : Node_Id;
         Current_Device : Node_Id;
         Feature        : Node_Id;
         Parent         : Node_Id;
         Src            : Node_Id;
         Dst            : Node_Id;
         The_System     : constant Node_Id       :=
           Parent_Component (Parent_Subcomponent (E));
      begin
         --  Associates the Makefile structure to the process
         --  instance. Keep in mind that it is important to use
         --  accesses here because all the visited threads and
         --  subprgrams will fetch this access to update the
         --  corresponding structure.

         Current_Process := E;

         Makefiles.Set (E, M);

         M.Appli_Name    := Normalize_Name (Name (Identifier (A)));
         M.Node_Name     := Normalize_Name (Name (Identifier (S)));
         M.Use_Transport := False;
         M.Use_Simulink  := False;
         M.Use_Scade     := False;

         --  Get the execution platform of the processor this node is
         --  bound to.

         M.Execution_Platform :=
           Get_Execution_Platform (Get_Bound_Processor (E));
         M.Execution_Platform_Name :=
           Get_Execution_Platform (Get_Bound_Processor (E));

         --  Get the transport API used by this node. It is
         --  important to ensure that the Namings package visitors
         --  have already been executed since they perform all
         --  consistency checks and bind a node to its transport
         --  API.

         M.Transport_API := Fetch_Transport_API (E);

         --  Initialize the lists

         Name_Tables.Init (M.Ada_Sources);
         Name_Tables.Init (M.Asn_Sources);
         Name_Tables.Init (M.C_Sources);
         Name_Tables.Init (M.CPP_Sources);
         Name_Tables.Init (M.C_Objs);
         Name_Tables.Init (M.C_Libraries);
         Name_Tables.Init (M.User_Source_Dirs);

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC

               Visit (Corresponding_Instance (SC));

               SC := Next_Node (SC);
            end loop;
         end if;

         if not AAU.Is_Empty (Features (E)) then
            Feature := First_Node (Features (E));

            while Present (Feature) loop
               if not AAU.Is_Empty (Sources (Feature)) then
                  Src := First_Node (Sources (Feature));

                  while Present (Src) loop

                     Parent := Parent_Component (Item (Src));

                     if AAU.Is_Process (Parent) and then Parent /= E then
                        if Get_Provided_Virtual_Bus_Class (Extra_Item (Src)) /=
                          No_Node
                        then
                           Visit
                             (Get_Provided_Virtual_Bus_Class
                                (Extra_Item (Src)));
                        end if;
                     end if;

                     Src := Next_Node (Src);
                  end loop;
               end if;

               --  The destinations of F

               if not AAU.Is_Empty (Destinations (Feature)) then
                  Dst := First_Node (Destinations (Feature));

                  while Present (Dst) loop
                     Parent := Parent_Component (Item (Dst));

                     if AAU.Is_Process (Parent) and then Parent /= E then
                        if Get_Provided_Virtual_Bus_Class (Extra_Item (Dst)) /=
                          No_Node
                        then
                           Visit
                             (Get_Provided_Virtual_Bus_Class
                                (Extra_Item (Dst)));
                        end if;
                     end if;

                     Dst := Next_Node (Dst);
                  end loop;
               end if;

               Feature := Next_Node (Feature);
            end loop;
         end if;

         --  We look for devices bound to the same processor
         --  than the current process to find the file
         --  that contains the configuration of the device.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AAU.Is_Device (Corresponding_Instance (C)) then
                  Current_Device := Corresponding_Instance (C);
                  declare
                     Source_Files : constant Name_Array :=
                       Get_Source_Text (Current_Device);
                  begin
                     Handle_C_Source
                       (Parent_Subcomponent (Current_Device),
                        No_Name,
                        Source_Files,
                        M);
                  end;
               end if;
               C := Next_Node (C);
            end loop;
         end if;
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;

      begin
         --  Visit all the subcomponents of the system

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Parent_Process : constant Node_Id :=
           Corresponding_Instance (Get_Container_Process (E));
         M : constant Makefile_Type := Makefiles.Get (Parent_Process);
         Compute_Entrypoint    : Name_Id;
         Initialize_Entrypoint : constant Name_Id       :=
           Get_Thread_Initialize_Entrypoint (E);
         Language : constant Supported_Source_Language := Resolve_Language (E);
         Source_Files : constant Name_Array := Get_Source_Text (E);
         Call_Seq     : Node_Id;
         Spg_Call     : Node_Id;
         F            : Node_Id;
      begin
         --  If the thread implementation is in C, we need to update
         --  the makefile structure.

         if Language = Language_C then
            Compute_Entrypoint := Get_Thread_Compute_Entrypoint (E);
            Handle_C_Source (E, Compute_Entrypoint, Source_Files, M);
            Handle_C_Source (E, Initialize_Entrypoint, Source_Files, M);
         end if;

         --  Visit the features of the thread for possible source
         --  files.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  M.Use_Transport := True;
                  if Is_In (F) then
                     Visit (F);
                  end if;
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the thread

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance
        (E            : Node_Id;
         Force_Parent : Node_Id := No_Node)
      is
         Parent_Process  : Node_Id;
         M               : Makefile_Type;
         Subprogram_Kind : constant Supported_Subprogram_Kind :=
           Get_Subprogram_Kind (E);
         Source_Name  : constant Name_Id    := Get_Source_Name (E);
         Source_Files : constant Name_Array := Get_Source_Text (E);
         Call_Seq     : Node_Id;
         Spg_Call     : Node_Id;
         Simulink_Dir : Name_Id;
         Scade_Dir    : Name_Id;
      begin
         --  Only C subprogram influence the structure of the
         --  generated makefile.
         if Force_Parent /= No_Node then
            Parent_Process := Force_Parent;
         else
            Parent_Process :=
              Corresponding_Instance (Get_Container_Process (E));
         end if;

         M := Makefiles.Get (Parent_Process);

         case Subprogram_Kind is
            when Subprogram_Opaque_C =>
               --  If the subprogram is implemented by C source files,
               --  add the files to the C_Files list of the makefile
               --  structure. If the subprogram is implemented by a C
               --  library, add the files to the C_Libraries list of
               --  the makefile structure.

               Handle_C_Source (E, Source_Name, Source_Files, M);

            when Subprogram_Opaque_CPP =>
               --  If the subprogram is implemented by CPP source
               --  files, add the files to the CPP_Files list of the
               --  makefile structure. If the subprogram is
               --  implemented by a CPP library, add the files to the
               --  C_Libraries list of the makefile structure.

               Handle_CPP_Source (E, Source_Name, Source_Files, M);

            when Subprogram_Opaque_Ada_95 =>
               --  If the subprogram is implemented by Ada source files,
               --  add the files to the Ada_Files list of the makefile
               --  structure.

               Handle_Ada_Source (E, Source_Name, Source_Files, M);

            when Subprogram_Scade =>
               Scade_Dir         := Source_Files (1);
               M.Use_Scade       := True;
               M.Scade_Directory := Scade_Dir;

            when Subprogram_Simulink =>
               Simulink_Dir := Source_Files (1);

               M.Use_Simulink := True;

               M.Simulink_Directory := Simulink_Dir;
               M.Simulink_Node      := Source_Name;

               Set_Str_To_Name_Buffer ("");
               Get_Name_String (Simulink_Dir);
               Add_Str_To_Name_Buffer ("/");
               Add_Str_To_Name_Buffer ("/*.o");
               Name_Tables.Append (M.C_Objs, Name_Find);

            when others =>
               null;
         end case;

         --  Visit all the call sequences of the subprogram

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      -------------------------
      -- Visit_Port_Instance --
      -------------------------

      procedure Visit_Port_Instance (E : Node_Id) is
         Parent_Process : constant Node_Id :=
           Corresponding_Instance
             (Get_Container_Process (Parent_Component (E)));
         M : constant Makefile_Type := Makefiles.Get (Parent_Process);
         Language : constant Supported_Source_Language := Resolve_Language (E);
         Compute_Entrypoint : constant Name_Id                   :=
           Get_Port_Compute_Entrypoint (E);
         Source_Files : constant Name_Array := Get_Source_Text (E);
         Data         : Node_Id;
      begin
         --  If the port implementation is in C, we need to update
         --  the makefile structure.

         if Language = Language_C then
            Handle_C_Source (E, Compute_Entrypoint, Source_Files, M);
         end if;

         if Is_Data (E) then
            Data := Corresponding_Instance (E);

            if Get_Source_Language (Data) = Language_Simulink then
               declare
                  Source_Text : constant Name_Array := Get_Source_Text (Data);
               begin

                  M.Use_Simulink := True;

                  if Source_Text'Length /= 0 then
                     M.Simulink_Directory := Source_Text (1);
                  end if;

                  if Get_Source_Name (Data) /= No_Name then
                     M.Simulink_Node := Get_Source_Name (Data);
                  end if;
               end;
            elsif Get_Source_Language (Data) = Language_ASN1 then
               declare
                  Source_Text : constant Name_Array := Get_Source_Text (Data);
               begin
                  if Get_Name_Table_Byte (Name_Find) = 0 then
                     Name_Tables.Append (M.Asn_Sources, Source_Text (1));
                     Set_Name_Table_Byte (Source_Text (1), 1);
                  end if;

                  if Get_Source_Name (Data) /= No_Name then
                     M.Simulink_Node := Get_Source_Name (Data);
                  end if;
               end;
            else
               declare
                  Source_Name : constant Name_Id :=
                    Get_Type_Source_Name (Data);
                  Source_Files : constant Name_Array := Get_Source_Text (Data);
               begin
                  Handle_C_Source (E, Source_Name, Source_Files, M);
               end;
            end if;

         end if;
      end Visit_Port_Instance;

      --------------
      -- Generate --
      --------------

      procedure Generate (E : Node_Id) is

         procedure Generate_Architecture_Instance (E : Node_Id);
         procedure Generate_Component_Instance (E : Node_Id);
         procedure Generate_System_Instance (E : Node_Id);
         procedure Generate_Process_Instance (E : Node_Id);
         procedure Generate_Processor_Instance (E : Node_Id);

         ------------------------------------
         -- Generate_Architecture_Instance --
         ------------------------------------

         procedure Generate_Architecture_Instance (E : Node_Id) is
         begin
            Generate (Root_System (E));
         end Generate_Architecture_Instance;

         ---------------------------------
         -- Generate_Component_Instance --
         ---------------------------------

         procedure Generate_Component_Instance (E : Node_Id) is
            Category : constant Component_Category :=
              Get_Category_Of_Component (E);
         begin
            case Category is
               when CC_System =>
                  Generate_System_Instance (E);

               when CC_Processor =>
                  Generate_Processor_Instance (E);

               when CC_Process =>
                  Generate_Process_Instance (E);

               when others =>
                  null;
            end case;
         end Generate_Component_Instance;

         ------------------------------
         -- Generate_System_Instance --
         ------------------------------

         procedure Generate_System_Instance (E : Node_Id) is
            Dir_Name : constant Name_Id :=
              Normalize_Name (Name (Identifier (E)));

            S  : Node_Id;
            Fd : File_Descriptor;

         begin
            if Is_Directory (Get_Name_String (Dir_Name)) then
               --  Create the file
               Enter_Directory (Dir_Name);
               Fd := Create_File ("Makefile", Text);

               Write_Eol;

               if Fd = Invalid_FD then
                  raise Program_Error;
               end if;

               --  Setting the output

               Set_Output (Fd);

               Write_Line
                 ("###################################################");
               Write_Line
                 ("# This Makefile has been generated automatically  #");
               Write_Line
                 ("# by the Ocarina AADL toolsuite.                  #");
               Write_Line
                 ("# Do not edit this file, all your changes will    #");
               Write_Line
                 ("# be overridden at the next code generation.      #");
               Write_Line
                 ("###################################################");
               Write_Eol;

               Write_Str ("SUBDIRS = ");
               if not AAU.Is_Empty (Subcomponents (E)) then
                  S := First_Node (Subcomponents (E));

                  while Present (S) loop
                     if AAU.Is_Process (Corresponding_Instance (S)) then
                        Write_Name (Normalize_Name (Name (Identifier (S))));
                        Write_Str (" ");
                        --  Corresponding_Instance (S)))));

                     end if;
                     S := Next_Node (S);
                  end loop;
               end if;
               Write_Eol;

               Write_Line ("all:");
               Write_Line
                 (ASCII.HT &
                  "set -e; for d in $(SUBDIRS); do $(MAKE) -C $$d ; done");

               Write_Line ("clean:");
               Write_Line
                 (ASCII.HT &
                  " set -e; for d in $(SUBDIRS); do $(MAKE) " &
                  "clean -C $$d ; done");

               --  Close the file

               Close (Fd);
               Set_Standard_Output;
               Leave_Directory;
            end if;

            --  Generate the makefiles of all process subcomponents

            if not AAU.Is_Empty (Subcomponents (E)) then
               S := First_Node (Subcomponents (E));

               while Present (S) loop
                  Generate (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end Generate_System_Instance;

         -------------------------------
         -- Generate_Process_Instance --
         -------------------------------

         procedure Generate_Process_Instance (E : Node_Id) is
            M  : constant Makefile_Type := Makefiles.Get (E);
            Fd : File_Descriptor;
         begin
            --  Enter the directories

            Enter_Directory (M.Appli_Name);
            Enter_Directory (M.Node_Name);

            --  Create the file

            Fd := Create_File ("Makefile", Text);

            if Fd = Invalid_FD then
               raise Program_Error;
            end if;

            --  Setting the output

            Set_Output (Fd);

            Write_Line ("###################################################");
            Write_Line ("# This Makefile has been generated automatically  #");
            Write_Line ("# by the Ocarina AADL toolsuite.                  #");
            Write_Line ("# Do not edit this file, all your changes will    #");
            Write_Line ("# be overridden at the next code generation.      #");
            Write_Line ("###################################################");
            Write_Eol;

            Write_Str ("#  Distributed application name : ");
            Write_Name (M.Appli_Name);
            Write_Eol;

            Write_Str ("#  Node name                    : ");
            Write_Name (M.Node_Name);
            Write_Eol;

            Write_Str ("#  Execution platform           : ");
            Write_Name (M.Execution_Platform_Name);
            if M.Execution_Platform = Platform_None then
               Write_Eol;
               Write_Str ("#   Note: user defined");
            end if;
            Write_Eol;

            Write_Line
              ("#  Transport API                : " & M.Transport_API'Img);

            Write_Eol;

            if Get_Current_Backend_Kind = PolyORB_HI_C then
               Write_Str ("USER_OBJS = ");
               if Length (M.C_Objs) > 0 then
                  for J in Name_Tables.First .. Name_Tables.Last (M.C_Objs)
                  loop
                     Write_Name (M.C_Objs.Table (J));
                     exit when J = Name_Tables.Last (M.C_Objs);

                     Write_Line (" \");
                     Write_Str (ASCII.HT & "   ");
                  end loop;
                  Write_Eol;
               end if;

               Ada_C_Command_Line_Flags
                 (M.Ada_Sources,
                  M.C_Sources,
                  M.CPP_Sources,
                  M.C_Libraries);

               if Length (M.Ada_Sources) > 0 then
                  Write_Line ("USER_LD=gnatlink `cat ali_file`");
               end if;
            else
               Write_Str ("C_OBJECTS=");
               Ada_C_Command_Line_Flags
                 (M.Ada_Sources,
                  M.C_Sources,
                  M.CPP_Sources,
                  M.C_Libraries);
            end if;
            Write_Eol;

            Generate_Runtime_Specific
              (M.Appli_Name,
               M.Node_Name,
               M.Execution_Platform,
               M.Execution_Platform_Name,
               M.Transport_API,
               M.Ada_Sources,
               M.Asn_Sources,
               M.C_Sources,
               M.C_Libraries,
               M.User_Source_Dirs,
               M.Use_Transport,
               M.Use_Simulink,
               M.Simulink_Directory,
               M.Simulink_Node,
               M.Use_Scade,
               M.Scade_Directory);

            --  Add rule to compile the C files, if any

            Write_Eol;
            Compile_C_Files (M.C_Sources);
            Write_Eol;

            Compile_CPP_Files (M.CPP_Sources);
            Write_Eol;

            Compile_Ada_Files (M.Ada_Sources);
            Write_Eol;

            if Get_Current_Backend_Kind = PolyORB_HI_Ada then
               Write_Line ("prove:");
               Write_Line
                 (ASCII.HT &
                    "gnatprove -P$(PROJECT_FILE) --warnings=continue " &
                    "--report=fail");
            end if;

            --  Close the file

            Close (Fd);
            Set_Standard_Output;

            --  Leave the directories

            Leave_Directory;
            Leave_Directory;
         end Generate_Process_Instance;

         ---------------------------------
         -- Generate_Processor_Instance --
         ---------------------------------

         procedure Generate_Processor_Instance (E : Node_Id) is
            Fd         : File_Descriptor;
            S          : Node_Id;
            The_System : Node_Id;
            PID        : Unsigned_Long_Long := 0;
         begin

         --  The following part is very specific to PolyORB-HI-C and especially
         --  to the code generator for Xtratum. It creates a Makefile to make
         --  the final Makefile that integrates all partitions together.

            if Get_Current_Backend_Kind /= PolyORB_HI_C then
               return;
            end if;

            if Get_Execution_Platform (E) /= Platform_LEON3_XTRATUM then
               return;
            end if;

            The_System := Parent_Component (Parent_Subcomponent (E));

            --  Enter the directories

            Enter_Directory
              (To_Lower
                 (Normalize_Name (Display_Name (Identifier (The_System)))));

            --  Create the file

            Fd :=
              Create_File
                ("Makefile." &
                 Get_Name_String
                   (To_Lower
                      (Normalize_Name
                         (Display_Name (Identifier (The_System))))),
                 Text);

            if Fd = Invalid_FD then
               raise Program_Error;
            end if;

            --  Setting the output

            Set_Output (Fd);

            Write_Line ("###################################################");
            Write_Line ("# This Makefile has been generated automatically  #");
            Write_Line ("# by the Ocarina AADL toolsuite.                  #");
            Write_Line ("# Do not edit this file, all your changes will    #");
            Write_Line ("# be overridden at the next code generation.      #");
            Write_Line ("###################################################");
            Write_Eol;

            Write_Line ("RUNTIME_PATH=" & Get_Runtime_Path ("polyorb-hi-c"));

            Write_Eol;

            Write_Str ("all: build-partitions resident_sw");

            Write_Eol;
            Write_Eol;

            Write_Str ("MAINAPP=");
            Write_Name
              (To_Lower
                 (Normalize_Name (Display_Name (Identifier (The_System)))));
            Write_Eol;

            Write_Str ("PARTITIONS_NAME=");
            --  Generate the makefiles of all process subcomponents

            if not AAU.Is_Empty (Subcomponents (The_System)) then
               S := First_Node (Subcomponents (The_System));

               while Present (S) loop
                  if AAU.Is_Process (Corresponding_Instance (S)) then
                     Write_Name
                       (To_Lower
                          (Normalize_Name (Display_Name (Identifier (S)))));
                     Write_Space;
                  end if;
                  S := Next_Node (S);
               end loop;
            end if;
            Write_Eol;

            Write_Str ("PARTITIONS=");

            --  Generate the makefiles of all process subcomponents

            if not AAU.Is_Empty (Subcomponents (The_System)) then
               S := First_Node (Subcomponents (The_System));

               while Present (S) loop
                  if AAU.Is_Process (Corresponding_Instance (S)) then
                     Write_Name
                       (To_Lower
                          (Normalize_Name (Display_Name (Identifier (S)))));
                     Write_Str ("/");
                     Write_Name
                       (To_Lower
                          (Normalize_Name (Display_Name (Identifier (S)))));
                     Write_Str (".xef");
                     Write_Space;
                  end if;
                  S := Next_Node (S);
               end loop;
            end if;
            Write_Eol;
            Write_Eol;

            Write_Str ("GENERATED_PACK_ARGS=");

            --  Generate the makefiles of all process subcomponents

            if not AAU.Is_Empty (Subcomponents (The_System)) then
               S := First_Node (Subcomponents (The_System));

               while Present (S) loop
                  if AAU.Is_Process (Corresponding_Instance (S)) then
                     Write_Str ("-p ");

                     Write_Str (Unsigned_Long_Long'Image (PID));
                     Write_Str (":");
                     Write_Name
                       (To_Lower
                          (Normalize_Name (Display_Name (Identifier (S)))));
                     Write_Str ("/");
                     Write_Name
                       (To_Lower
                          (Normalize_Name (Display_Name (Identifier (S)))));
                     Write_Str (".xef");
                     Write_Space;
                     PID := PID + 1;
                  end if;
                  S := Next_Node (S);
               end loop;
            end if;
            Write_Eol;
            Write_Eol;

            Write_Str ("include $(RUNTIME_PATH)/make/Makefile.leon3-xtratum");

            Write_Eol;
            Write_Eol;

            Close (Fd);

            Set_Standard_Output;

            --  Leave the directories

            Leave_Directory;
         end Generate_Processor_Instance;

      --  Main part of Generate begins here

      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Generate_Architecture_Instance (E);

            when K_Component_Instance =>
               Generate_Component_Instance (E);

            when others =>
               null;
         end case;
      end Generate;

      ------------------------------
      -- Ada_C_Command_Line_Flags --
      ------------------------------

      procedure Ada_C_Command_Line_Flags
        (Ada_Sources : Name_Tables.Instance;
         C_Sources   : Name_Tables.Instance;
         CPP_Sources : Name_Tables.Instance;
         C_Libraries : Name_Tables.Instance)
      is
      begin
         if Length (Ada_Sources) > 0
           or else Length (C_Sources) > 0
           or else Length (C_Libraries) > 0
         then
            Write_Str (" ");
         end if;

         --  In case of Ada source files, link has to be performed by
         --  gnatlink (as of August 2011 and decision made to get rid
         --  of C binder file). The actual list of Ada object file is
         --  retrived form the binder generated file, hence there is
         --  no need to add them.

         --  In case of C source files, we add the corresponding .o
         --  files.

         if Length (C_Sources) > 0 then
            if Get_Current_Backend_Kind = PolyORB_HI_C
              and then Length (Ada_Sources) > 0
            then
               Write_Line (" \");
               Write_Str (ASCII.HT & "   ");
            end if;

            for J in Name_Tables.First .. Name_Tables.Last (C_Sources) loop
               Get_Name_String (C_Sources.Table (J));
               Set_Str_To_Name_Buffer
                 (Base_Name (Name_Buffer (1 .. Name_Len)));

               Name_Buffer (Name_Len) := 'o';
               Write_Name (Name_Find);

               exit when J = Name_Tables.Last (C_Sources);

               Write_Line (" \");
               Write_Str (ASCII.HT & "   ");
            end loop;
         end if;

         if Length (CPP_Sources) > 0 then
            Write_Line (" \");
            Write_Str (ASCII.HT & "   ");
            for J in Name_Tables.First .. Name_Tables.Last (CPP_Sources) loop
               Get_Name_String (CPP_Sources.Table (J));
               Set_Str_To_Name_Buffer
                 (Base_Name (Name_Buffer (1 .. Name_Len)));

               Name_Buffer (Name_Len - 1 .. Name_Len) := "o ";
               Write_Name (Name_Find);

               exit when J = Name_Tables.Last (CPP_Sources);

               Write_Line (" \");
               Write_Str (ASCII.HT & "   ");
            end loop;
         end if;

         --  In case of C libraries or objects, we add the
         --  corresponding option.

         if Length (C_Libraries) > 0 then
            Write_Line (" \");
            Write_Str (ASCII.HT & "   ");

            for J in Name_Tables.First .. Name_Tables.Last (C_Libraries) loop
               Get_Name_String (C_Libraries.Table (J));

               --  Some tests

               declare
                  Is_Object : constant Boolean := Name_Buffer (Name_Len) = 'o';
                  Dirname   : constant String  :=
                    Dir_Name (Name_Buffer (1 .. Name_Len));
                  Basename : constant String :=
                    Base_Name (Name_Buffer (1 .. Name_Len));
               begin
                  if Is_Object then
                     Write_Name (C_Libraries.Table (J));
                  else
                     Write_Str ("-L" & Dirname & ' ');
                     Write_Str ("-l");
                     Write_Str
                       (Basename (Basename'First + 3 .. Basename'Last));
                  end if;
               end;

               exit when J = Name_Tables.Last (C_Libraries);

               Write_Line (" \");
               Write_Str (ASCII.HT & "   ");
            end loop;
         end if;
         Write_Eol;

         if Length (CPP_Sources) > 0 then
            Write_Line ("USE_CPP_LINKER = true");
         end if;
      end Ada_C_Command_Line_Flags;

      ---------------------
      -- Compile_C_Files --
      ---------------------

      procedure Compile_C_Files (C_Sources : Name_Tables.Instance) is
      begin
         Write_Line ("compile-c-files:");
         if Length (C_Sources) > 0 then

            for J in Name_Tables.First .. Name_Tables.Last (C_Sources) loop
               declare
                  O_File      : Name_Id;
                  Include_Dir : Name_Id;
               begin
                  Get_Name_String (C_Sources.Table (J));
                  Name_Buffer (Name_Len) := 'o';
                  Set_Str_To_Name_Buffer
                    (Base_Name (Name_Buffer (1 .. Name_Len)));
                  O_File := Name_Find;

                  Get_Name_String (C_Sources.Table (J));
                  while (Name_Buffer (Name_Len) /= Directory_Separator)
                    and then Name_Len > 0
                  loop
                     Name_Len := Name_Len - 1;
                  end loop;

                  if Name_Len > 0 then
                     Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len));
                     Include_Dir := Name_Find;
                  else
                     Include_Dir := No_Name;
                  end if;

                  Write_Char (ASCII.HT);
                  Write_Str ("$(CC) -c $(INCLUDE) $(CFLAGS) ");

                  if Include_Dir /= No_Name then
                     Write_Str ("-I");
                     Write_Str ("'");
                     Write_Name (Include_Dir);
                     Write_Str ("'");
                  end if;

                  Write_Str (" '");
                  Write_Name (C_Sources.Table (J));
                  Write_Str ("' -o ");
                  Write_Name (O_File);
                  Write_Eol;
               end;
            end loop;
         end if;
      end Compile_C_Files;

      -----------------------
      -- Compile_CPP_Files --
      -----------------------

      procedure Compile_CPP_Files (CPP_Sources : Name_Tables.Instance) is
      begin
         Write_Line ("compile-cpp-files:");
         if Length (CPP_Sources) > 0 then

            for J in Name_Tables.First .. Name_Tables.Last (CPP_Sources) loop
               declare
                  O_File      : Name_Id;
                  Include_Dir : Name_Id;
               begin
                  Get_Name_String (CPP_Sources.Table (J));
                  Name_Buffer (Name_Len - 1 .. Name_Len) := "o ";
                  Set_Str_To_Name_Buffer
                    (Base_Name (Name_Buffer (1 .. Name_Len)));
                  O_File := Name_Find;

                  Get_Name_String (CPP_Sources.Table (J));
                  while (Name_Buffer (Name_Len) /= Directory_Separator)
                    and then Name_Len > 0
                  loop
                     Name_Len := Name_Len - 1;
                  end loop;

                  if Name_Len > 0 then
                     Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len));
                     Include_Dir := Name_Find;
                  else
                     Include_Dir := No_Name;
                  end if;

                  Write_Char (ASCII.HT);
                  Write_Str ("$(CC) -c $(INCLUDE) $(CFLAGS) ");

                  if Include_Dir /= No_Name then
                     Write_Str ("-I");
                     Write_Str ("'");
                     Write_Name (Include_Dir);
                     Write_Str ("'");
                  end if;

                  Write_Str (" '");
                  Write_Name (CPP_Sources.Table (J));
                  Write_Str ("' -o ");
                  Write_Name (O_File);
                  Write_Eol;
               end;
            end loop;
         end if;
      end Compile_CPP_Files;

      -----------------------
      -- Compile_Ada_Files --
      -----------------------

      procedure Compile_Ada_Files (Ada_Sources : Name_Tables.Instance) is
      begin
         Write_Line ("compile-ada-files:");
         if Length (Ada_Sources) > 0 then
            for J in Name_Tables.First .. Name_Tables.Last (Ada_Sources) loop
               declare
                  O_File   : Name_Id;
                  Ali_File : Name_Id;
               begin
                  Get_Name_String (Ada_Sources.Table (J));
                  Name_Buffer (Name_Len - 2 .. Name_Len) := "o  ";
                  Set_Str_To_Name_Buffer
                    (Base_Name (Name_Buffer (1 .. Name_Len)));
                  O_File := Name_Find;

                  Name_Buffer (Name_Len - 2 .. Name_Len) := "ali";
                  Set_Str_To_Name_Buffer
                    (Base_Name (Name_Buffer (1 .. Name_Len)));
                  Ali_File := Name_Find;

                  Write_Char (ASCII.HT);
                  Write_Str ("$(CC) -c $(INCLUDE) $(CFLAGS) '");
                  Write_Name (Ada_Sources.Table (J));
                  Write_Str ("' -o ");
                  Write_Name (O_File);
                  Write_Eol;
                  Write_Char (ASCII.HT);
                  Write_Str ("echo ");
                  Write_Name (Ali_File);
                  Write_Str (" > ali_file");
                  Write_Eol;

               end;
            end loop;

            Write_Char (ASCII.HT);
            Write_Line ("gnatbind -n *.ali");
         end if;
      end Compile_Ada_Files;

      -----------
      -- Build --
      -----------

      procedure Build (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Build_Architecture_Instance (E);

            when K_Component_Instance =>
               Build_Component_Instance (E);

            when others =>
               null;
         end case;
      end Build;

      ---------------------------------
      -- Build_Architecture_Instance --
      ---------------------------------

      procedure Build_Architecture_Instance (E : Node_Id) is
      begin
         Build (Root_System (E));
      end Build_Architecture_Instance;

      ------------------------------
      -- Build_Component_Instance --
      ------------------------------

      procedure Build_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Build_System_Instance (E);

            when CC_Process =>
               Build_Process_Instance (E);

            when others =>
               null;
         end case;
      end Build_Component_Instance;

      ---------------------------
      -- Build_System_Instance --
      ---------------------------

      procedure Build_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         --  Build all process subcomponents

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               Build (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;
      end Build_System_Instance;

      ----------------------------
      -- Build_Process_Instance --
      ----------------------------

      procedure Build_Process_Instance (E : Node_Id) is
         M       : constant Makefile_Type := Makefiles.Get (E);
         Pid     : Process_Id;
         Out_Pid : Process_Id             := Invalid_Pid;
         Success : Boolean;
         Args    : Argument_List (1 .. 1);
      begin
         --  Enter the directories

         Enter_Directory (M.Appli_Name);
         Enter_Directory (M.Node_Name);

         --  If the user set the BUILD environment variable to some
         --  value, we pass it the GNU make command.

         declare
            Build_Kind    : String_Access := Getenv ("BUILD");
            GNU_Make_Path : String_Access :=
              Locate_Exec_On_Path (GNU_Make_Cmd);
         begin
            Change_If_Empty (String_Ptr (Build_Kind), "Debug");
            Args (1) := new String'("BUILD=" & Build_Kind.all);

            --  Invoke the 'make' command

            Pid :=
              Non_Blocking_Spawn
                (Program_Name => GNU_Make_Path.all,
                 Args         => Args);

            --  Wait until the command achieves its execution

            while Out_Pid /= Pid loop
               Wait_Process (Out_Pid, Success);
               exit when Out_Pid = Pid or else Out_Pid = Invalid_Pid;
            end loop;

            if Out_Pid = Pid then
               if not Success then
                  Display_Error
                    (GNU_Make_Path.all & " died unexpectedly",
                     Fatal => True);
               else
                  pragma Debug
                    (Display_Debug_Message
                       (GNU_Make_Cmd & " terminated normally",
                        Force => True));
                  null;
               end if;
            end if;

            Free (Build_Kind);
            Free (GNU_Make_Path);

            for J in Args'Range loop
               Free (Args (J));
            end loop;
         end;

         --  Leave the directories

         Leave_Directory;
         Leave_Directory;
      end Build_Process_Instance;

      -----------
      -- Clean --
      -----------

      procedure Clean (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Clean_Architecture_Instance (E);

            when K_Component_Instance =>
               Clean_Component_Instance (E);

            when others =>
               null;
         end case;
      end Clean;

      ---------------------------------
      -- Clean_Architecture_Instance --
      ---------------------------------

      procedure Clean_Architecture_Instance (E : Node_Id) is
      begin
         Clean (Root_System (E));
      end Clean_Architecture_Instance;

      ------------------------------
      -- Clean_Component_Instance --
      ------------------------------

      procedure Clean_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Clean_System_Instance (E);

            when others =>
               null;
         end case;
      end Clean_Component_Instance;

      ---------------------------
      -- Clean_System_Instance --
      ---------------------------

      procedure Clean_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         --  Clean all process subcomponents

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               if AAU.Is_Process (Corresponding_Instance (S)) then
                  --  We fetch the application directory name from the
                  --  Makefile structure of one of the application
                  --  nodes.

                  declare
                     M : constant Makefile_Type :=
                       Makefiles.Get (Corresponding_Instance (S));
                  begin
                     GNAT.Directory_Operations.Remove_Dir
                       (Get_Name_String (M.Appli_Name),
                        True);
                  exception
                     when GNAT.Directory_Operations.Directory_Error =>
                        pragma Debug
                          (Display_Debug_Message
                             (Get_Name_String (M.Appli_Name) &
                              " already clean",
                              Force => True));
                        null;
                  end;

                  exit;
               end if;

               S := Next_Node (S);
            end loop;
         end if;
      end Clean_System_Instance;

   end Makefiles;

   -----------------------
   -- Ada_Project_Files --
   -----------------------

   package body Ada_Project_Files is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance
        (E            : Node_Id;
         Force_Parent : Node_Id := No_Node);
      procedure Visit_Port_Instance (E : Node_Id);

      type Ada_Project_File_Rec is record
         Appli_Name : Name_Id;
         --  The distributed application name

         Node_Name : Name_Id;
         --  The node name (in lower case)

         Is_Server : Boolean;
         --  True of the process has IN ports

         Execution_Platform : Supported_Execution_Platform;
         --  The execution platform of the processor the current node
         --  is bound to.

         Transport_API : Supported_Transport_APIs;
         --  The transport API used by the current node to
         --  communicate with other nodes.

         Spec_Names        : Name_Tables.Instance;
         Custom_Spec_Names : Name_Tables.Instance;
         --  USER Ada specs with custom names. For each index J,
         --  Spec_Names (J) is the Ada spec name and Custom_Spec_Names
         --  (J) is the file name containing the spec.

         Body_Names        : Name_Tables.Instance;
         Custom_Body_Names : Name_Tables.Instance;
         --  USER Ada bodies with custom names. For each index J,
         --  Body_Names (J) is the Ada body name and Custom_Body_Names
         --  (J) is the file name containing the body.

         User_Source_Dirs : Name_Tables.Instance;
         --  Directories of the source files provided by the user
      end record;
      --  This structure gathers all the information needed to
      --  generate an Ada project file for a given node of the
      --  distributed application.

      type Ada_Project_File_Type is access all Ada_Project_File_Rec;

      procedure Free (P : in out Ada_Project_File_Type);
      --  Deallocates the internals of T

      procedure Handle_Ada_Source
        (E            : Node_Id;
         Implem_Name  : Name_Id;
         Source_Files : Name_Array;
         P            : Ada_Project_File_Type);
      --  Update the project file structure by adding necessary paths
      --  to sources provided by the 'Source_Files' array. If no
      --  source text is given by an implementation name, we deduce
      --  file names from implementation name. E is the node for which
      --  the source files are given, it is used to resolve relative
      --  paths through its absolute location.

      ----------
      -- Free --
      ----------

      procedure Free (P : in out Ada_Project_File_Type) is
         procedure Deallocate is new Ada.Unchecked_Deallocation
           (Ada_Project_File_Rec,
            Ada_Project_File_Type);
      begin
         --  Deallocate internal tables

         Name_Tables.Free (P.all.Spec_Names);
         Name_Tables.Free (P.all.Custom_Spec_Names);
         Name_Tables.Free (P.all.Body_Names);
         Name_Tables.Free (P.all.Custom_Body_Names);
         Name_Tables.Free (P.all.User_Source_Dirs);

         Deallocate (P);
      end Free;

      package Ada_Project_Files is new Generic_List
        (Ada_Project_File_Type,
         "Ada_Project_File_List",
         Free);
      --  The list of all the makefile structures

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Ada_Project_Files.Free;
      end Reset;

      -----------------------
      -- Handle_Ada_Source --
      -----------------------

      procedure Handle_Ada_Source
        (E            : Node_Id;
         Implem_Name  : Name_Id;
         Source_Files : Name_Array;
         P            : Ada_Project_File_Type)
      is
         Conv_Base_Name  : Name_Id;
         Custom_Name     : Name_Id;
         Suffix          : String (1 .. 4);
         Source_Dirname  : Name_Id;
         Source_Basename : Name_Id;
         Binding_Key     : constant String := "%user_src_dir%";
      begin
         if Implem_Name /= No_Name then
            Conv_Base_Name :=
              ADU.Conventional_Base_Name (ADU.Unit_Name (Implem_Name));
         end if;

         --  Ensure the user gives at most 2 sources files (a spec and
         --  a body).

         if Source_Files'Length > 2 then
            Display_Located_Error
              (Loc (E),
               "More than 2 source files for an Ada subprogram",
               Fatal => True);
         end if;

         if Source_Files'Length = 0 and then Implem_Name /= No_Name then
            --  This means that the user did not provide source file
            --  names for the Ada implementation but provided the
            --  implementation name. Therefore, the corresponding
            --  source files have conventional names and are located
            --  at the same directory as the AADL file.

            Split_Path
              (Conv_Base_Name,
               Loc (E).Dir_Name,
               Source_Basename,
               Source_Dirname);

            Set_Str_To_Name_Buffer (Binding_Key);
            Get_Name_String_And_Append (Source_Dirname);
            Get_Name_String_And_Append (P.Node_Name);

            if Get_Name_Table_Byte (Name_Find) = 0 then
               Name_Tables.Append (P.User_Source_Dirs, Source_Dirname);
               Set_Name_Table_Byte (Name_Find, 1);
            end if;

         elsif Source_Files'Length /= 0 and then Implem_Name /= No_Name then
            for J in Source_Files'Range loop
               Split_Path
                 (Source_Files (J),
                  Loc (E).Dir_Name,
                  Source_Basename,
                  Source_Dirname);

               --  Add the directory to the user directory list
               --  (if it has not been added yet).

               Set_Str_To_Name_Buffer (Binding_Key);
               Get_Name_String_And_Append (Source_Dirname);
               Get_Name_String_And_Append (P.Node_Name);

               if Get_Name_Table_Byte (Name_Find) = 0 then
                  Name_Tables.Append (P.User_Source_Dirs, Source_Dirname);
                  Set_Name_Table_Byte (Name_Find, 1);
               end if;

               Get_Name_String (Source_Basename);

               --  The .ad[bs] consumes 4 characters from to
               --  total file name. The user must give at least
               --  one character base name.

               if Name_Len < 5 then
                  Display_Located_Error
                    (Loc (E),
                     "Incorrect text file name",
                     Fatal => True);
               end if;

               Suffix := Name_Buffer (Name_Len - 3 .. Name_Len);

               Custom_Name := Name_Find;

               if Suffix = ".ads" then
                  if Custom_Name /= Conv_Base_Name then
                     --  Add a custom Spec clause

                     Name_Tables.Append
                       (P.Spec_Names,
                        ADU.Unit_Name (Implem_Name));
                     Name_Tables.Append (P.Custom_Spec_Names, Custom_Name);
                  end if;
               elsif Suffix = ".adb" then
                  if Custom_Name /= Conv_Base_Name then
                     --  Add a custom Body clause

                     Name_Tables.Append
                       (P.Body_Names,
                        ADU.Unit_Name (Implem_Name));
                     Name_Tables.Append (P.Custom_Body_Names, Custom_Name);
                  end if;
               else
                  Display_Located_Error
                    (Loc (E),
                     "Unknown suffix for Ada file name: """ & Suffix & """",
                     Fatal => True);
               end if;
            end loop;
         end if;
      end Handle_Ada_Source;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when K_Component_Instance =>
               Visit_Component_Instance (E);

            when K_Port_Spec_Instance =>
               Visit_Port_Instance (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------

      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         S  : constant Node_Id               := Parent_Subcomponent (E);
         A  : constant Node_Id := Parent_Component (Parent_Subcomponent (E));
         P  : constant Ada_Project_File_Type := new Ada_Project_File_Rec;
         SC : Node_Id;
      begin
         --  Associates the Ada project file structure to the process
         --  instance. Keep in mind that it is important to use
         --  accesses here because all the visited threads and
         --  subprograms will fetch this access to update the
         --  corresponding structure.

         Ada_Project_Files.Set (E, P);

         P.Appli_Name := Normalize_Name (Name (Identifier (A)));
         P.Node_Name  := Normalize_Name (Name (Identifier (S)));

         P.Is_Server := Has_In_Ports (E);

         --  Get the execution platform of the processor this node is
         --  bound to.

         P.Execution_Platform :=
           Get_Execution_Platform (Get_Bound_Processor (E));

         --  Get the transport API used by this node. It is
         --  important to ensure that the Namings package visitors
         --  have already been executed since they perform all
         --  consistency checks and bind a node to its transport
         --  API.

         P.Transport_API := Fetch_Transport_API (E);

         --  Initialize the lists

         Name_Tables.Init (P.Spec_Names);
         Name_Tables.Init (P.Custom_Spec_Names);

         Name_Tables.Init (P.Body_Names);
         Name_Tables.Init (P.Custom_Body_Names);

         Name_Tables.Init (P.User_Source_Dirs);

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            SC := First_Node (Subcomponents (E));

            while Present (SC) loop
               --  Visit the corresponding instance of SC

               Visit (Corresponding_Instance (SC));

               SC := Next_Node (SC);
            end loop;
         end if;
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         --  Visit all the subcomponents of the system

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Parent_Process : constant Node_Id :=
           Corresponding_Instance (Get_Container_Process (E));
         P : constant Ada_Project_File_Type :=
           Ada_Project_Files.Get (Parent_Process);
         Language : constant Supported_Source_Language := Resolve_Language (E);
         Compute_Entrypoint : constant Name_Id                   :=
           Get_Thread_Compute_Entrypoint (E);
         Source_Files : constant Name_Array := Get_Source_Text (E);
         Call_Seq     : Node_Id;
         Spg_Call     : Node_Id;
         F            : Node_Id;
      begin
         --  Only Ada files affect the structure of Ada project files

         if Language = Language_Ada_95 then
            Handle_Ada_Source (E, Compute_Entrypoint, Source_Files, P);
         end if;

         --  Visit the features of the thread for possible source
         --  files.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
                  Visit (F);
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Visit all the call sequences of the thread

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance
        (E            : Node_Id;
         Force_Parent : Node_Id := No_Node)
      is
         Parent_Process  : Node_Id;
         P               : Ada_Project_File_Type;
         Subprogram_Kind : constant Supported_Subprogram_Kind :=
           Get_Subprogram_Kind (E);
         Source_Name  : constant Name_Id    := Get_Source_Name (E);
         Source_Files : constant Name_Array := Get_Source_Text (E);
         Call_Seq     : Node_Id;
         Spg_Call     : Node_Id;
      begin
         if Force_Parent = No_Node then
            Parent_Process :=
              Corresponding_Instance (Get_Container_Process (E));
         else
            Parent_Process := Force_Parent;
         end if;

         P := Ada_Project_Files.Get (Parent_Process);
         --  Only Ada subprograms may influence the structure of the
         --  generated project files.

         case Subprogram_Kind is
            when Subprogram_Opaque_Ada_95 | Subprogram_Hybrid_Ada_95 =>
               Handle_Ada_Source (E, Source_Name, Source_Files, P);

            when others =>
               null;
         end case;

         --  Visit all the call sequences of the subprogram

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      -------------------------
      -- Visit_Port_Instance --
      -------------------------

      procedure Visit_Port_Instance (E : Node_Id) is
         Parent_Process : constant Node_Id :=
           Corresponding_Instance
             (Get_Container_Process (Parent_Component (E)));
         P : constant Ada_Project_File_Type :=
           Ada_Project_Files.Get (Parent_Process);
         Language : constant Supported_Source_Language := Resolve_Language (E);
         Compute_Entrypoint : constant Name_Id                   :=
           Get_Port_Compute_Entrypoint (E);
         Source_Files : constant Name_Array := Get_Source_Text (E);
      begin
         --  Only Ada files affect the structure of Ada project files

         if Language = Language_Ada_95 then
            Handle_Ada_Source (E, Compute_Entrypoint, Source_Files, P);
         end if;
      end Visit_Port_Instance;

      --------------
      -- Generate --
      --------------

      procedure Generate (E : Node_Id) is

         procedure Generate_Architecture_Instance (E : Node_Id);
         procedure Generate_Component_Instance (E : Node_Id);
         procedure Generate_System_Instance (E : Node_Id);
         procedure Generate_Process_Instance (E : Node_Id);

         ------------------------------------
         -- Generate_Architecture_Instance --
         ------------------------------------

         procedure Generate_Architecture_Instance (E : Node_Id) is
         begin
            Generate (Root_System (E));
         end Generate_Architecture_Instance;

         ---------------------------------
         -- Generate_Component_Instance --
         ---------------------------------

         procedure Generate_Component_Instance (E : Node_Id) is
            Category : constant Component_Category :=
              Get_Category_Of_Component (E);
         begin
            case Category is
               when CC_System =>
                  Generate_System_Instance (E);

               when CC_Process =>
                  Generate_Process_Instance (E);

               when others =>
                  null;
            end case;
         end Generate_Component_Instance;

         ------------------------------
         -- Generate_System_Instance --
         ------------------------------

         procedure Generate_System_Instance (E : Node_Id) is
            S : Node_Id;
         begin
            --  Generate the project files of all process subcomponents

            if not AAU.Is_Empty (Subcomponents (E)) then
               S := First_Node (Subcomponents (E));

               while Present (S) loop
                  Generate (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end Generate_System_Instance;

         -------------------------------
         -- Generate_Process_Instance --
         -------------------------------

         procedure Generate_Process_Instance (E : Node_Id) is
            P  : constant Ada_Project_File_Type := Ada_Project_Files.Get (E);
            Fd : File_Descriptor;
         begin
            --  Enter the directories

            Enter_Directory (P.Appli_Name);
            Enter_Directory (P.Node_Name);

            --  Create the file

            Get_Name_String (P.Node_Name);
            Fd := Create_File (Name_Buffer (1 .. Name_Len) & ".gpr", Text);

            if Fd = Invalid_FD then
               raise Program_Error;
            end if;

            --  Setting the output

            Set_Output (Fd);

            Write_Line
              ("--------------------------------------------------------");
            Write_Line
              ("-- This project file has been generated automatically --");
            Write_Line
              ("-- by the Ocarina AADL toolsuite.                     --");
            Write_Line
              ("-- Do not edit this file since all your changes will  --");
            Write_Line
              ("-- be overridden at the next code generation.         --");
            Write_Line
              ("--------------------------------------------------------");
            Write_Eol;

            Write_Str ("--  Application name   : ");
            Write_Name (P.Appli_Name);
            Write_Eol;

            Write_Str ("--  Node name          : ");
            Write_Name (P.Node_Name);
            Write_Eol;

            Write_Line
              ("--  Execution platform : " & P.Execution_Platform'Img);
            Write_Line ("--  Transport API      : " & P.Transport_API'Img);

            Write_Eol;

            Generate_Runtime_Specific
              (P.Appli_Name,
               P.Node_Name,
               P.Is_Server,
               P.Execution_Platform,
               P.Transport_API,
               P.Spec_Names,
               P.Custom_Spec_Names,
               P.Body_Names,
               P.Custom_Body_Names,
               P.User_Source_Dirs);

            --  Close the file

            Close (Fd);
            Set_Standard_Output;

            --  Leave the directories

            Leave_Directory;
            Leave_Directory;
         end Generate_Process_Instance;

      --  Main processing part of Generate begins here

      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Generate_Architecture_Instance (E);

            when K_Component_Instance =>
               Generate_Component_Instance (E);

            when others =>
               null;
         end case;
      end Generate;

   end Ada_Project_Files;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Makefiles.Reset;
      Ada_Project_Files.Reset;
   end Reset;

end Ocarina.Backends.Build_Utils;
