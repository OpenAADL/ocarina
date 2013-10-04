------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . B U I L D _ U T I L S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2013 ESA & ISAE.      --
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

--  This package gathers all the necessary routines to generate
--  makefiles, Ada project files or what ever seems (in the future)
--  necessary to make the build of a generated application simple. It
--  consists of several nested packages that contain mainly a couple
--  of procedures:

--  "Visit" which is a classical visitor that traverses the AADL
--  instance tree and defines the structure of the corresponding
--  entity that will be generated.

--  "Generate" which dumps the internal structure corresponding to a
--  given node into a file.

with GNAT.Dynamic_Tables;
with Ocarina.Backends.Properties;

package Ocarina.Backends.Build_Utils is

   use Ocarina.Backends.Properties;

   package Name_Tables is new GNAT.Dynamic_Tables (Name_Id, Nat, 1, 10, 10);
   --  Provides a flexible Name_Id list
   function Length (T : Name_Tables.Instance) return Int;
   --  Return the length of a name table

   function Get_Runtime_Path (Runtime_Name : String) return String;
   --  Return the directory path to the given runtime

   ----------------------------------
   -- Makefile generation routines --
   ----------------------------------

   package Makefiles is
      procedure Visit (E : Node_Id);

      generic
         with procedure Generate_Runtime_Specific
           (Appli_Name         : Name_Id;
            Node_Name          : Name_Id;
            Execution_Platform : Supported_Execution_Platform := Platform_None;
            Execution_Platform_Name : Name_Id;
            Transport_API      : Supported_Transport_APIs;
            Ada_Sources        : Name_Tables.Instance;
            Asn_Sources        : Name_Tables.Instance;
            C_Sources          : Name_Tables.Instance;
            C_Libraries        : Name_Tables.Instance;
            User_Source_Dirs   : Name_Tables.Instance;
            Use_Transport      : Boolean;
            Use_Simulink       : Boolean;
            Simulink_Directory : Name_Id;
            Simulink_Node      : Name_Id;
            Use_Scade          : Boolean;
            Scade_Directory    : Name_Id);
         --  Generate the part of the Makefile that is specific to
         --  the corresponding runtime.

      procedure Generate (E : Node_Id);
      procedure Build (E : Node_Id);
      procedure Clean (E : Node_Id);

      procedure Reset;
      --  Deallocates the internals of this package

   end Makefiles;

   ------------------------------------------
   -- Ada project file generation routines --
   ------------------------------------------

   package Ada_Project_Files is

      procedure Visit (E : Node_Id);

      generic
         with procedure Generate_Runtime_Specific
           (Appli_Name         : Name_Id;
            Node_Name          : Name_Id;
            Is_Server          : Boolean;
            Execution_Platform : Supported_Execution_Platform;
            Transport_API      : Supported_Transport_APIs;
            Spec_Names         : Name_Tables.Instance;
            Custom_Spec_Names  : Name_Tables.Instance;
            Body_Names         : Name_Tables.Instance;
            Custom_Body_Names  : Name_Tables.Instance;
            User_Source_Dirs   : Name_Tables.Instance);

      procedure Generate (E : Node_Id);

      procedure Reset;
      --  Deallocates the internals of this package

   end Ada_Project_Files;

   procedure Reset;
   --  Deallocate all the internal data

end Ocarina.Backends.Build_Utils;
