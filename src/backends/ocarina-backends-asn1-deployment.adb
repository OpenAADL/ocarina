------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . A S N 1 . D E P L O Y M E N T      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2015 ESA & ISAE.                    --
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

with Ocarina.Namet; use Ocarina.Namet;
with Utils;         use Utils;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.ASN1_Values;
with Ocarina.Backends.ASN1_Tree.Nutils;
with Ocarina.Backends.ASN1_Tree.Nodes;

package body Ocarina.Backends.ASN1.Deployment is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.ASN1_Tree.Nutils;

   package ASN1V renames Ocarina.Backends.ASN1_Values;
   package ASN1N renames Ocarina.Backends.ASN1_Tree.Nodes;
   package ASN1U renames Ocarina.Backends.ASN1_Tree.Nutils;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Device_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);
   procedure Visit_Subprogram_Instance (E : Node_Id);

   Thread_Enumeration : List_Id;
   --   The Thread_Enumeration list contains the identifiers
   --   and the associated identifier value for each thread
   --   within the distributed system.

   Thread_Id : Unsigned_Long_Long := 0;
   --  The thread identifier, unique for each thread in the
   --  DISTRIBUTED system.

   Port_Enumeration : List_Id;
   --  The Port_Enumeration list contains the enumeration of all
   --  ports used in the distributed system. In fact, we add each
   --  port in an enumeration and associate a value to them.

   Packet_Type : Node_Id;
   --  The Packet_Type node contains the definition of a packet
   --  sent by a node. It consists of a sender-thread/port id, the
   --  receiver thread/port id, and a message that is expressed
   --  using an ASN1 CHOICE.

   Port_Id : Unsigned_Long_Long := 0;
   --  The port identifier. This is unique for each port.

   Msg_Choices : List_Id;

   Module_Node : Node_Id;

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

         when others =>
            null;
      end case;
   end Visit;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      ASN1_Root :=
        Make_ASN1_File
          (Make_Defining_Identifier (Get_String_Name ("asn1_deployment")));
      ASN1N.Set_Name
        (ASN1N.Module_Node (ASN1_Root),
         Get_String_Name ("POHIC-DEPLOYMENT"));
      Module_Node := ASN1N.Module_Node (ASN1_Root);

      Thread_Enumeration := New_List (ASN1N.K_Enumerated_Value_List);
      Port_Enumeration   := New_List (ASN1N.K_Enumerated_Value_List);
      Msg_Choices        := New_List (ASN1N.K_Enumerated_Value_List);

      Visit (Root_System (E));

      if Length (Thread_Enumeration) > 0 then
         Append_Node_To_List
           (Make_Type_Definition
              (Get_String_Name ("Thread-id"),
               Make_Enumerated (Thread_Enumeration)),
            ASN1N.Definitions (Module_Node));
      end if;

      if Length (Thread_Enumeration) > 0 then
         Append_Node_To_List
           (Make_Type_Definition
              (Get_String_Name ("Port-id"),
               Make_Enumerated (Port_Enumeration)),
            ASN1N.Definitions (Module_Node));
      end if;

      declare
         Pkt_Contents : constant List_Id := ASN1U.New_List (ASN1N.K_List_Id);
      begin
         Append_Node_To_List
           (Make_Sequence_Member
              (Get_String_Name ("sender-thread"),
               Make_Defining_Identifier (Get_String_Name ("Thread-id"))),
            Pkt_Contents);

         Append_Node_To_List
           (Make_Sequence_Member
              (Get_String_Name ("sender-port"),
               Make_Defining_Identifier (Get_String_Name ("Port-id"))),
            Pkt_Contents);

         Append_Node_To_List
           (Make_Sequence_Member
              (Get_String_Name ("receiver-thread"),
               Make_Defining_Identifier (Get_String_Name ("Thread-id"))),
            Pkt_Contents);

         Append_Node_To_List
           (Make_Sequence_Member
              (Get_String_Name ("receiver-port"),
               Make_Defining_Identifier (Get_String_Name ("Port-id"))),
            Pkt_Contents);

         Append_Node_To_List
           (Make_Sequence_Member
              (Get_String_Name ("msg"),
               Make_Choice (Msg_Choices)),
            Pkt_Contents);

         Packet_Type :=
           Make_Type_Definition
             (Get_String_Name ("Pkt"),
              Make_Sequence (Pkt_Contents));
         Append_Node_To_List (Packet_Type, ASN1N.Definitions (Module_Node));
      end;

   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Thread =>
            Visit_Thread_Instance (E);

         when CC_Device =>
            Visit_Device_Instance (E);

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
      S : Node_Id;
   begin
      if not AAU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
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
      S           : Node_Id;
      F           : Node_Id;
      Call_Seq    : Node_Id;
      Spg_Call    : Node_Id;
      Thread_Name : Name_Id;
      Port_Name   : Name_Id;
      Parent_Name : Name_Id;
      --  Name of the containing process.
      Msg_Choice      : Node_Id;
      Msg_Name        : Name_Id;
      Msg_Constraints : Node_Id;
   begin

      Set_Str_To_Name_Buffer ("thread-");
      Parent_Name :=
        Display_Name
          (Identifier
             (Parent_Subcomponent
                (Parent_Component (Parent_Subcomponent (E)))));
      Get_Name_String_And_Append (Parent_Name);
      Add_Str_To_Name_Buffer ("-");
      Get_Name_String_And_Append
        (Display_Name (Identifier (Parent_Subcomponent (E))));
      Thread_Name := Name_Find;

      Thread_Name := To_Lower (Thread_Name);

      Thread_Name := Replace_Char (Thread_Name, '_', '-');
      --  We replace _ by - because ASN1 does not allow
      --  character _.

      Append_Node_To_List
        (Make_Enumerated_Value (Thread_Name, Thread_Id),
         Thread_Enumeration);

      Thread_Id := Thread_Id + 1;

      --  Here, we build the ASN1 name of the thread and add it to the
      --  Thread_Enumeration list that contains all threads of the
      --  distributed system. When we discover a thread, we increment
      --  the thread identifier so that we have a unique identifier
      --  for each thread.

      if not AAU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

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

      if Has_Ports (E) then
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance and then Is_Data (F) then
               Set_Str_To_Name_Buffer ("port-");
               Get_Name_String_And_Append (Thread_Name);
               Add_Str_To_Name_Buffer ("-");
               Get_Name_String_And_Append (Display_Name (Identifier (F)));
               Port_Name := Name_Find;
               Port_Name := To_Lower (Port_Name);

               Port_Name := Replace_Char (Port_Name, '_', '-');
               --  We replace _ by - because ASN1 does not allow
               --  character _.

               Append_Node_To_List
                 (Make_Enumerated_Value (Port_Name, Port_Id),
                  Port_Enumeration);

               Port_Id := Port_Id + 1;
               --  Here, we build the port identifier (we increment it)
               --  each time a port is discovered in a thread and add
               --  it to the Port_Enumeration list that contains all
               --  port identifiers.

               Msg_Name        := Port_Name;
               Msg_Constraints :=
                 Make_Type_Constraints
                   (Size_Down => ASN1V.New_Int_Value (0, 1, 10),
                    Size_Up   =>
                      ASN1V.New_Int_Value
                        (To_Bytes (Get_Data_Size (Corresponding_Instance (F))),
                         1,
                         10));
               Msg_Choice :=
                 Make_Choice_Member
                   (Msg_Name,
                    Make_Type_Designator
                      (Type_Name =>
                         Make_Defining_Identifier
                           (Get_String_Name ("OCTET STRING")),
                       Type_Constraints => Msg_Constraints));

               Append_Node_To_List (Msg_Choice, Msg_Choices);

            end if;
            F := Next_Node (F);
         end loop;
      end if;
   end Visit_Thread_Instance;

   -------------------------------
   -- Visit_Subprogram_Instance --
   -------------------------------

   procedure Visit_Subprogram_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Subprogram_Instance;

   ---------------------------
   -- Visit_Device_Instance --
   ---------------------------

   procedure Visit_Device_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Device_Instance;

end Ocarina.Backends.ASN1.Deployment;
