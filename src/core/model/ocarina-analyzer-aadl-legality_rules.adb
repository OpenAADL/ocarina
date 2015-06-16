------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.ANALYZER.AADL.LEGALITY_RULES                    --
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

with Ocarina.Analyzer.Messages;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.Analyzer.AADL.Legality_Rules is

   use Ocarina.Analyzer.Messages;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;

   function Check_Rules_In_Component_Type (Node : Node_Id) return Boolean;

   function Check_Rules_In_Component_Implementation
     (Node : Node_Id) return Boolean;

   ----------------------------
   -- A_Component_Connection --
   ----------------------------

   function A_Component_Connection
     (Component  : Node_Id;
      Connection : Node_Id) return Boolean
   is
      pragma Assert (Present (Component) and then Present (Connection));

      OK               : Boolean;
      Component_Categ  : Component_Category;
      Connection_Categ : Connection_Type;
   begin

      if Kind (Component) /= K_Component_Implementation then
         DAE
           (Loc      => Loc (Component),
            Node1    => Component,
            Message1 => " is not a component implementation");
         return False;
      end if;

      if Kind (Connection) /= K_Connection then
         DAE
           (Loc      => Loc (Connection),
            Node1    => Connection,
            Message1 => " is not a connection.");
         return False;
      end if;

      --  Check the validity of the composition

      Component_Categ  := Component_Category'Val (Category (Component));
      Connection_Categ := Connection_Type'Val (Category (Connection));

      case AADL_Version is
         when AADL_V1 =>
            case Component_Categ is
               when CC_Data =>
                  OK :=
                    Connection_Categ = CT_Access_Bus
                    or else Connection_Categ = CT_Access_Data;

               when CC_Subprogram |
                 CC_System        |
                 CC_Thread_Group  |
                 CC_Process       |
                 CC_Thread        |
                 CC_Abstract      =>
                  OK := True;

               when CC_Bus    |
                 CC_Processor |
                 CC_Memory    |
                 CC_Device    |
                 CC_Unknown   =>
                  OK := False;

               when others =>
                  OK := False;
            end case;

         when AADL_V2 =>
            case Component_Categ is

               when CC_Data      |
                 CC_Subprogram   |
                 CC_System       |
                 CC_Thread_Group |
                 CC_Process      |
                 CC_Thread       |
                 CC_Abstract     |
                 CC_Bus          |
                 CC_Virtual_Bus  |
                 CC_Device       =>
                  OK := True;

               when CC_Subprogram_Group =>
                  OK := Connection_Categ = CT_Access_Subprogram;

               when CC_Processor =>
                  OK := Connection_Categ = CT_Access_Bus;

               when CC_Virtual_Processor =>
                  OK := Connection_Categ = CT_Access_Virtual_Bus;

               when CC_Memory =>
                  OK := Connection_Categ = CT_Access_Bus;

               when others =>
                  OK := False;
            end case;
      end case;

      if OK then
         return True;
      else
         DAE
           (Loc      => Loc (Connection),
            Node1    => Component,
            Node2    => Connection,
            Message1 => "cannot have ",
            Message2 => " as a connection");
         return False;
      end if;
   end A_Component_Connection;

   -------------------------
   -- A_Component_Feature --
   -------------------------

   function A_Component_Feature
     (Component : Node_Id;
      Feature   : Node_Id) return Boolean
   is
      pragma Assert (Present (Component) and then Present (Feature));

      OK              : Boolean;
      Component_Categ : Component_Category;
   begin

      if Kind (Component) /= K_Component_Type
        and then not Is_Refinement (Feature)
      then
         DAE
           (Loc      => Loc (Component),
            Node1    => Component,
            Message1 => " is not a component type");
         return False;
      end if;

      if Kind (Feature) /= K_Feature_Group_Spec
        and then Kind (Feature) /= K_Port_Spec
        and then Kind (Feature) /= K_Parameter
        and then Kind (Feature) /= K_Subprogram_Spec
        and then Kind (Feature) /= K_Subcomponent_Access
      then
         DAE
           (Loc      => Loc (Feature),
            Node1    => Feature,
            Message1 => " not a feature.");
         return False;
      end if;

      --  Check the validity of the composition

      Component_Categ := Component_Category'Val (Category (Component));

      case AADL_Version is
         when AADL_V1 =>
            case Component_Categ is
               when CC_Data =>
                  OK :=
                    (Kind (Feature) = K_Subprogram_Spec
                     or else
                     (Kind (Feature) = K_Subcomponent_Access
                      and then Is_Provided (Feature)
                      and then
                        Component_Category'Val
                          (Subcomponent_Category (Feature)) =
                        CC_Data));

               when CC_Subprogram =>
                  OK :=
                    Kind (Feature) = K_Parameter
                    or else Kind (Feature) = K_Feature_Group_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then not Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Data)
                    or else
                    (Kind (Feature) = K_Port_Spec
                     and then not Is_In (Feature)
                     and then Is_Event (Feature));
               when CC_Thread | CC_Thread_Group | CC_Process =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else Kind (Feature) = K_Port_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Data)
                    or else
                    (Kind (Feature) = K_Subprogram_Spec
                     and then Is_Server (Feature));

               when CC_Processor =>
                  OK :=
                    ((Kind (Feature) = K_Subprogram_Spec
                      and then Is_Server (Feature))
                     or else Kind (Feature) = K_Port_Spec
                     or else Kind (Feature) = K_Feature_Group_Spec
                     or else
                     (Kind (Feature) = K_Subcomponent_Access
                      and then not Is_Provided (Feature)
                      and then
                        Component_Category'Val
                          (Subcomponent_Category (Feature)) =
                        CC_Bus));

               when CC_Memory | CC_Bus =>
                  OK :=
                    Kind (Feature) = K_Subcomponent_Access
                    and then not Is_Provided (Feature)
                    and then
                      Component_Category'Val
                        (Subcomponent_Category (Feature)) =
                      CC_Bus;

               when CC_Device =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else Kind (Feature) = K_Port_Spec
                    or else
                    (Kind (Feature) = K_Subprogram_Spec
                     and then Is_Server (Feature))
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then not Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Bus);

               when CC_System =>
                  OK :=
                    (Kind (Feature) = K_Subprogram_Spec
                     and then Is_Server (Feature))
                    or else Kind (Feature) = K_Port_Spec
                    or else Kind (Feature) = K_Feature_Group_Spec
                    or else Kind (Feature) = K_Subcomponent_Access;

               when others =>
                  OK := False;
            end case;

         when AADL_V2 =>
            case Component_Categ is
               when CC_Data =>
                  OK :=
                    (Kind (Feature) = K_Feature_Group_Spec
                     or else
                     (Kind (Feature) = K_Subcomponent_Access
                      and then
                        Component_Category'Val
                          (Subcomponent_Category (Feature)) =
                        CC_Subprogram)
                     or else
                     (Kind (Feature) = K_Subcomponent_Access
                      and then not Is_Provided (Feature)
                      and then
                        Component_Category'Val
                          (Subcomponent_Category (Feature)) =
                        CC_Subprogram_Group));

               when CC_Subprogram =>
                  OK :=
                    Kind (Feature) = K_Parameter
                    or else Kind (Feature) = K_Feature_Group_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then not Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Data)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then not Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then not Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group)
                    or else
                    (Kind (Feature) = K_Port_Spec
                     and then not Is_In (Feature)
                     and then Is_Event (Feature));

               when CC_Subprogram_Group =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then not Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group);

               when CC_Thread | CC_Thread_Group | CC_Process =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else Kind (Feature) = K_Port_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Data)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group);

               when CC_Processor =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else Kind (Feature) = K_Port_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Bus);

               when CC_Virtual_Processor =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else Kind (Feature) = K_Port_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group);

               when CC_Memory =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Bus);

               when CC_Bus =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then not Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Bus);

               when CC_Device =>
                  OK :=
                    Kind (Feature) = K_Feature_Group_Spec
                    or else Kind (Feature) = K_Port_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then Is_Provided (Feature)
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group)
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                       Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Bus);

               when CC_System =>
                  OK :=
                    Kind (Feature) = K_Port_Spec
                    or else Kind (Feature) = K_Feature_Group_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                     ((Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Bus)
                      or else
                      (Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                      or else
                      (Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Data)
                      or else
                      (Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group)));

               when CC_Abstract =>
                  OK :=
                    Kind (Feature) = K_Port_Spec
                    or else Kind (Feature) = K_Feature_Group_Spec
                    or else
                    (Kind (Feature) = K_Subcomponent_Access
                     and then
                     ((Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram_Group)
                      or else
                      (Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Subprogram)
                      or else
                      (Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Data)
                      or else
                      (Component_Category'Val
                         (Subcomponent_Category (Feature)) =
                       CC_Bus)));

               when others =>
                  OK := False;
            end case;
      end case;

      if OK then
         return True;
      else
         DAE
           (Loc      => Loc (Feature),
            Node1    => Component,
            Node2    => Feature,
            Message1 => "cannot have ",
            Message2 => " as a feature");
         return False;
      end if;

   end A_Component_Feature;

   ----------------------
   -- A_Component_Flow --
   ----------------------

   function A_Component_Flow
     (Component : Node_Id;
      Flow      : Node_Id) return Boolean
   is
      pragma Assert (Present (Component) and then Present (Flow));

      OK              : Boolean;
      Component_Categ : Component_Category;
   begin

      if Kind (Component) /= K_Component_Implementation then
         DAE
           (Loc      => Loc (Component),
            Node1    => Component,
            Message1 => " is not a component implementation");
         return False;
      end if;

      if Kind (Flow) /= K_Flow_Implementation
        and then Kind (Flow) /= K_End_To_End_Flow_Spec
        and then Kind (Flow) /= K_Flow_Implementation_Refinement
        and then Kind (Flow) /= K_End_To_End_Flow_Refinement
      then
         DAE (Loc => Loc (Flow), Node1 => Flow, Message1 => " is not a flow.");
         return False;
      end if;

      --  Check the validity of the composition

      Component_Categ := Component_Category'Val (Category (Component));

      case AADL_Version is
         when AADL_V1 =>
            case Component_Categ is
               when CC_Subprogram |
                 CC_System        |
                 CC_Thread_Group  |
                 CC_Thread        |
                 CC_Process       |
                 CC_Processor     |
                 CC_Device        =>
                  OK := True;

               when CC_Data | CC_Bus | CC_Memory | CC_Unknown =>
                  OK := False;

               when others =>
                  OK := False;
            end case;

         when AADL_V2 =>
            case Component_Categ is
               when CC_Subprogram     |
                 CC_System            |
                 CC_Thread_Group      |
                 CC_Thread            |
                 CC_Process           |
                 CC_Processor         |
                 CC_Virtual_Processor |
                 CC_Device            |
                 CC_Abstract          =>
                  OK := True;

               when CC_Data          |
                 CC_Bus              |
                 CC_Memory           |
                 CC_Subprogram_Group |
                 CC_Virtual_Bus      |
                 CC_Unknown          =>
                  OK := False;

            end case;
      end case;

      if OK then
         return True;
      else
         DAE
           (Loc      => Loc (Flow),
            Node1    => Component,
            Node2    => Flow,
            Message1 => "cannot have ",
            Message2 => " as a flow");
         return False;
      end if;
   end A_Component_Flow;

   ------------------------------------
   -- A_Component_Flow_Specification --
   ------------------------------------

   function A_Component_Flow_Specification
     (Component : Node_Id;
      Flow_Spec : Node_Id) return Boolean
   is
      pragma Assert (Present (Component) and then Present (Flow_Spec));

      OK              : Boolean;
      Component_Categ : Component_Category;
   begin

      if Kind (Component) /= K_Component_Type then
         DAE
           (Loc      => Loc (Component),
            Node1    => Component,
            Message1 => " is not a component type");
         return False;
      end if;

      if Kind (Flow_Spec) /= K_Flow_Spec then
         DAE
           (Loc      => Loc (Flow_Spec),
            Node1    => Flow_Spec,
            Message1 => " is not a flow specification.");
         return False;
      end if;

      --  Check the validity of the composition

      Component_Categ := Component_Category'Val (Category (Component));

      case AADL_Version is
         when AADL_V1 =>
            case Component_Categ is
               when CC_Subprogram |
                 CC_System        |
                 CC_Thread_Group  |
                 CC_Thread        |
                 CC_Process       |
                 CC_Processor     |
                 CC_Device        =>
                  OK := True;

               when CC_Data | CC_Bus | CC_Memory | CC_Unknown =>
                  OK := False;

               when others =>
                  OK := False;
            end case;

         when AADL_V2 =>
            case Component_Categ is
               when CC_Subprogram     |
                 CC_System            |
                 CC_Thread_Group      |
                 CC_Thread            |
                 CC_Process           |
                 CC_Processor         |
                 CC_Virtual_Processor |
                 CC_Device            |
                 CC_Abstract          =>
                  OK := True;

               when CC_Data          |
                 CC_Bus              |
                 CC_Memory           |
                 CC_Subprogram_Group |
                 CC_Virtual_Bus      |
                 CC_Unknown          =>
                  OK := False;

            end case;
      end case;

      if OK then
         return True;
      else
         DAE
           (Loc      => Loc (Flow_Spec),
            Node1    => Component,
            Node2    => Flow_Spec,
            Message1 => "cannot have ",
            Message2 => " as a flow specification");
         return False;
      end if;
   end A_Component_Flow_Specification;

   ------------------------------
   -- A_Component_Subcomponent --
   ------------------------------

   function A_Component_Subcomponent
     (Component    : Node_Id;
      Subcomponent : Node_Id) return Boolean
   is
      pragma Assert (Present (Component) and then Present (Subcomponent));

      OK                 : Boolean;
      Component_Categ    : Component_Category;
      Subcomponent_Categ : Component_Category;
   begin
      if Kind (Component) /= K_Component_Implementation then
         DAE
           (Loc      => Loc (Component),
            Node1    => Component,
            Message1 => " is not a component implementation");
         return False;
      end if;

      if Kind (Subcomponent) /= K_Subcomponent then
         DAE
           (Loc      => Loc (Subcomponent),
            Node1    => Subcomponent,
            Message1 => " is not a subcomponent");
         return False;
      end if;

      --  Check the validity of the composition

      Component_Categ    := Component_Category'Val (Category (Component));
      Subcomponent_Categ := Component_Category'Val (Category (Subcomponent));

      case AADL_Version is
         when AADL_V1 =>
            case Component_Categ is
               when CC_Data =>
                  case Subcomponent_Categ is
                     when CC_Data =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Subprogram | CC_Device =>
                  OK := False;

               when CC_Thread =>
                  case Subcomponent_Categ is
                     when CC_Data =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Thread_Group | CC_Process =>
                  case Subcomponent_Categ is
                     when CC_Data | CC_Thread | CC_Thread_Group =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Memory =>
                  case Subcomponent_Categ is
                     when CC_Memory =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Processor =>
                  case Subcomponent_Categ is
                     when CC_Memory =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_System =>
                  case Subcomponent_Categ is
                     when CC_Data   |
                       CC_Process   |
                       CC_Processor |
                       CC_Memory    |
                       CC_Device    |
                       CC_Bus       |
                       CC_System    =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when others =>
                  OK := False;
            end case;

         when AADL_V2 =>
            case Component_Categ is
               when CC_Data =>
                  case Subcomponent_Categ is
                     when CC_Abstract | CC_Data | CC_Subprogram =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Subprogram =>
                  case Subcomponent_Categ is
                     when CC_Abstract | CC_Data =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Subprogram_Group =>
                  case Subcomponent_Categ is
                     when CC_Subprogram | CC_Subprogram_Group =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Thread =>
                  case Subcomponent_Categ is
                     when CC_Abstract      |
                       CC_Data             |
                       CC_Subprogram       |
                       CC_Subprogram_Group =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Thread_Group | CC_Process =>
                  case Subcomponent_Categ is
                     when CC_Abstract      |
                       CC_Data             |
                       CC_Thread           |
                       CC_Thread_Group     |
                       CC_Subprogram       |
                       CC_Subprogram_Group =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Processor =>
                  case Subcomponent_Categ is
                     when CC_Abstract       |
                       CC_Bus               |
                       CC_Memory            |
                       CC_Virtual_Processor |
                       CC_Virtual_Bus       =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Virtual_Processor =>
                  case Subcomponent_Categ is
                     when CC_Abstract       |
                       CC_Virtual_Processor |
                       CC_Virtual_Bus       =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Memory =>
                  case Subcomponent_Categ is
                     when CC_Abstract | CC_Memory | CC_Bus =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Bus =>
                  case Subcomponent_Categ is
                     when CC_Abstract | CC_Virtual_Bus =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Virtual_Bus =>
                  case Subcomponent_Categ is
                     when CC_Abstract | CC_Virtual_Bus =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Device =>
                  case Subcomponent_Categ is
                     when CC_Bus | CC_Abstract =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_System =>
                  case Subcomponent_Categ is
                     when CC_Data           |
                       CC_Subprogram        |
                       CC_Subprogram_Group  |
                       CC_Process           |
                       CC_Processor         |
                       CC_Virtual_Processor |
                       CC_Memory            |
                       CC_Bus               |
                       CC_Virtual_Bus       |
                       CC_Device            |
                       CC_System            |
                       CC_Abstract          =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when CC_Abstract =>
                  case Subcomponent_Categ is
                     when CC_Data           |
                       CC_Subprogram        |
                       CC_Subprogram_Group  |
                       CC_Thread            |
                       CC_Thread_Group      |
                       CC_Process           |
                       CC_Processor         |
                       CC_Virtual_Processor |
                       CC_Virtual_Bus       |
                       CC_Memory            |
                       CC_Device            |
                       CC_Bus               |
                       CC_System            |
                       CC_Abstract          =>
                        OK := True;
                     when others =>
                        OK := False;
                  end case;

               when others =>
                  OK := False;
            end case;
      end case;

      if OK then
         return True;
      else
         DAE
           (Loc      => Loc (Subcomponent),
            Node1    => Component,
            Node2    => Subcomponent,
            Message1 => "cannot have ",
            Message2 => " as a subcomponent");
         return False;
      end if;
   end A_Component_Subcomponent;

   ------------------------------------------
   -- A_Component_Subprogram_Call_Sequence --
   ------------------------------------------

   function A_Component_Subprogram_Call_Sequence
     (Component           : Node_Id;
      Subprogram_Call_Seq : Node_Id) return Boolean
   is
      pragma Assert
        (Present (Component) and then Present (Subprogram_Call_Seq));

      OK              : Boolean;
      Component_Categ : Component_Category;
   begin

      if Kind (Component) /= K_Component_Implementation then
         DAE
           (Loc      => Loc (Component),
            Node1    => Component,
            Message1 => " is not a component implementation");
         return False;
      end if;

      if Kind (Subprogram_Call_Seq) /= K_Subprogram_Call_Sequence then
         DAE
           (Loc      => Loc (Subprogram_Call_Seq),
            Node1    => Subprogram_Call_Seq,
            Message1 => " is not a subprogram call sequence.");
         return False;
      end if;

      --  Check the validity of the composition

      Component_Categ := Component_Category'Val (Category (Component));

      case Component_Categ is
         when CC_Subprogram | CC_Thread | CC_Abstract =>
            OK := True;

         when CC_Data           |
           CC_Thread_Group      |
           CC_Process           |
           CC_Bus               |
           CC_Processor         |
           CC_Virtual_Processor |
           CC_Virtual_Bus       |
           CC_Memory            |
           CC_Device            |
           CC_System            |
           CC_Subprogram_Group  |
           CC_Unknown           =>
            OK := False;
      end case;

      if OK then
         return True;
      else
         DAE
           (Loc      => Loc (Subprogram_Call_Seq),
            Node1    => Component,
            Node2    => Subprogram_Call_Seq,
            Message1 => "cannot have ",
            Message2 => " as a subprogram call");
         return False;
      end if;
   end A_Component_Subprogram_Call_Sequence;

   -----------------------------------
   -- Check_Rules_In_Component_Type --
   -----------------------------------

   function Check_Rules_In_Component_Type (Node : Node_Id) return Boolean is
      pragma Assert (Kind (Node) = K_Component_Type);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      --  Features

      if not Is_Empty (Features (Node)) then
         List_Node := First_Node (Features (Node));

         while Present (List_Node) loop
            Success := A_Component_Feature (Node, List_Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if not Is_Empty (Flows (Node)) then
         List_Node := First_Node (Flows (Node));

         while Present (List_Node) loop
            Success :=
              A_Component_Flow_Specification (Node, List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Rules_In_Component_Type;

   ---------------------------------------------
   -- Check_Rules_In_Component_Implementation --
   ---------------------------------------------

   function Check_Rules_In_Component_Implementation
     (Node : Node_Id) return Boolean
   is
      pragma Assert (Kind (Node) = K_Component_Implementation);

      List_Node : Node_Id;
      Success   : Boolean := True;
   begin
      --  Type refinements

      if not Is_Empty (Refines_Type (Node)) then
         List_Node := First_Node (Refines_Type (Node));

         while Present (List_Node) loop
            Success := A_Component_Feature (Node, List_Node) and then Success;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Subcomponents

      if not Is_Empty (Subcomponents (Node)) then
         List_Node := First_Node (Subcomponents (Node));

         while Present (List_Node) loop
            Success :=
              A_Component_Subcomponent (Node, List_Node) and then Success;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Call sequences

      if not Is_Empty (Calls (Node)) then
         List_Node := First_Node (Calls (Node));

         while Present (List_Node) loop
            Success :=
              A_Component_Subprogram_Call_Sequence (Node, List_Node)
              and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Connections

      if not Is_Empty (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node)) then
         List_Node :=
           First_Node (Ocarina.ME_AADL.AADL_Tree.Nodes.Connections (Node));

         while Present (List_Node) loop
            Success :=
              A_Component_Connection (Node, List_Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      --  Flows

      if not Is_Empty (Flows (Node)) then
         List_Node := First_Node (Flows (Node));

         while Present (List_Node) loop
            Success   := A_Component_Flow (Node, List_Node) and then Success;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Rules_In_Component_Implementation;

   --------------------------
   -- Check_Legality_Rules --
   --------------------------

   function Check_Legality_Rules (Root : Node_Id) return Boolean is
      pragma Assert (Kind (Root) = K_AADL_Specification);

      List_Node         : Node_Id;
      Package_List_Node : Node_Id;
      Success           : Boolean := True;
   begin
      if not Is_Empty (Declarations (Root)) then
         List_Node := First_Node (Declarations (Root));

         while Present (List_Node) loop
            case Kind (List_Node) is
               when K_Component_Type =>
                  Success :=
                    Check_Rules_In_Component_Type (List_Node) and then Success;

               when K_Component_Implementation =>
                  Success :=
                    Check_Rules_In_Component_Implementation (List_Node)
                    and then Success;

               when K_Package_Specification =>
                  if not Is_Empty (Declarations (List_Node)) then
                     Package_List_Node :=
                       First_Node (Declarations (List_Node));

                     while Package_List_Node /= No_Node loop
                        case Kind (Package_List_Node) is
                           when K_Component_Type =>
                              Success :=
                                Check_Rules_In_Component_Type
                                  (Package_List_Node)
                                and then Success;

                           when K_Component_Implementation =>
                              Success :=
                                Check_Rules_In_Component_Implementation
                                  (Package_List_Node)
                                and then Success;

                           when others =>
                              null;
                        end case;

                        Package_List_Node := Next_Node (Package_List_Node);
                     end loop;
                  end if;

               when others =>
                  null;
            end case;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Check_Legality_Rules;

end Ocarina.Analyzer.AADL.Legality_Rules;
