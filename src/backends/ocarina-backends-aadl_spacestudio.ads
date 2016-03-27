--  This is the root unit of the AADL SpaceStudio backend. This
--  backend generates:
--  - a set of Python units that represent AADL model elements
--  - System C files from AADL elements.

package Ocarina.Backends.AADL_SpaceStudio is

   procedure Generate (AADL_Root : Node_Id);

   procedure Init;

   procedure Reset;

private
   Python_Root             : Node_Id;
   Current_Python_Node     : Node_Id;
   Distributed_Application : Node_Id;
   HI_Node                 : Node_Id;
   HI_Unit                 : Node_Id;

end Ocarina.Backends.AADL_SpaceStudio;
