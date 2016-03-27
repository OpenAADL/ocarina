with Ada.Containers.Doubly_Linked_Lists;

generic
   type Element_Type is private;

package Fifo is
   package List_Pkg is new Ada.Containers.Doubly_Linked_Lists (Element_Type);
   use List_Pkg;
   type Fifo_Type is new List with null record;

   procedure Push (List : in out Fifo_Type; Item : in Element_Type);
   procedure Pop (List : in out Fifo_Type; Item : out Element_Type);
   function Is_Empty (List : Fifo_Type) return Boolean;

   Empty_Error : exception;

end Fifo;
