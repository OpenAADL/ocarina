----------------------------------------------------
--    This file should contains all functionnal   --
--        description, defined by user            --
----------------------------------------------------

--  syntaxic tips :

--  a subprogram functionnal definition must begin and end by 
--  transitions respectively named "$(subprogram_name)_func_enter"
--  and "$(subprogram_name)_func_leave".

--  a sequence choosing algorithm must refers to existing sequences 
--  by using their first and last transition's name (resp. 
--  "$(full_thread_name)_$(sequence_name)_begin" and 
--  "$(full_thread_name)_$(sequence_name)_end")

--  all places or transitions which refers to exisiting nodes (in
--  generated code) must be prefixed by "UD_". 

--  all nodes refering to existing node must be put in either the
--  places_to_merge or the transitions_to_merge list.

list places_to_merge;
list transitions_to_merge;

-- ADD FUNCTIONNAL DESCRIPTION HERE
