(* everything having to do with memory layouts and  *)

(************************************************************** 

Typedefs

****************************************************************)

type mem_type =
    | Global
    | Local

type mem_location =
    | Host
    | Device

(* data distribution policy *)
type dist_policy = 
    | Chunked
    | Strided
    (* if custom than allow code to be written there {} *)
    | Custom

(* memory layout that multiple data maps can share *)
(* mem-type (global/local) ~ hostToDevice or deviceToHost ~ symbol name ~ 
   [x] ~ [y] ~ *)
(* layout name ~ physical storage (TODO: default to global no coords) ~ distribution ~ transfer type*)
type data_layout = string * mem_type * dist_policy


(************************************************************** 

Relevant functions and data

****************************************************************)


(* save the data layouts for lookup later *)
let data_layout_table : data_layout list = []

(* store a layout *)
let append_data_layout (new_layout : data_layout) =
  data_layout_table = new_layout::data_layout_table

(* search a layout list for one with a matching symbol name *)
let rec iterate_through_layout (search_name : string) (layouts : data_layout list) : data_layout =
  match layouts with
    (* should fail if reach end of the list *)
    | [] -> (search_name,Global, Chunked)
    (* check if there's a match, if find one, still keep going until the end *)
    | l::lt -> (
      match l with 
      | (layout_name, _, _) -> (
        if layout_name = search_name then l
        else (iterate_through_layout search_name lt)
      )
    )

(* search through the data layout table for the one with the matching symbol *)
let find_data_layout_by_symbol (search_name : string) : data_layout =
    let _ = (append_data_layout (search_name,Global, Chunked)) in
    let ret : data_layout = (iterate_through_layout search_name data_layout_table) in
    ret

