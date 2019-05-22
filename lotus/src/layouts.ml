open Ast

(* everything having to do with memory layouts and  *)



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
    | [] -> (search_name,Global, Strided)
    (* check if there's a match, if find one, still keep going until the end *)
    | l::lt -> (
      match l with 
      | (layout_name, _, _) -> (
        Printf.printf "%s\n" layout_name;
        if layout_name = search_name then l
        else (iterate_through_layout search_name lt)
      )
    )

(* search through the data layout table for the one with the matching symbol *)
let find_data_layout_by_symbol (search_name : string) : data_layout =
    let ret : data_layout = (iterate_through_layout search_name data_layout_table) in
    ret

(* generate the symbol table before compilation (merging of the sections) begins *)
let generate_layout_symbol_table (data : data_decl) =
  match data with
  | (_, _, lyt) -> (
    (apply_to_option lyt true (fun (d : data_layout) -> (append_data_layout d)))
  )
