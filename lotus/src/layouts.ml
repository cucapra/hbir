(*
open Ast

(* everything having to do with memory layouts and  *)



(************************************************************** 

Relevant functions and data

****************************************************************)

(* save the data layouts for lookup later *)
let data_layout_table : old_data_layout list ref = ref ([])

(* store a layout *)
let append_data_layout (new_layout : old_data_layout) =
  let ret = data_layout_table := !data_layout_table @ [ new_layout ] in
  let _ = Printf.printf "list len internal: %d\n" (List.length !data_layout_table) in
  ret

(* search a layout list for one with a matching symbol name *)
let rec iterate_through_layout (search_name : string) (layouts : old_data_layout list ref) : old_data_layout =
  match !layouts with
    (* should fail if reach end of the list *)
    | [] -> (search_name,Global, Strided)
    (* check if there's a match, if find one, still keep going until the end *)
    | l::lt -> (
      match l with 
      | (layout_name, _, _) -> (
        if layout_name = search_name then l
        else (iterate_through_layout search_name (ref lt))
      )
    )

(* search through the data layout table for the one with the matching symbol *)
let find_data_layout_by_symbol (search_name : string) : old_data_layout =
    let ret : old_data_layout = (iterate_through_layout search_name data_layout_table) in
    ret

(* generate the symbol table before compilation (merging of the sections) begins *)
let generate_layout_symbol_table (data : data_decl) =
    apply_to_option (Some (data.data_name, Global, data.data_layout)) () (fun (d : old_data_layout) -> (append_data_layout d))
*)
