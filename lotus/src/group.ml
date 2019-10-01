type abs_arrangement = {
  abs_arr_grid_size : int * int;
  abs_arr_group_arrays : abs_group_array list }

and abs_group_array = {
  ga_rel_name : string;
  ga_dim_bounds : int list;
  ga_groups : abs_group list }

and abs_group = {
  (* relative name; absolute name computable with index *)
  g_path : Ast.group_path; 

  (* closed intervals for absolute ranges *)
  g_abs_row_range : int * int; 
  g_abs_col_range : int * int; 

  g_subgroups : abs_group_array list }

and ga_index = int list

let rec ga_index_to_string ix : string = 
  begin match ix with
  | [] -> ""
  | i::is -> Printf.sprintf "[%d]" i ^ ga_index_to_string is
  end

let rec cross_bounds (bounds : int list) : ga_index list =
  let rec range (n : int) : ga_index list = 
    if n <= 0 then []
    else [n-1]::(range (n-1)) in
  let grow_ixs (i : int) (ixs : ga_index list)  = 
    List.map (fun ix -> ix@[i]) ixs in
  match bounds with
  | [] -> []
  | b::[] -> range b
  | _::0::[] -> []
  | b1::b2::[] -> 
    (grow_ixs (b2-1) (range b1)) @ cross_bounds (b1::[b2-1])
  | _ -> failwith "unimplemented cross product"

(* Relative to Absolute group translation *)
let arrange_decl_to_abs_arrangement (tile_grid_size : int * int) 
                                    (arr_decl : Ast.arrange_decl) : abs_arrangement =

  let rec group_decl_to_group_array (par_group_path : Ast.group_path) 
                                    (par_ix_ctxt : Utils.ctxt)
                                    (par_size_ctxt : Utils.ctxt)
                                    (par_origin : int * int)
                                    (g_decl : Ast.group_decl) : abs_group_array =
    (* generate all indices from iterators *)
    let ((indices, dim_bounds) : Utils.ctxt list * int list) = 
      match g_decl.Ast.gd_dim_iters with
      | [] -> [[]], []
      | _ -> 
        let eval_ctxt : Utils.ctxt = par_ix_ctxt @ par_size_ctxt in
        let ix_names, ix_bound_exprs = List.split g_decl.Ast.gd_dim_iters in
        let ix_bounds : int list =
          List.map (Utils.eval_int_expr eval_ctxt) ix_bound_exprs in
        let ixs : ga_index list = cross_bounds ix_bounds in
        List.map (fun ix -> List.combine ix_names ix) ixs, ix_bounds in

    (* one relative group corresponds to many absolute groups (i.e., group array) *)
    { ga_rel_name = g_decl.Ast.gd_name;
      ga_dim_bounds = dim_bounds;
      ga_groups = 
        List.map 
        (rel_ix_group_to_abs_ix_group par_group_path par_ix_ctxt par_size_ctxt g_decl par_origin) 
        indices }

  (* calculate indexed absolute group corresponding to relative group at a given index
   * note that relative subgroups are recursively parametrized by all given indices *)
 and rel_ix_group_to_abs_ix_group (par_group_path : Ast.group_path) 
                                  (par_ix_ctxt : Utils.ctxt)
                                  (par_size_ctxt : Utils.ctxt)
                                  (g_decl : Ast.group_decl)
                                  (abs_offset : int * int)
                                  (ix_ctxt : Utils.ctxt) : abs_group =
    let abs_row_offset, abs_col_offset = abs_offset in 
    let row_size_name, row_range = g_decl.Ast.gd_row_range in
    let col_size_name, col_range = g_decl.Ast.gd_col_range in

    let full_ctxt : Utils.ctxt = par_size_ctxt @ par_ix_ctxt @ ix_ctxt in
    let min_row, max_row = Utils.eval_range full_ctxt row_range in
    let min_col, max_col = Utils.eval_range full_ctxt col_range in

    let subgroup_par_size_ctxt = 
      [(row_size_name, (max_row - min_row));
       (col_size_name, (max_col - min_col))] in
    let subgroup_ix_ctxt = par_ix_ctxt @ ix_ctxt in

    (* evaluate nested group_decls only using index bindings; we remove parent size bindings *)
    let _, ix = List.split ix_ctxt in 
    let path = par_group_path @ [g_decl.Ast.gd_name, ix] in
    let subgroups : abs_group_array list =
      List.map
      (group_decl_to_group_array
       path
       subgroup_ix_ctxt 
       subgroup_par_size_ctxt 
       (min_row + abs_row_offset, min_col + abs_col_offset))
      g_decl.Ast.gd_subgroups in

    { g_path = path;
      g_abs_row_range = 
        (min_row + abs_row_offset, 
        max_row - 1 + abs_row_offset);
      g_abs_col_range = 
        (min_col + abs_col_offset, 
        max_col - 1 + abs_col_offset);
      g_subgroups = subgroups; } in


  let tile_size_bindings : Utils.ctxt = 
    let tile_rows_name, tile_cols_name = arr_decl.Ast.arr_size_vars in
    let tile_rows, tile_cols = tile_grid_size in
    [(tile_rows_name, tile_rows); (tile_cols_name, tile_cols)] in

  { abs_arr_grid_size = tile_grid_size;
    abs_arr_group_arrays = 
      List.map
      (group_decl_to_group_array [] [] tile_size_bindings (0,0)) 
      arr_decl.Ast.arr_groups }


let abs_arrangement_from_prog (prog : Ast.program) : abs_arrangement =
  let tile_grid_size : int list = 
    List.fold_left 
    (fun tiles targ_decl -> 
      match targ_decl with 
      | Ast.TileDecl tile_decl -> tile_decl::tiles
      | _ -> tiles)
    []
    prog.Ast.target_section
    |> List.hd
    |> (fun t -> t.Ast.tile_dims)
    |> List.map (Utils.eval_expr [])
    |> List.map Utils.int_of_value in
    arrange_decl_to_abs_arrangement 
    (List.nth tile_grid_size 0, List.nth tile_grid_size 1)
    (List.hd prog.config_section) 
    



(* Group Pattern Matching *)
let rec match_pattern_with_path (pattern : Ast.group_pattern)
                                (path : Ast.group_path) : bool =
  match pattern, path with
  | (g_name1, patt_ix)::rem_patt, (g_name2, path_ix)::rem_path -> 
      g_name1 = g_name2 && 
      match_pattern_ix_with_path_ix patt_ix path_ix &&
      match_pattern_with_path rem_patt rem_path
  | (g_name, _)::_, _ -> failwith ("unknown name in pattern: " ^ g_name)
  | _, _ -> true

and match_pattern_ix_with_path_ix (patt_ix : Ast.ix_elem_pattern list) 
                                  (path_ix : int list) : bool = 
  match patt_ix, path_ix with
  | [], [] -> true
  | (SymIx _)::rem_patt_ix, _::rem_path_ix -> 
      match_pattern_ix_with_path_ix rem_patt_ix rem_path_ix
  | (ConcIx n1)::rem_patt_ix, n2::rem_path_ix -> 
      if n1 = n2 
      then match_pattern_ix_with_path_ix rem_patt_ix rem_path_ix
      else false
  | _, _ -> failwith "index matching failed: pattern size mismatch"


let match_in_abs_arrangement (pattern : Ast.group_pattern)
                             (arr : abs_arrangement)
                             : abs_group list =
  let rec match_in_group_array (pattern : Ast.group_pattern)
                               (ga : abs_group_array) : abs_group list =
    ga.ga_groups
    |> List.map (match_in_group pattern)
    |> List.concat
    |> List.map (fun ga -> ga.ga_groups)
    |> List.concat

  and match_in_group (pattern : Ast.group_pattern)
                     (g : abs_group) : abs_group_array list =
    if match_pattern_with_path pattern g.g_path
    then  List.map (match_in_group_array pattern) g.g_subgroups
          |> List.concat
          |> List.map (fun g -> g.g_subgroups)
          |> List.concat
    else g.g_subgroups in
    
  List.map (match_in_group_array pattern) arr.abs_arr_group_arrays |> List.concat

let match_in_program (prog : Ast.program) 
                     (gp : Ast.group_pattern) : abs_group list =
  let arr : abs_arrangement = abs_arrangement_from_prog prog in
  match gp with
  (* first element of pattern is tile grid name 
   * in the future, perhaps match on list of arrangements *)
  | (_, [])::gps -> match_in_abs_arrangement gps arr
  | _ -> failwith "cannot index into physical tile grid"



(* group printing on command line *)
let rec string_of_path (path : Ast.group_path) : string =

  let string_of_path_head (g_name : string) (ix : int list) : string =
    let string_of_ix (ix : int list) : string = 
      List.map string_of_int ix |> String.concat "," in
    Printf.sprintf "%s[%s]" g_name (string_of_ix ix) in

  match path with
  | [] -> ""
  | (g_name, ix)::[] -> string_of_path_head g_name ix
  | (g_name, ix)::rem_path -> 
      string_of_path_head g_name ix ^ "." ^ string_of_path rem_path

let rec string_of_abs_group_array (depth : int) (ga : abs_group_array) : string =
  let string_of_array_size = 
    List.map string_of_int ga.ga_dim_bounds |> String.concat " * " in

  let string_of_array_header = 
    Printf.sprintf "group_array %s of size %s"
      ga.ga_rel_name string_of_array_size in

  let string_of_group_members = 
    Core_emit.indent 1 " "
    (List.map (string_of_abs_group depth) ga.ga_groups 
      |> String.concat "\n,") in

  Core_emit.indent depth " "
    (Printf.sprintf "%s {\n%s\n}" 
      string_of_array_header string_of_group_members)

and string_of_abs_group (depth : int) (g : abs_group) : string =
  let row_min, row_max = g.g_abs_row_range in
  let col_min, col_max = g.g_abs_col_range in
  Printf.sprintf "group_path: %s\ngroup_range: (%d:%d, %d:%d)\n\t{%s\n\t}"
  (string_of_path g.g_path)
  row_min row_max col_min col_max 
  (List.map (string_of_abs_group_array (depth + 1))g.g_subgroups |> String.concat ",\n\t")

(* Alternative entry point for printing *)
and print_group (g : abs_group) : string =
  let row_min, row_max = g.g_abs_row_range in
  let col_min, col_max = g.g_abs_col_range in
  Printf.sprintf "group_name: %s\ngroup_range: (%d:%d, %d:%d)\n\t{%s\n\t}"
  (string_of_path g.g_path)
  row_min row_max col_min col_max 
  (List.map (string_of_abs_group_array 0) g.g_subgroups |> String.concat ",\n\t")

let print_abs_arrangement (a : abs_arrangement) : unit =
  List.map (string_of_abs_group_array 0) a.abs_arr_group_arrays
  |> String.concat ",\n"
  |> print_endline

let print_config_section (prog : Ast.program) : unit =
  abs_arrangement_from_prog prog |> print_abs_arrangement
