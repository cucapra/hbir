type abs_arrangement = {
  abs_arr_grid_size : int * int;
  abs_arr_group_arrays : group_array list }

and group_array = {
  ga_rel_name : string;
  ga_dim_bounds : int list;
  ga_indexed_groups : indexed_group list }

and abs_group = {
  (* relative name; absolute name computable with index *)
  g_rel_name : string; 

  (* closed intervals for absolute ranges *)
  g_abs_row_range : int * int; 
  g_abs_col_range : int * int; 

  g_subgroups : group_array list }

and ga_index = int list 
and indexed_group = ga_index * abs_group

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

  let rec group_decl_to_group_array (par_ix_ctxt : Utils.ctxt)
                                    (par_size_ctxt : Utils.ctxt)
                                    (par_origin : int * int)
                                    (g_decl : Ast.group_decl) : group_array =
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
      ga_indexed_groups = 
        List.map 
        (rel_ix_group_to_abs_ix_group par_ix_ctxt par_size_ctxt g_decl par_origin) 
        indices }

  (* calculate indexed absolute group corresponding to relative group at a given index
   * note that relative subgroups are recursively parametrized by all given index *)
 and rel_ix_group_to_abs_ix_group (par_ix_ctxt : Utils.ctxt)
                                  (par_size_ctxt : Utils.ctxt)
                                  (g_decl : Ast.group_decl)
                                  (abs_offset : int * int)
                                  (ix_ctxt : Utils.ctxt) : indexed_group =
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
    let subgroups : group_array list =
      List.map
      (group_decl_to_group_array 
       subgroup_ix_ctxt 
       subgroup_par_size_ctxt 
       (min_row + abs_row_offset, min_col + abs_col_offset))
      g_decl.Ast.gd_subgroups in

    let _, ix = List.split ix_ctxt in 
    ix,
    { g_rel_name = g_decl.Ast.gd_name;
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
      List.map (group_decl_to_group_array [] tile_size_bindings (0,0)) arr_decl.Ast.arr_groups }


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
type group_pattern = (string * ix_pattern) list
and ix_elem_pattern = SymIx of string | ConcIx of int
and ix_pattern = ix_elem_pattern list

let match_in_arrangement (pattern : group_pattern) 
                         (arr : abs_arrangement)
                         : abs_group list =

  let rec match_in_group_array (pattern : group_pattern)
                               (ga : group_array) : abs_group list =

    let rec match_sym_ix (sym_ix : ix_pattern) 
                         (ix : ga_index) : bool =
      match (sym_ix, ix) with
      | ([], _::_) -> false
      | (_::_, []) -> false
      | ([], []) -> true
      | (sym_i::sym_ixs, i::ixs) ->
        match sym_i with
        | SymIx _ -> true
        | ConcIx n -> i == n
        && match_sym_ix sym_ixs ixs in

    match pattern with
    | [] -> 
        List.map (fun ix_g -> let _, g = ix_g in g) ga.ga_indexed_groups 
    | (name, sym_ix)::remaining_pattern ->
      ga.ga_indexed_groups
      |> List.filter
          (fun ix_g -> let ix, g = ix_g in
            (*
            begin if (String.trim g.g_rel_name) = (String.trim name) 
            then Printf.printf 
                 "SUCCESS match on group %s: %s == %s\n" 
                 (g.g_rel_name ^ "[" ^ ga_index_to_string ix ^ "]")
                 g.g_rel_name name
            else Printf.printf 
                 "FAILED match on group %s: %s != %s\n" 
                 (g.g_rel_name ^ "[" ^ ga_index_to_string ix ^ "]")
                 g.g_rel_name name
            end;
            *)
            g.g_rel_name = name && match_sym_ix sym_ix ix)
      |> List.map 
          (fun ix_g -> let _, g = ix_g in 
            match_in_group remaining_pattern g)
      |> List.concat

  and match_in_group (pattern : group_pattern)
                     (g: abs_group) : abs_group list =

     match g.g_subgroups with
     | [] -> [g]
     | _ -> 
       List.map (match_in_group_array pattern) g.g_subgroups
       |> List.concat in

  List.map (match_in_group_array pattern) arr.abs_arr_group_arrays |> List.concat


(* group printing on command line *)
let rec print_abs_group_array (par_name : string) (ga : group_array) : string =
  Printf.sprintf "group_array %s of size %s:\nmembers: {\n%s\n}" 
  par_name
  (List.map (string_of_int) ga.ga_dim_bounds |> String.concat " * ")
  (List.map (print_ix_group (par_name ^ "." ^ ga.ga_rel_name)) ga.ga_indexed_groups 
  |> String.concat ",\n\t")

and print_ix_group (par_name : string) 
                   (ix_group : indexed_group) : string =
  let index, g = ix_group in
  let row_min, row_max = g.g_abs_row_range in
  let col_min, col_max = g.g_abs_col_range in
  let ix_group_name = 
    if par_name = "" then "" 
    else par_name ^ "." ^ g.g_rel_name in
  Printf.sprintf "ix_group_name: %s%s\ngroup_range: (%d:%d, %d:%d)\n\t{%s\n\t}"
  ix_group_name
  (g.g_rel_name ^ ga_index_to_string index)
  row_min row_max col_min col_max 
  (List.map (print_abs_group_array ix_group_name) g.g_subgroups |> String.concat ",\n\t")

(* Alternative entry point for printing *)
and print_group (g : abs_group) : string =
  let row_min, row_max = g.g_abs_row_range in
  let col_min, col_max = g.g_abs_col_range in
  Printf.sprintf "group_name: %s\ngroup_range: (%d:%d, %d:%d)\n\t{%s\n\t}"
  g.g_rel_name
  row_min row_max col_min col_max 
  (List.map (print_abs_group_array g.g_rel_name) g.g_subgroups |> String.concat ",\n\t")

let print_abs_arrangement (a : abs_arrangement) : unit =
  List.map (print_abs_group_array "t") a.abs_arr_group_arrays
  |> String.concat ",\n"
  |> print_endline

let print_config_section (prog : Ast.program) : unit =
  abs_arrangement_from_prog prog |> print_abs_arrangement
