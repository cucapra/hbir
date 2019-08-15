type abs_arrangement = {
  abs_arr_grid_size : int * int;
  abs_arr_groups : group_array list
}

and group_array = {
  ga_dims : int;
  ga_indexed_groups : (ga_index * abs_group) list
}

and abs_group = {
  g_rel_name : string; (* relative name *)
  g_abs_row_range : int * int; (* absolute range *)
  g_abs_col_range : int * int; (* absolute range *)
  g_subgroups : group_array list
}

and ga_index = int list 

let rec cross_bounds (bounds : int list) : ga_index list =
  let rec range (n : int) : ga_index list = 
    if n <= 0 then []
    else [n-1]::(range (n-1)) in
  let rec grow_ixs (i : int) (ixs : ga_index list)  = 
    List.map (fun ix -> ix@[i]) ixs in
  match bounds with
  | [] -> []
  | b::[] -> range b
  | _::0::[] -> []
  | b1::b2::[] -> 
    (grow_ixs (b2-1) (range b1)) @ cross_bounds (b1::[b2-1])
  | _ -> failwith "unimplemented cross product"


let arrange_decl_to_abs_arrangement (arr_decl : Ast.arrange_decl) 
                                    (tile_grid_size : int * int) : abs_arrangement =

  let rec group_decl_to_group_array (par_ix_ctxt : Utils.ctxt)
                                    (par_size_ctxt : Utils.ctxt)
                                    (g_decl : Ast.group_decl) : group_array =
    (* generate all indices from iterators *)
    let indices : Utils.ctxt list = 
      let eval_ctxt : Utils.ctxt = par_ix_ctxt@par_size_ctxt in
      let ix_names, ix_bound_exprs = List.split g_decl.Ast.gd_dim_iters in
      let ix_bounds : int list =
        List.map (Utils.eval_expr eval_ctxt) ix_bound_exprs 
        |> List.map Utils.int_of_value in
      let ixs : ga_index list = cross_bounds ix_bounds in
      List.map (fun ix -> List.combine ix_names ix) ixs in

    (* one relative group corresponds to many absolute groups (i.e., group array) *)
    { ga_dims = List.length g_decl.Ast.gd_dim_iters;
      ga_indexed_groups = 
        List.map 
        (rel_ix_group_to_abs_ix_group par_ix_ctxt par_size_ctxt g_decl) 
        indices }

  (* calculate indexed absolute group corresponding to relative group at a given index
   * note that relative subgroups are recursively parametrized by all given index *)
 and rel_ix_group_to_abs_ix_group (par_ix_ctxt : Utils.ctxt)
                                  (par_size_ctxt : Utils.ctxt)
                                  (g_decl : Ast.group_decl)
                                  (ix_ctxt : Utils.ctxt) : ga_index * abs_group =
    let row_size_name, row_range = g_decl.Ast.gd_row_range in
    let col_size_name, col_range = g_decl.Ast.gd_col_range in

    let full_ctxt : Utils.ctxt = par_size_ctxt @ par_ix_ctxt @ ix_ctxt in
    let min_row, max_row = Utils.eval_range full_ctxt row_range in
    let min_col, max_col= Utils.eval_range full_ctxt col_range in

    let subgroup_par_size_ctxt = 
      [(row_size_name, (max_row - min_row));
       (col_size_name, (max_col - min_col))] in
    let subgroup_ix_ctxt = par_ix_ctxt @ ix_ctxt in

    (* evaluate nested group_decls only using index bindings; we remove parent size bindings *)
    let subgroups : group_array list =
      List.map
      (group_decl_to_group_array subgroup_ix_ctxt subgroup_par_size_ctxt)
      g_decl.Ast.gd_subgroups in

    let _, ix = List.split ix_ctxt in 
    let rec ix_name (ix : int list) = 
      match ix with
      | [] -> ""
      | i::is -> Printf.sprintf "[%d]" i ^ ix_name is in
    ix,
    { g_rel_name = g_decl.Ast.gd_name ^ ix_name ix;
      g_abs_row_range = (min_row, max_row);
      g_abs_col_range = (min_col, max_col);
      g_subgroups = subgroups; } in


  let tile_size_bindings : Utils.ctxt = 
    let tile_rows_name, tile_cols_name = arr_decl.Ast.arr_size_vars in
    let tile_rows, tile_cols = tile_grid_size in
    [(tile_rows_name, tile_rows); (tile_cols_name, tile_cols)] in

  { abs_arr_grid_size = tile_grid_size;
    abs_arr_groups = 
      List.map (group_decl_to_group_array [] tile_size_bindings) arr_decl.Ast.arr_groups
  }


