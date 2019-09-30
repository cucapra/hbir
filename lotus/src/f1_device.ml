(* Compiles F1 Device Program *)
let emit (filename : string) (prog : Ast.program) : string =

  (* 1. headers *)
  let device_header_emit = 
    ["\"bsg_manycore.h\""; 
     "\"bsg_set_tile_x_y.h\"";
     "\"bsg_cuda_lite_runtime.h\"";
     "<stdint.h>"]
    |> List.map Core_emit.header_emit 
    |> String.concat "\n" in

  (* 2. constants from: 
    * data section 
    * target section (e.g., xmax)
    *)
  let constants_emit (ds : Ast.data_section) 
                     (ts : Ast.target_section) : string =
    let constant_emit (decl : Ast.typ * string * Ast.expr) : string =
      let (_, x, e) = decl in
      Core_emit.expr_macro_emit x (Core_emit.expr_emit e) in

    let data_section_constants : string list = 
      List.map constant_emit ds.Ast.ds_constant_decls in

    let layout_constants : string list =
      let num_tiles : string = 
        ts
        |> List.map (fun td ->
            match td with
            | Ast.TileDecl tile -> [tile]
            | _ -> [])
        |> List.flatten
        |> (fun tile_decls -> 
              if List.length tile_decls != 1 
              then failwith Error.exactly_one_tile_group_required
              else tile_decls)
        |> List.hd 
        |> (fun tile -> Core_emit.dims_product_emit tile.tile_dims) in

      let block_size (d : Ast.data_decl) : string = 
        let num_data_elements : string = 
          Core_emit.dims_product_emit d.Ast.data_dims in

        let parens (s : string) : string = "(" ^ s ^ ")" in

        parens num_data_elements ^ "/" ^ parens num_tiles in

      ds.Ast.ds_data_decls
      |> List.map (fun d ->
          ("block_size_" ^ d.Ast.data_name, block_size d))
      |> (@) [("tile_id", "__bsg_id")]
      |> List.map (fun p -> let (x, v) = p in Core_emit.expr_macro_emit x v) in

    let completion_barrier_setup : string =
      let macros =
        [("BSG_TILE_GROUP_X_DIM", "bsg_tiles_X");
         ("BSG_TILE_GROUP_Y_DIM", "bsg_tiles_Y")]
        |> List.map (fun p -> let (x, s) = p in Core_emit.expr_macro_emit x s)
        |> String.concat "\n" in

      let barrier_init = Core_emit.unindent
        "INIT_TILE_GROUP_BARRIER(r_barrier, c_barrier, 0, bsg_tiles_X - 1, 0,
     bsg_tiles_Y - 1);" in

      [macros;
       Core_emit.header_emit "\"bsg_tile_group_barrier.h\""; (* This header requires macros first *)
       barrier_init]
      |> String.concat "\n" in

    [data_section_constants |> String.concat "\n"; (*@ target_section_constants *)
     layout_constants |> String.concat "\n";
     completion_barrier_setup]
    |> String.concat "\n\n" in

  let extern_fun_decls (fs : Ast.fun_decl list) : string = 
    fs
    |> List.map (fun f ->
      let (f_name, ps, ret_typ) = f in
      let ps_emit = 
        List.map (fun p -> let (x, tau) = p in (x, Core_emit.type_emit tau))
        ps in
      Core_emit.fun_decl_emit (Core_emit.type_emit ret_typ) f_name ps_emit)
    |> String.concat "\n" in

  let rec fun_name_of_pattern (gp : Ast.group_pattern) : string = 
    let rec string_of_ix_pattern (ix : Ast.ix_pattern) : string =

      let string_of_ix_elem_pattern (ix_elem : Ast.ix_elem_pattern) : string =
        match ix_elem with
        | Ast.SymIx x -> x
        | Ast.ConcIx n -> string_of_int n in

      match ix with
      | [] -> ""
      | i::[] -> Printf.sprintf "%s" (string_of_ix_elem_pattern i)
      | i::is -> Printf.sprintf "%s_%s" (string_of_ix_elem_pattern i) (string_of_ix_pattern is) in

    match gp with
    | [] -> ""
    | (g_name, [])::[] -> g_name
    | (g_name, [])::p -> 
        Printf.sprintf "%s_%s" g_name (fun_name_of_pattern p)
    | (g_name, sym_ix)::[] ->
        Printf.sprintf "%s_%s" g_name (string_of_ix_pattern sym_ix)
    | (g_name, sym_ix)::p -> 
        Printf.sprintf "%s_%s_%s" g_name (string_of_ix_pattern sym_ix) (fun_name_of_pattern p) in

  let fun_def_emit (prog : Ast.program) (cb : Ast.code_block_decl) : string =
    let ds = prog.data_section in

    let ret_type =
      let output_arrays = 
        List.filter (fun d -> d.Ast.data_dir == Ast.Out) ds.Ast.ds_data_decls in

      if List.length output_arrays == 0
        then "void" (*failwith Error.no_output_array*)
        else Core_emit.type_emit (List.hd output_arrays).Ast.data_type in

    let par_list : (string * string) list = 
      prog.data_section.Ast.ds_data_decls
      |> List.map 
            (fun d -> 
              let typ = Core_emit.type_emit d.Ast.data_type in
              (d.Ast.data_name, typ)) in

    let body = 
      let ctxt = Core_emit.build_stmt_ctxt prog in
      let code_body = Core_emit.stmt_emit ctxt cb.Ast.cb_code in
      let barrier_completion = Core_emit.unindent
        "// Barrier to signal completion.
        bsg_tile_group_barrier(&r_barrier, &c_barrier);
        return 0;" in

      [code_body; barrier_completion] |> String.concat "\n" in

    let fun_name : string = fun_name_of_pattern cb.cb_group_name in
    Core_emit.fun_emit ret_type fun_name par_list body in


  let main_def (main_name : string) (prog : Ast.program) : string =
    
    let tile_id_x_var, tile_id_y_var = "bsg_x", "bsg_y" in

    let range_check_emit (var : string) (abs_range : int * int) : string =
      let (lb, ub)  = abs_range in
      Printf.sprintf "%s <= %s && %s <= %s" 
      (string_of_int lb) var var (string_of_int ub) in

    let arg_list : string = 
      prog.data_section.Ast.ds_data_decls
      |> List.map (fun d -> d.Ast.data_name)
      |> String.concat ", " in

    let code_block_emit (cb : Ast.code_block_decl) : string =
      let matching_abs_groups : Group.abs_group list = 
        Group.match_in_program prog cb.cb_group_name in

      let abs_group_fun_call_emit (g : Group.abs_group) : string =
        let if_cond : string =
          Printf.sprintf "%s && %s"
          (range_check_emit tile_id_x_var g.g_abs_row_range)
          (range_check_emit tile_id_y_var g.g_abs_col_range) in

        let then_body : string = 
          Printf.sprintf "%s(%s);" 
          (fun_name_of_pattern cb.cb_group_name) 
          arg_list in
          
        Printf.sprintf "if(%s)\n{\n%s\n}" if_cond (Core_emit.indent 1 then_body) in

      print_endline (let s, _ = cb.cb_group_name |> List.hd in s);
      print_endline ("matching groups: " ^ string_of_int (List.length matching_abs_groups));
      List.map abs_group_fun_call_emit matching_abs_groups 
      |> String.concat "\n" in
          

    let main_body = 
      let code_blocks = 
        List.map code_block_emit prog.code_section.cs_code_block_decls
        |> String.concat "\n" in
      let body_end = Core_emit.unindent
        "__wait_until_valid_func();
        return 0;" in
       code_blocks ^ "\n" ^ body_end in

    Core_emit.fun_emit "int" main_name [] main_body in


  [device_header_emit;
   constants_emit prog.Ast.data_section prog.Ast.target_section;
   extern_fun_decls prog.Ast.code_section.cs_extern_fun_decls;
   List.map (fun cb -> fun_def_emit prog cb) prog.Ast.code_section.cs_code_block_decls
      |> String.concat "\n\n";
   main_def filename prog]
   |> String.concat "\n\n"
