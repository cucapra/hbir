(* Compiles F1 Device Program *)
let emit (prog : Ast.program) (filename : string) : string =

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

  let fun_def_emit (fun_name : string) (prog : Ast.program) (cb : Ast.code_block_decl) : string =
    let ds = prog.data_section in

    let ret_type =
      let output_arrays = 
        List.filter (fun d -> d.Ast.data_dir == Ast.Out) ds.Ast.ds_data_decls in

      if List.length output_arrays == 0
        then failwith Error.no_output_array
        else Core_emit.type_emit (List.hd output_arrays).Ast.data_type in

    let params = 
      (ds.Ast.ds_data_decls
       |> List.map 
            (fun d -> 
              ("*" ^ d.Ast.data_name, 
              (Core_emit.type_emit d.Ast.data_type) ^ "32_t"))
      ) in

    let body = 
      let ctxt = Core_emit.build_stmt_ctxt prog in
      let code_body = Core_emit.stmt_emit ctxt cb.Ast.cb_code in
      let barrier_completion = Core_emit.unindent
        "// Barrier to signal completion.
        bsg_tile_group_barrier(&r_barrier, &c_barrier);
        return 0;" in

      [code_body; barrier_completion] |> String.concat "\n" in

    Core_emit.fun_emit ret_type fun_name params body in

  let main_def : string = 
    let body = Core_emit.unindent
      "__wait_until_valid_func();
      return 0;" in

    Core_emit.fun_emit "int" "main" [] body in


  [device_header_emit;
   constants_emit prog.Ast.data_section prog.Ast.target_section;
   extern_fun_decls prog.Ast.code_section.cs_extern_fun_decls;
   List.map (fun cb -> fun_def_emit filename prog cb) prog.Ast.code_section.cs_code_block_decls
      |> String.concat "\n\n";
   main_def]
   |> String.concat "\n\n"
